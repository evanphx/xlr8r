#include "ruby.h"
#include "node.h"
#include "env.h"
#include "util.h"
#include "rubysig.h"
#include "internals.h"

#include <dlfcn.h>

void scope_dup(struct SCOPE* scope) {
  ID *tbl;
  VALUE *vars;

  scope->flags |= SCOPE_DONT_RECYCLE;
  if (scope->flags & SCOPE_MALLOC) return;

  if (scope->local_tbl) {
    tbl = scope->local_tbl;
    vars = ALLOC_N(VALUE, tbl[0]+1);
    *vars++ = scope->local_vars[-1];
    MEMCPY(vars, scope->local_vars, VALUE, tbl[0]);
    scope->local_vars = vars;
    scope->flags |= SCOPE_MALLOC;
  }
}

struct iter **int_ruby_iter;
unsigned long *int_frame_unique;
struct tag **int_prot_tag;
struct SCOPE **int_top_scope;
rb_event_hook_t **int_event_hooks;
int *int_scope_vmode;

struct BLOCK **int_ruby_block;
unsigned long *int_block_unique;

void* mri_blk_free;

VALUE (*mri_eval)(VALUE self, NODE* n);
void (*mri_assign)(VALUE self, NODE* lhs, VALUE val, int pcall);
VALUE (*mri_call)(VALUE klass, VALUE recv, ID mid,
                  int argc, const VALUE* argv, int scope, VALUE self);

VALUE (*mri_ev_const_get)(NODE* cref, ID id, VALUE self);

static inline VALUE
call_cfunc(func, recv, len, argc, argv)
    VALUE (*func)();
    VALUE recv;
    int len, argc;
    VALUE *argv;
{
  if (len >= 0 && argc != len) {
    rb_raise(rb_eArgError, "wrong number of arguments (%d for %d)",
        argc, len);
  }

  switch (len) {
    case -2:
      return (*func)(recv, rb_ary_new4(argc, argv));
      break;
    case -1:
      return (*func)(argc, argv, recv);
      break;
    case 0:
      return (*func)(recv);
      break;
    case 1:
      return (*func)(recv, argv[0]);
      break;
    case 2:
      return (*func)(recv, argv[0], argv[1]);
      break;
    case 3:
      return (*func)(recv, argv[0], argv[1], argv[2]);
      break;
    case 4:
      return (*func)(recv, argv[0], argv[1], argv[2], argv[3]);
      break;
    case 5:
      return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4]);
      break;
    case 6:
      return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
          argv[5]);
      break;
    case 7:
      return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
          argv[5], argv[6]);
      break;
    case 8:
      return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
          argv[5], argv[6], argv[7]);
      break;
    case 9:
      return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
          argv[5], argv[6], argv[7], argv[8]);
      break;
    case 10:
      return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
          argv[5], argv[6], argv[7], argv[8], argv[9]);
      break;
    case 11:
      return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
          argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]);
      break;
    case 12:
      return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
          argv[5], argv[6], argv[7], argv[8], argv[9],
          argv[10], argv[11]);
      break;
    case 13:
      return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
          argv[5], argv[6], argv[7], argv[8], argv[9], argv[10],
          argv[11], argv[12]);
      break;
    case 14:
      return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
          argv[5], argv[6], argv[7], argv[8], argv[9], argv[10],
          argv[11], argv[12], argv[13]);
      break;
    case 15:
      return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
          argv[5], argv[6], argv[7], argv[8], argv[9], argv[10],
          argv[11], argv[12], argv[13], argv[14]);
      break;
    default:
      rb_raise(rb_eArgError, "too many arguments (%d)", len);
      break;
  }
  return Qnil;		/* not reached */
}

VALUE spec_no_args(VALUE klass, VALUE recv, ID id, ID oid,
                          int argc, VALUE* argv, NODE* volatile body, int flags)
{
  int state;
  VALUE *local_vars;	/* OK */
  NODE *saved_cref = 0;
  NODE* b2;
  volatile int safe = -1;
  VALUE result;

  PUSH_SCOPE();

  if (body->nd_rval) {
    saved_cref = ruby_cref;
    ruby_cref = (NODE*)body->nd_rval;
  }

  PUSH_CLASS(ruby_cbase);

  if (body->nd_tbl) {
    local_vars = TMP_ALLOC(body->nd_tbl[0]+1);
    *local_vars++ = (VALUE)body;
    rb_mem_clear(local_vars, body->nd_tbl[0]);
    ruby_scope->local_tbl = body->nd_tbl;
    ruby_scope->local_vars = local_vars;
  } else {
    local_vars = ruby_scope->local_vars = 0;
    ruby_scope->local_tbl  = 0;
  }

  b2 = body = body->nd_next;

  if (NOEX_SAFE(flags) > ruby_safe_level) {
    safe = ruby_safe_level;
    ruby_safe_level = NOEX_SAFE(flags);
  }

  PUSH_VARS();
  PUSH_TAG(PROT_FUNC);

  if ((state = EXEC_TAG()) == 0) {
    if (argc > 0) {
      rb_raise(rb_eArgError, "wrong number of arguments (%d for %d)",
          argc, 0);
    }

    ruby_frame->argc = 0;

    body = body->nd_next;

    if (event_hooks) {
      EXEC_EVENT_HOOK(RUBY_EVENT_CALL, b2, recv, id, klass);
    }

    result = mri_eval(recv, body);
  } else if (state == TAG_RETURN && TAG_DST()) {
    result = prot_tag->retval;
    state = 0;
  }

  POP_TAG();

  if (event_hooks) {
    EXEC_EVENT_HOOK(RUBY_EVENT_RETURN, ruby_current_node,
        recv, id, klass);
  }

  POP_VARS();
  POP_CLASS();
  POP_SCOPE();

  ruby_cref = saved_cref;

  if (safe >= 0) ruby_safe_level = safe;

  switch (state) {
    case 0:
      break;

    case TAG_BREAK:
    case TAG_RETURN:
      JUMP_TAG(state);
      break;

    case TAG_RETRY:
      if (rb_block_given_p()) JUMP_TAG(state);
      /* fall through */
    default:
      jump_tag_but_local_jump(state, result);
      break;
  }

  return result;
}

typedef VALUE (*caller)(VALUE klass, VALUE recv, ID id, ID oid,
                 int argc, VALUE* argv, NODE* volatile body, int flags);

struct custom_call {
  void* data;
  caller func;
};

static struct custom_call spec_no_args_obj = { 0, spec_no_args };

const char* node_name(NODE* n) {
  if(!n) return "<NULL>";

  switch(nd_type(n)) {
    case NODE_METHOD: return "NODE_METHOD";
    case NODE_FBODY: return "NODE_FBODY";
    case NODE_CFUNC: return "NODE_CFUNC";
    case NODE_SCOPE: return "NODE_SCOPE";
    case NODE_BLOCK: return "NODE_BLOCK";
    case NODE_IF: return "NODE_IF";
    case NODE_CASE: return "NODE_CASE";
    case NODE_WHEN: return "NODE_WHEN";
    case NODE_OPT_N: return "NODE_OPT_N";
    case NODE_WHILE: return "NODE_WHILE";
    case NODE_UNTIL: return "NODE_UNTIL";
    case NODE_ITER: return "NODE_ITER";
    case NODE_FOR: return "NODE_FOR";
    case NODE_BREAK: return "NODE_BREAK";
    case NODE_NEXT: return "NODE_NEXT";
    case NODE_REDO: return "NODE_REDO";
    case NODE_RETRY: return "NODE_RETRY";
    case NODE_BEGIN: return "NODE_BEGIN";
    case NODE_RESCUE: return "NODE_RESCUE";
    case NODE_RESBODY: return "NODE_RESBODY";
    case NODE_ENSURE: return "NODE_ENSURE";
    case NODE_AND: return "NODE_AND";
    case NODE_OR: return "NODE_OR";
    case NODE_NOT: return "NODE_NOT";
    case NODE_MASGN: return "NODE_MASGN";
    case NODE_LASGN: return "NODE_LASGN";
    case NODE_DASGN: return "NODE_DASGN";
    case NODE_DASGN_CURR: return "NODE_DASGN_CURR";
    case NODE_GASGN: return "NODE_GASGN";
    case NODE_IASGN: return "NODE_IASGN";
    case NODE_CDECL: return "NODE_CDECL";
    case NODE_CVASGN: return "NODE_CVASGN";
    case NODE_CVDECL: return "NODE_CVDECL";
    case NODE_OP_ASGN1: return "NODE_OP_ASGN1";
    case NODE_OP_ASGN2: return "NODE_OP_ASGN2";
    case NODE_OP_ASGN_AND: return "NODE_OP_ASGN_AND";
    case NODE_OP_ASGN_OR: return "NODE_OP_ASGN_OR";
    case NODE_CALL: return "NODE_CALL";
    case NODE_FCALL: return "NODE_FCALL";
    case NODE_VCALL: return "NODE_VCALL";
    case NODE_SUPER: return "NODE_SUPER";
    case NODE_ZSUPER: return "NODE_ZSUPER";
    case NODE_ARRAY: return "NODE_ARRAY";
    case NODE_ZARRAY: return "NODE_ZARRAY";
    case NODE_HASH: return "NODE_HASH";
    case NODE_RETURN: return "NODE_RETURN";
    case NODE_YIELD: return "NODE_YIELD";
    case NODE_LVAR: return "NODE_LVAR";
    case NODE_DVAR: return "NODE_DVAR";
    case NODE_GVAR: return "NODE_GVAR";
    case NODE_IVAR: return "NODE_IVAR";
    case NODE_CONST: return "NODE_CONST";
    case NODE_CVAR: return "NODE_CVAR";
    case NODE_NTH_REF: return "NODE_NTH_REF";
    case NODE_BACK_REF: return "NODE_BACK_REF";
    case NODE_MATCH: return "NODE_MATCH";
    case NODE_MATCH2: return "NODE_MATCH2";
    case NODE_MATCH3: return "NODE_MATCH3";
    case NODE_LIT: return "NODE_LIT";
    case NODE_STR: return "NODE_STR";
    case NODE_DSTR: return "NODE_DSTR";
    case NODE_XSTR: return "NODE_XSTR";
    case NODE_DXSTR: return "NODE_DXSTR";
    case NODE_EVSTR: return "NODE_EVSTR";
    case NODE_DREGX: return "NODE_DREGX";
    case NODE_DREGX_ONCE: return "NODE_DREGX_ONCE";
    case NODE_ARGS: return "NODE_ARGS";
    case NODE_ARGSCAT: return "NODE_ARGSCAT";
    case NODE_ARGSPUSH: return "NODE_ARGSPUSH";
    case NODE_SPLAT: return "NODE_SPLAT";
    case NODE_TO_ARY: return "NODE_TO_ARY";
    case NODE_SVALUE: return "NODE_SVALUE";
    case NODE_BLOCK_ARG: return "NODE_BLOCK_ARG";
    case NODE_BLOCK_PASS: return "NODE_BLOCK_PASS";
    case NODE_DEFN: return "NODE_DEFN";
    case NODE_DEFS: return "NODE_DEFS";
    case NODE_ALIAS: return "NODE_ALIAS";
    case NODE_VALIAS: return "NODE_VALIAS";
    case NODE_UNDEF: return "NODE_UNDEF";
    case NODE_CLASS: return "NODE_CLASS";
    case NODE_MODULE: return "NODE_MODULE";
    case NODE_SCLASS: return "NODE_SCLASS";
    case NODE_COLON2: return "NODE_COLON2";
    case NODE_COLON3: return "NODE_COLON3";
    case NODE_CREF: return "NODE_CREF";
    case NODE_DOT2: return "NODE_DOT2";
    case NODE_DOT3: return "NODE_DOT3";
    case NODE_FLIP2: return "NODE_FLIP2";
    case NODE_FLIP3: return "NODE_FLIP3";
    case NODE_ATTRSET: return "NODE_ATTRSET";
    case NODE_SELF: return "NODE_SELF";
    case NODE_NIL: return "NODE_NIL";
    case NODE_TRUE: return "NODE_TRUE";
    case NODE_FALSE: return "NODE_FALSE";
    case NODE_DEFINED: return "NODE_DEFINED";
    case NODE_NEWLINE: return "NODE_NEWLINE";
    case NODE_POSTEXE: return "NODE_POSTEXE";
    case NODE_ALLOCA: return "NODE_ALLOCA";
    case NODE_DMETHOD: return "NODE_DMETHOD";
    case NODE_BMETHOD: return "NODE_BMETHOD";
    case NODE_MEMO: return "NODE_MEMO";
    case NODE_IFUNC: return "NODE_IFUNC";
    case NODE_DSYM: return "NODE_DSYM";
    case NODE_ATTRASGN: return "NODE_ATTRASGN";
    case NODE_LAST: return "NODE_LAST";


    default: return "UNKNOWN";
  }
}

VALUE spec_nil(VALUE klass, VALUE recv, ID id, ID oid,
                          int argc, VALUE* argv, NODE* volatile body, int flags)
{
  return Qnil;
}

static struct custom_call spec_nil_obj = { 0, spec_nil };

VALUE spec_true(VALUE klass, VALUE recv, ID id, ID oid,
                          int argc, VALUE* argv, NODE* volatile body, int flags)
{
  return Qtrue;
}

static struct custom_call spec_true_obj = { 0, spec_true };

VALUE spec_false(VALUE klass, VALUE recv, ID id, ID oid,
                          int argc, VALUE* argv, NODE* volatile body, int flags)
{
  return Qfalse;
}

static struct custom_call spec_false_obj = { 0, spec_false };

VALUE spec_self(VALUE klass, VALUE recv, ID id, ID oid,
                          int argc, VALUE* argv, NODE* volatile body, int flags)
{
  return recv;
}

static struct custom_call spec_self_obj = { 0, spec_self };

VALUE spec_lit(VALUE klass, VALUE recv, ID id, ID oid,
                          int argc, VALUE* argv, NODE* volatile body, int flags)
{
  return body->nd_next->nd_next->nd_head->nd_next->nd_lit;
}

static struct custom_call spec_lit_obj = { 0, spec_lit };

static NODE* specialize(NODE* scope) {
  NODE* blk = scope->nd_next;
  NODE* args = 0;

  if(nd_type(blk) != NODE_BLOCK) goto done;

  args = blk->nd_head;

  if(nd_type(args) != NODE_ARGS) goto done;

  if(args->nd_cnt == 0 && !args->nd_rest && !args->nd_opt) {
    if(nd_type(blk->nd_next) == NODE_BLOCK &&
        nd_type(blk->nd_next->nd_head) == NODE_NEWLINE &&
        !blk->nd_next->nd_next) {
      switch(nd_type(blk->nd_next->nd_head->nd_next)) {
      case NODE_NIL:
        blk->flags |= FL_FREEZE;
        blk->u2.value = (VALUE)&spec_nil_obj;
        break;
      case NODE_TRUE:
        blk->flags |= FL_FREEZE;
        blk->u2.value = (VALUE)&spec_true_obj;
        break;
      case NODE_FALSE:
        blk->flags |= FL_FREEZE;
        blk->u2.value = (VALUE)&spec_false_obj;
        break;
      case NODE_SELF:
        blk->flags |= FL_FREEZE;
        blk->u2.value = (VALUE)&spec_self_obj;
        break;
      case NODE_LIT:
        blk->flags |= FL_FREEZE;
        blk->u2.value = (VALUE)&spec_lit_obj;
        break;
      default:
        blk->flags |= FL_FREEZE;
        blk->u2.value = (VALUE)&spec_no_args_obj;
      }
    } else {
      if(!compile_node(blk)) {
        printf("failed compile\n");
      } else {
        printf("compiled!\n");
      }

      blk->flags |= FL_FREEZE;
      blk->u2.value = (VALUE)&spec_no_args_obj;
    }
  } else {
    if(!compile_node(blk)) {
      printf("failed compile\n");
    } else {
      printf("compiled!\n");
    }
  }

done:
  blk->flags |= FL_TAINT;

  return blk;
}

NODE* setup_args(NODE* body, VALUE recv,
                       int argc, VALUE* argv, VALUE* local_vars)
{
  NODE *node = 0;
  int i, nopt = 0;

  if (nd_type(body) == NODE_ARGS) {
    node = body;
    body = 0;
  } else if (nd_type(body) == NODE_BLOCK) {
    node = body->nd_head;
    body = body->nd_next;
  }

  if (node) {
    if (nd_type(node) != NODE_ARGS) {
      rb_bug("no argument-node");
    }

    i = node->nd_cnt;
    if (i > argc) {
      rb_raise(rb_eArgError, "wrong number of arguments (%d for %d)",
          argc, i);
    }
    if (!node->nd_rest) {
      NODE *optnode = node->nd_opt;

      nopt = i;
      while (optnode) {
        nopt++;
        optnode = optnode->nd_next;
      }
      if (nopt < argc) {
        rb_raise(rb_eArgError,
            "wrong number of arguments (%d for %d)",
            argc, nopt);
      }
    }
    if (local_vars) {
      if (i > 0) {
        /* +2 for $_ and $~ */
        MEMCPY(local_vars+2, argv, VALUE, i);
      }
    }
    argv += i; argc -= i;
    if (node->nd_opt) {
      NODE *opt = node->nd_opt;

      while (opt && argc) {
        mri_assign(recv, opt->nd_head, *argv, 1);
        argv++; argc--;
        ++i;
        opt = opt->nd_next;
      }
      if (opt) {
        mri_eval(recv, opt);
        while (opt) {
          opt = opt->nd_next;
          ++i;
        }
      }
    }
    if (!node->nd_rest) {
      i = nopt;
    }
    else {
      VALUE v;

      if (argc > 0) {
        v = rb_ary_new4(argc,argv);
        i = -i - 1;
      }
      else {
        v = rb_ary_new2(0);
      }
      mri_assign(recv, node->nd_rest, v, 1);
    }
    ruby_frame->argc = i;
  }

  return body;
}

static VALUE generic_call(VALUE klass, VALUE recv, ID id, ID oid,
                 int argc, VALUE* argv, NODE* volatile body, int flags)
{
  volatile VALUE result = Qnil;
  int itr;
  volatile int safe = -1;
  NODE *b2;
  TMP_PROTECT;

  int state;
  VALUE *local_vars;	/* OK */
  NODE *saved_cref = 0;

  PUSH_SCOPE();

  if (body->nd_rval) {
    saved_cref = ruby_cref;
    ruby_cref = (NODE*)body->nd_rval;
  }

  PUSH_CLASS(ruby_cbase);

  if (body->nd_tbl) {
    local_vars = TMP_ALLOC(body->nd_tbl[0]+1);
    *local_vars++ = (VALUE)body;
    rb_mem_clear(local_vars, body->nd_tbl[0]);
    ruby_scope->local_tbl = body->nd_tbl;
    ruby_scope->local_vars = local_vars;
  } else {
    local_vars = ruby_scope->local_vars = 0;
    ruby_scope->local_tbl  = 0;
  }

  if(body->nd_next->flags & FL_TAINT) {
    b2 = body = body->nd_next;
  } else {
    b2 = body = specialize(body);
  }

  if (NOEX_SAFE(flags) > ruby_safe_level) {
    safe = ruby_safe_level;
    ruby_safe_level = NOEX_SAFE(flags);
  }

  PUSH_VARS();
  PUSH_TAG(PROT_FUNC);

  if ((state = EXEC_TAG()) == 0) {
    body = setup_args(body, recv, argc, argv, local_vars);

    if (event_hooks) {
      EXEC_EVENT_HOOK(RUBY_EVENT_CALL, b2, recv, id, klass);
    }

    result = mri_eval(recv, body);
  } else if (state == TAG_RETURN && TAG_DST()) {
    result = prot_tag->retval;
    state = 0;
  }

  POP_TAG();

  if (event_hooks) {
    EXEC_EVENT_HOOK(RUBY_EVENT_RETURN, ruby_current_node,
        recv, id, klass);
  }

  POP_VARS();
  POP_CLASS();
  POP_SCOPE();

  ruby_cref = saved_cref;

  if (safe >= 0) ruby_safe_level = safe;

  switch (state) {
    case 0:
      break;

    case TAG_BREAK:
    case TAG_RETURN:
      JUMP_TAG(state);
      break;

    case TAG_RETRY:
      if (rb_block_given_p()) JUMP_TAG(state);
      /* fall through */
    default:
      jump_tag_but_local_jump(state, result);
      break;
  }

  return result;
}

VALUE xlr8r_call0(VALUE klass, VALUE recv, ID id, ID oid,
                 int argc, VALUE* argv, NODE* volatile body, int flags)
{
  volatile VALUE result = Qnil;
  int itr;
  volatile int safe = -1;
  NODE *b2;
  TMP_PROTECT;

  if (NOEX_SAFE(flags) > ruby_safe_level && NOEX_SAFE(flags) > 2) {
    rb_raise(rb_eSecurityError, "calling insecure method: %s",
        rb_id2name(id));
  }

  switch (ruby_iter->iter) {
    case ITER_PRE:
    case ITER_PAS:
      itr = ITER_CUR;
      break;
    case ITER_CUR:
    default:
      itr = ITER_NOT;
      break;
  }

  // eval_check_tick();
  if (argc < 0) {
    VALUE tmp;
    VALUE *nargv;

    argc = -argc-1;
    tmp = splat_value(argv[argc]);
    nargv = TMP_ALLOC(argc + RARRAY(tmp)->len);
    MEMCPY(nargv, argv, VALUE, argc);
    MEMCPY(nargv+argc, RARRAY(tmp)->ptr, VALUE, RARRAY(tmp)->len);
    argc += RARRAY(tmp)->len;
    argv = nargv;
  }

  PUSH_ITER(itr);
  PUSH_FRAME();

  ruby_frame->last_func = id;
  ruby_frame->orig_func = oid;
  ruby_frame->last_class = (flags & NOEX_NOSUPER)?0:klass;
  ruby_frame->self = recv;
  ruby_frame->argc = argc;
  ruby_frame->flags = 0;

  if(nd_type(body) == NODE_SCOPE &&
      nd_type(body->nd_next) == NODE_BLOCK &&
      body->nd_next->flags & FL_FREEZE)
  {
    struct custom_call* c = (struct custom_call*)body->nd_next->u2.value;
    result = c->func(klass, recv, id, oid, argc, argv, body, flags);
  } else {
    switch (nd_type(body)) {
    case NODE_CFUNC:
      {
        int len = body->nd_argc;

        if (len < -2) {
          rb_bug("bad argc (%d) specified for `%s(%s)'",
              len, rb_class2name(klass), rb_id2name(id));
        }

        if (event_hooks) {
          int state;

          EXEC_EVENT_HOOK(RUBY_EVENT_C_CALL, ruby_current_node,
              recv, id, klass);
          PUSH_TAG(PROT_FUNC);
          if ((state = EXEC_TAG()) == 0) {
            result = call_cfunc(body->nd_cfnc, recv, len, argc, argv);
          }
          POP_TAG();
          ruby_current_node = ruby_frame->node;
          EXEC_EVENT_HOOK(RUBY_EVENT_C_RETURN, ruby_current_node,
              recv, id, klass);
          if (state) JUMP_TAG(state);
        }
        else {
          result = call_cfunc(body->nd_cfnc, recv, len, argc, argv);
        }
      }
      break;

      /* for attr get/set */
    case NODE_IVAR:
      if (argc != 0) {
        rb_raise(rb_eArgError, "wrong number of arguments (%d for 0)", argc);
      }
      result = rb_attr_get(recv, body->nd_vid);
      break;

    case NODE_ATTRSET:
      if (argc != 1)
        rb_raise(rb_eArgError, "wrong number of arguments (%d for 1)", argc);
      result = rb_ivar_set(recv, body->nd_vid, argv[0]);
      break;

    case NODE_ZSUPER:
      result = rb_call_super(argc, argv);
      break;

    case NODE_DMETHOD:
      result = method_call(argc, argv, umethod_bind(body->nd_cval, recv));
      break;

    case NODE_BMETHOD:
      ruby_frame->flags |= FRAME_DMETH;
      if (event_hooks) {
        struct BLOCK *data;
        Data_Get_Struct(body->nd_cval, struct BLOCK, data);
        EXEC_EVENT_HOOK(RUBY_EVENT_CALL, data->body, recv, id, klass);
      }
      result = proc_invoke(body->nd_cval, rb_ary_new4(argc, argv), recv, klass);
      if (event_hooks) {
        EXEC_EVENT_HOOK(RUBY_EVENT_RETURN, ruby_current_node, recv, id, klass);
      }
      break;

    case NODE_SCOPE:
      result = generic_call(klass, recv, id, oid, argc, argv, body, flags);
      break;

    default:
      rb_bug("Unknown node used as method");
      break;
    }
  }

cleanup:
  POP_FRAME();
  POP_ITER();
  return result;
}

#include <stdint.h>

// call0 = 0000000000022f2b
// apply = 0000000000029074
int diff = 0x6149;

// ruby_iter
// ruby_init @ 000000000002bfdf
// 000000000002c028        movq    %rax,0x000b5639(%rip)
// 000000000002c02f        callq   0x0008f83e
// 
// pc = 0x2c02f
// bs = 0x2bfdf
// of = 0xb5639
//
// df = of + (pc - bs) == 0xb5689

#include <sys/mman.h>

#include "config.h"

#define APPLY_OFFSET(base, offset) (void*)(((uintptr_t)base) - offset)

void Init_xlr8r_ext() {
  void* addr = dlsym(RTLD_DEFAULT, "rb_apply");

  void* call0 = APPLY_OFFSET(addr, OFFSET_CALL0);

  int_ruby_iter = APPLY_OFFSET(addr, OFFSET_RUBY_ITER);
  int_frame_unique = APPLY_OFFSET(addr, OFFSET_FRAME_UNIQUE);
  int_prot_tag = APPLY_OFFSET(addr, OFFSET_PROT_TAG);
  int_top_scope = APPLY_OFFSET(addr, OFFSET_TOP_SCOPE);
  int_event_hooks = APPLY_OFFSET(addr, OFFSET_EVENT_HOOKS);
  int_scope_vmode = APPLY_OFFSET(addr, OFFSET_SCOPE_VMODE);

  mri_eval = APPLY_OFFSET(addr, OFFSET_RB_EVAL);
  mri_assign = APPLY_OFFSET(addr, OFFSET_ASSIGN);
  mri_call = APPLY_OFFSET(addr, OFFSET_RB_CALL);


  int_ruby_block = APPLY_OFFSET(addr, OFFSET_RUBY_BLOCK);
  int_block_unique = APPLY_OFFSET(addr, OFFSET_BLOCK_UNIQUE);

  mri_blk_free = APPLY_OFFSET(addr, OFFSET_BLK_FREE);

  uintptr_t dest = ((uintptr_t)xlr8r_call0) - (((uintptr_t)call0) + 6);
  uint8_t* buf = (uint8_t*)call0;

  mprotect((void*)(((uintptr_t)buf) & ~0xfff), 4096,
           PROT_READ | PROT_WRITE | PROT_EXEC);

  buf[0] = 0x66;
  buf[1] = 0xe9;
  buf[2] = ((dest >> 0)  & 0xff);
  buf[3] = ((dest >> 8)  & 0xff);
  buf[4] = ((dest >> 16) & 0xff);
  buf[5] = ((dest >> 24) & 0xff);

  mprotect((void*)(((uintptr_t)buf) & ~0xfff), 4096,
           PROT_READ | PROT_EXEC);

  init_runtime();
}
