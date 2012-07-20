#include <stdint.h>
#include <vector>
#include <list>

extern "C" {

#include "ruby.h"
#include "node.h"
#include "env.h"
#include "util.h"
#include "rubysig.h"
#include "internals.h"

}

extern "C" const char* node_name(NODE* n);

enum Commands {
  kNOOP = 0,
  kLoadLiteral,
  kLoadLiteral32,
  kLoadSelf,
  kLoadTrue,
  kLoadFalse,
  kLoadNil,
  kGotoIfFalse,
  kGoto,
  kCall,
  kPlus,
  kPrivateCall,
  kPrivateCallWithBlock,
  kReturn,
  kConst,
  kCurrentBlock,
  kSetLocal,
  kGetLocal,
  kCallWithBlock,
  kConvertProc,
  kSetArgs,
  kSetArgsFromCat,
  kCreateArray,
  kCreateString,
  kSetCache
};

typedef VALUE (*caller)(VALUE klass, VALUE recv, ID id, ID oid,
                 int argc, VALUE* argv, NODE* volatile body, int flags);

static ID id_plus;

struct custom_call {
  void* data;
  caller func;
};

struct CompiledCode {
  int num_literals;
  VALUE* literals;

  int num_registers;
  uint8_t* bytecode;

  int num_caches;
  InlineCache* caches;

  struct custom_call caller_obj;
};


static bool trace = false;
#define TRACE(str) if(trace) printf("=> " #str "\n");

#define TRACE1(str) if(trace) printf("=> " #str ", %d\n", bc[ip+1]);
#define TRACE2(str) if(trace) printf("=> " #str ", %d, %d\n", bc[ip+1], bc[ip+2]);
#define TRACE3(str) if(trace) printf("=> " #str ", %d, %d, %d\n", bc[ip+1], bc[ip+2], bc[ip+3]);
#define TRACE4(str) if(trace) printf("=> " #str ", %d, %d, %d, %d\n", bc[ip+1], bc[ip+2], bc[ip+3], bc[ip+4]);

InlineCache* last_ic = 0;

extern "C" VALUE (*mri_call)(VALUE klass, VALUE recv, ID mid,
                           int argc, const VALUE* argv, int scope, VALUE self);

class RegisterInterpreter {
  CompiledCode* cc_;

public:

  RegisterInterpreter(CompiledCode* cc)
    : cc_(cc)
  {}

  static VALUE ev_const_get(NODE* cref, ID id, VALUE self) {
    NODE *cbase = cref;
    VALUE result;

    while (cbase && cbase->nd_next) {
      VALUE klass = cbase->nd_clss;

      if (!NIL_P(klass)) {
        while (RCLASS(klass)->iv_tbl &&
            st_lookup(RCLASS(klass)->iv_tbl, id, &result)) {
          if (result == Qundef) {
            if (!RTEST(rb_autoload_load(klass, id))) break;
            continue;
          }
          return result;
        }
      }
      cbase = cbase->nd_next;
    }
    return rb_const_get(NIL_P(cref->nd_clss) ? CLASS_OF(self): cref->nd_clss, id);
  }

#define PROC_TSHIFT (FL_USHIFT+1)
#define PROC_TMASK  (FL_USER1|FL_USER2|FL_USER3)
#define PROC_TMAX   (PROC_TMASK >> PROC_TSHIFT)

  static int proc_get_safe_level(VALUE data) {
      return (RBASIC(data)->flags & PROC_TMASK) >> PROC_TSHIFT;
  }

  static void proc_set_safe_level(VALUE data) {
      ruby_safe_level = proc_get_safe_level(data);
  }

  static int block_orphan(struct BLOCK* data) {
    if (data->scope->flags & SCOPE_NOSTACK) {
      return 1;
    }
    if (data->orig_thread != rb_thread_current()) {
      return 1;
    }
    return 0;
  }

  static void proc_jump_error(int state, VALUE result) {
    char mesg[32];
    const char *statement;

    switch (state) {
      case TAG_BREAK:
        statement = "break"; break;
      case TAG_RETURN:
        statement = "return"; break;
      case TAG_RETRY:
        statement = "retry"; break;
      default:
        statement = "local-jump"; break; /* should not happen */
    }
    snprintf(mesg, sizeof mesg, "%s from proc-closure", statement);
    localjump_error(mesg, result, state);
  }

  static VALUE rb_obj_is_proc(VALUE proc) {
    if (TYPE(proc) == T_DATA && RDATA(proc)->dfree == (RUBY_DATA_FUNC)mri_blk_free) {
      return Qtrue;
    }
    return Qfalse;
  }

  static VALUE splat_value(VALUE v) {
    if (NIL_P(v)) return rb_ary_new3(1, Qnil);
    return rb_Array(v);
  }

  static VALUE call_block(VALUE recv, VALUE proc,
                          InlineCache* ic, ID id,
                          int argc, VALUE* argv,
                          int scope, VALUE self)
  {
    VALUE result;
    VALUE b;

    if(NIL_P(proc)) {
      PUSH_ITER(ITER_NOT);

      VALUE tm2 = CLASS_OF(recv);

      if(ic->klass == tm2) {
        result = xlr8r_call0(ic->origin, recv,
                               id, 
                               ic->id,
                               argc, argv,
                               ic->body, ic->noex);
      } else {
        last_ic = ic;
        result = mri_call(tm2, recv, id, argc, argv, scope, self);
      }

      POP_ITER();
      return result;
    }

    if(!rb_obj_is_proc(proc)) {
      b = rb_check_convert_type(proc, T_DATA, "Proc", "to_proc");
      if (!rb_obj_is_proc(b)) {
        rb_raise(rb_eTypeError, "wrong argument type %s (expected Proc)",
            rb_obj_classname(proc));
      }
      proc = b;
    }

    if(ruby_safe_level >= 1 && OBJ_TAINTED(proc) &&
        ruby_safe_level > proc_get_safe_level(proc)) {
      rb_raise(rb_eSecurityError, "Insecure: tainted block value");
    }

    if(ruby_block && ruby_block->block_obj == proc) {
      PUSH_ITER(ITER_PAS);
      VALUE tm2 = CLASS_OF(recv);

      if(ic->klass == tm2) {
        result = xlr8r_call0(ic->origin, recv,
                               id, 
                               ic->id,
                               argc, argv,
                               ic->body, ic->noex);
      } else {
        last_ic = ic;
        result = mri_call(tm2, recv, id, argc, argv, scope, self);
      }
      POP_ITER();
      return result;
    }

    return call_unwraped_block(recv, proc, ic, id, argc, argv, scope, self);
  }

  static VALUE call_unwraped_block(VALUE recv, VALUE proc,
                                   InlineCache* ic, ID id,
                                   int argc, VALUE* argv,
                                   int scope, VALUE self)
  {
    VALUE b;
    struct BLOCK * volatile old_block;
    struct BLOCK _block;
    struct BLOCK *data;
    volatile VALUE result = Qnil;
    int state;
    volatile int orphan;
    volatile int safe = ruby_safe_level;

    Data_Get_Struct(proc, struct BLOCK, data);
    orphan = block_orphan(data);

    /* PUSH BLOCK from data */
    old_block = ruby_block;
    _block = *data;
    _block.outer = ruby_block;

    if (orphan) _block.uniq = block_unique++;
    ruby_block = &_block;

    PUSH_ITER(ITER_PRE);
    if (ruby_frame->iter == ITER_NOT)
      ruby_frame->iter = ITER_PRE;

    PUSH_TAG(PROT_LOOP);
    state = EXEC_TAG();
    if(state == 0) {
      proc_set_safe_level(proc);

      if(safe > ruby_safe_level) {
        ruby_safe_level = safe;
      }

      VALUE tm2 = CLASS_OF(recv);

      if(ic->klass == tm2) {
        result = xlr8r_call0(ic->origin, recv,
                               id, 
                               ic->id,
                               argc, argv,
                               ic->body, ic->noex);
      } else {
        last_ic = ic;
        result = mri_call(tm2, recv, id, argc, argv, scope, self);
      }

    } else if(state == TAG_BREAK && TAG_DST()) {
      result = prot_tag->retval;
      state = 0;
    } else if(state == TAG_RETRY) {
      state = 0;
      rb_bug("retry in block not supported by xlr8r");
    }

    POP_TAG();
    POP_ITER();
    ruby_block = old_block;
    ruby_safe_level = safe;

    switch (state) { /* escape from orphan block */
      case 0:
        break;
      case TAG_RETURN:
        if (orphan) {
          proc_jump_error(state, prot_tag->retval);
        }
      default:
        JUMP_TAG(state);
    }

    return result;
  }

  static VALUE add_fixnum(VALUE x, VALUE y) {
    long a, b, c;

    a = FIX2LONG(x);
    b = FIX2LONG(y);
    c = a + b;

    return LONG2NUM(c);
  }

  VALUE interpret(VALUE self) {
    uint8_t* bc = cc_->bytecode;
    int ip = 0;
    int dest;
    VALUE tmp, tm2;
    VALUE *vals;

    int next_args_len = 0;
    VALUE* next_args;

    InlineCache* ic;

    VALUE* regs = (VALUE*)alloca(cc_->num_registers * sizeof(VALUE));

    for(;;) {
      switch(bc[ip]) {
      case kLoadLiteral:
        TRACE2(load_literal);

        regs[bc[ip+1]] = cc_->literals[bc[ip+2]];
        ip += 3;
        break;

      case kLoadSelf:
        TRACE1(load_self);

        regs[bc[ip+1]] = self;
        ip += 2;
        break;
      case kLoadNil:
        TRACE1(load_nil);

        regs[bc[ip+1]] = Qnil;
        ip += 2;
        break;
      case kLoadTrue:
        TRACE1(load_true);

        regs[bc[ip+1]] = Qtrue;
        ip += 2;
        break;
      case kLoadFalse:
        TRACE1(load_false);

        regs[bc[ip+1]] = Qfalse;
        ip += 2;
        break;

      case kSetCache:
        TRACE1(set_cache);

        ic = cc_->caches + bc[ip+1];

        ip += 2;
        break;

      case kSetArgs:
        TRACE2(set_args);

        next_args = regs + bc[ip+1];
        next_args_len = bc[ip+2];

        ip += 3;
        break;
      case kSetArgsFromCat:
        TRACE2(set_args_from_cat);

        tmp = rb_ary_concat(regs[bc[ip+1]], splat_value(regs[bc[ip+2]]));
        next_args = RARRAY(tmp)->ptr;
        next_args_len = RARRAY(tmp)->len;

        ip += 3;
        break;

      case kPlus:
        TRACE3(plus);

        tmp = regs[bc[ip+2]];
        tm2 = regs[bc[ip+3]];

        if(FIXNUM_P(tmp) | FIXNUM_P(tm2)) {
          regs[bc[ip+1]] = add_fixnum(tmp, tm2);
        } else {
          tm2 = CLASS_OF(tmp);

          if(ic->klass == tm2) {
            regs[bc[ip+1]] = xlr8r_call0(ic->origin, tmp,
                                   id_plus, 
                                   ic->id,
                                   1, &regs[bc[ip+3]],
                                   ic->body, ic->noex);
          } else {
            last_ic = ic;
            regs[bc[ip+1]] = mri_call(CLASS_OF(tmp), tmp,
                                      id_plus, 1, &regs[bc[ip+3]], 0, self);
          }
        }

        ip += 4;
        break;

      case kCall:
        TRACE3(call);

        tmp = regs[bc[ip+3]];
        tm2 = CLASS_OF(tmp);

        if(ic->klass == tm2) {
          regs[bc[ip+1]] = xlr8r_call0(ic->origin, tmp,
                                 cc_->literals[bc[ip+2]],
                                 ic->id,
                                 next_args_len, next_args,
                                 ic->body, ic->noex);
        } else {
          last_ic = ic;
          regs[bc[ip+1]] = mri_call(tm2, tmp,
                                 cc_->literals[bc[ip+2]],
                                 next_args_len, next_args, 0, self);
        }

        next_args_len = 0;
        ip += 4;
        break;
      case kCallWithBlock:
        TRACE3(call_with_block);

        vals = &regs[bc[ip+3]];

        regs[bc[ip+1]] = call_block(vals[1], vals[0], ic,
                                    cc_->literals[bc[ip+2]],
                                    next_args_len, next_args, 0, self);
        next_args_len = 0;
        ip += 4;
        break;
      case kPrivateCall:
        TRACE2(private_call);

        tm2 = CLASS_OF(self);

        if(ic->klass == tm2) {
          regs[bc[ip+1]] = xlr8r_call0(ic->origin, self,
                                 cc_->literals[bc[ip+2]],
                                 ic->id,
                                 next_args_len, next_args,
                                 ic->body, ic->noex);
        } else {
          last_ic = ic;
          regs[bc[ip+1]] = mri_call(tm2, self,
                                 cc_->literals[bc[ip+2]],
                                 next_args_len, next_args, 1, self);
        }

        next_args_len = 0;
        ip += 3;
        break;
      case kPrivateCallWithBlock:
        TRACE3(private_call_with_block);

        vals = &regs[bc[ip+3]];

        regs[bc[ip+1]] = call_block(self, vals[0], ic,
                                    cc_->literals[bc[ip+2]],
                                    next_args_len, next_args, 1, self);
        next_args_len = 0;
        ip += 4;
        break;
      case kReturn:
        TRACE1(return);
        return regs[bc[ip+1]];
      case kConst:
        TRACE2(const);
        regs[bc[ip+1]] = ev_const_get(ruby_cref,
                                      cc_->literals[bc[ip+2]], self);
        ip += 3;
        break;
      case kCurrentBlock:
        TRACE1(current_block);
        regs[bc[ip+1]] = rb_block_given_p() ? rb_block_proc() : Qnil;
        ip += 2;
        break;
      case kSetLocal:
        TRACE2(set_local);

        ruby_scope->local_vars[bc[ip+2]] = regs[bc[ip+1]];
        ip += 3;
        break;
      case kGetLocal:
        TRACE2(get_local);

        regs[bc[ip+1]] = ruby_scope->local_vars[bc[ip+2]];
        ip += 3;
        break;

      case kCreateArray:
        TRACE3(create_array);

        regs[bc[ip+1]] = rb_ary_new4(bc[ip+3], &regs[bc[ip+2]]);
        ip += 4;
        break;

      case kCreateString:
        TRACE2(create_string);

        regs[bc[ip+1]] = rb_str_new3(cc_->literals[bc[ip+2]]);
        ip += 3;
        break;

      default:
        printf("Bad opcode: %d\n", bc[ip]);
        rb_bug("xlr8r crashed");
      }
    }
  }

  static VALUE caller(VALUE klass, VALUE recv, ID id, ID oid,
                 int argc, VALUE* argv, NODE* volatile body, int flags)
  {
    struct custom_call* c = (struct custom_call*)body->nd_next->u2.value;

    CompiledCode* cc = (CompiledCode*)c->data;

    RegisterInterpreter interp(cc);

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
      body = setup_args(body, recv, argc, argv, local_vars);

      if (event_hooks) {
        EXEC_EVENT_HOOK(RUBY_EVENT_CALL, b2, recv, id, klass);
      }

      result = interp.interpret(recv);
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
};

class Runtime {
  std::list<CompiledCode*> code_;

public:
  void mark() {
    printf("marking!\n");
    for(std::list<CompiledCode*>::iterator i = code_.begin();
        i != code_.end();
        ++i)
    {
      CompiledCode* cc = *i;
      rb_gc_mark_locations(cc->literals, cc->literals + cc->num_literals);
    }
  }
};

static Runtime* g_runtime;

class RegisterCompiler {
  typedef uint8_t bc;

  std::vector<VALUE> literals_;
  int next_reg_;
  int max_reg_;

  bc buf_[1024];
  int ip_;
  int caches_;

public:

  RegisterCompiler()
    : next_reg_(0)
    , ip_(0)
    , caches_(0)
  {}

  int add_literal(VALUE val) {
    int idx = literals_.size();
    literals_.push_back(val);
    return idx;
  }

  int next_reg() {
    int reg = next_reg_++;
    if(reg > max_reg_) max_reg_ = reg;
    return reg;
  }

  int next_cache() {
    return caches_++;
  }

  void reset_reg(int r) {
    next_reg_ = r;
  }

  uint8_t* current_pos() {
    return buf_ + ip_;
  }

  void add_op8(bc cmd) {
    buf_[ip_++] = cmd;
  }

  void add_op32(uint32_t i) {
    *(int*)(buf_ + ip_) = i;
    ip_ += 4;
  }

  CompiledCode* compile_top(NODE* node) {
    int r = next_reg();
    if(!compile(node, r)) return 0;
    add_op8(kReturn);
    add_op8(r);

    CompiledCode* cc = (CompiledCode*)malloc(sizeof(CompiledCode));
    cc->num_literals = literals_.size();
    cc->literals = (VALUE*)calloc(cc->num_literals, sizeof(VALUE));

    int j = 0;
    for(std::vector<VALUE>::iterator i = literals_.begin();
        i != literals_.end();
        ++i) {
      cc->literals[j++] = *i;
    }

    cc->num_registers = max_reg_ + 1;
    cc->bytecode = (bc*)calloc(ip_, sizeof(bc));
    memcpy(cc->bytecode, buf_, ip_);

    cc->caches = (InlineCache*)calloc(caches_, sizeof(InlineCache));
    cc->num_caches = caches_;

    cc->caller_obj.data = cc;
    cc->caller_obj.func = RegisterInterpreter::caller;

    return cc;
  }

  bool compile(NODE* node, int dest) {
    NODE* tmp = 0;
    int idx;

again:
    printf("on %s\n", node_name(node));

    switch(nd_type(node)) {
    case NODE_BLOCK:
      while(node) {
        if(!compile(node->nd_head, dest)) return false;
        node = node->nd_next;
      }
      break;

    case NODE_BEGIN:
      node = node->nd_body;
      goto again;

    case NODE_NEWLINE:
      node = node->nd_next;
      goto again;

    case NODE_LIT:
      idx = add_literal(node->nd_lit);
      if(idx < 255) {
        add_op8(kLoadLiteral);
        add_op8(dest);
        add_op8(idx);
      } else {
        add_op8(kLoadLiteral32);
        add_op8(dest);
        add_op32(idx);
      }
      break;

    case NODE_SELF:
      add_op8(kLoadSelf);
      add_op8(dest);
      break;

    case NODE_NIL:
      add_op8(kLoadNil);
      add_op8(dest);
      break;

    case NODE_TRUE:
      add_op8(kLoadTrue);
      add_op8(dest);
      break;

    case NODE_FALSE:
      add_op8(kLoadFalse);
      add_op8(dest);
      break;

    case NODE_IF:
      {
        return false;

        if(!compile(node->nd_cond, dest)) return false;
        add_op8(kGotoIfFalse);
        uint8_t* fixup = current_pos();
        add_op8(0);

        if(!compile(node->nd_body, dest)) return false;
        add_op8(kGoto);
        uint8_t* fin = current_pos();
        add_op8(0);

        *fixup = ip_;

        if(node->nd_else) {
          if(!compile(node->nd_else, dest)) return false;
        } else {
          add_op8(kLoadNil);
          add_op8(dest);
        }

        *fin = ip_;
      }
      break;

    case NODE_CONST:
      add_op8(kConst);
      add_op8(dest);
      add_op8(add_literal(node->nd_vid));
      break;

    case NODE_CALL:
      {
        int r = next_reg();
        if(!compile(node->nd_recv, r)) return false;

        if(!node->nd_args) {
          add_op8(kSetCache);
          add_op8(next_cache());

          add_op8(kCall);
          add_op8(dest);
          add_op8(add_literal(node->nd_mid));
          add_op8(r);
        } else {
          if(nd_type(node->nd_args) != NODE_ARRAY) return false;

          tmp = node->nd_args;

          if(node->nd_mid == id_plus && tmp->nd_alen == 1) {
            int q = next_reg();

            if(!compile(tmp->nd_head, q)) return false;

            add_op8(kSetCache);
            add_op8(next_cache());

            add_op8(kPlus);
            add_op8(dest);
            add_op8(r);
            add_op8(q);
          } else {
            while(tmp) {
              if(!compile(tmp->nd_head, next_reg())) return false;
              tmp = tmp->nd_next;
            }

            add_op8(kSetCache);
            add_op8(next_cache());

            add_op8(kSetArgs);
            add_op8(r+1);
            add_op8(node->nd_args->nd_alen);

            add_op8(kCall);
            add_op8(dest);
            add_op8(add_literal(node->nd_mid));
            add_op8(r);
          }
        }

        reset_reg(r);
      }

      break;

    case NODE_FCALL:
      if(!node->nd_args) {
        add_op8(kPrivateCall);
        add_op8(dest);
        add_op8(add_literal(node->nd_mid));
      } else {
        int r = next_reg();
        if(nd_type(node->nd_args) != NODE_ARRAY) return false;

        tmp = node->nd_args;

        if(!compile(tmp->nd_head, r)) return false;
        tmp = tmp->nd_next;

        while(tmp) {
          if(!compile(tmp->nd_head, next_reg())) return false;
          tmp = tmp->nd_next;
        }

        add_op8(kSetArgs);
        add_op8(r);
        add_op8(node->nd_args->nd_alen);

        add_op8(kPrivateCall);
        add_op8(dest);
        add_op8(add_literal(node->nd_mid));

        reset_reg(r);
      }

      break;

    case NODE_BLOCK_ARG:
      add_op8(kCurrentBlock);
      add_op8(dest);
      add_op8(kSetLocal);
      add_op8(dest);
      add_op8(node->nd_cnt);
      break;

    case NODE_ARRAY:
      {
        int start = -1;

        tmp = node;
        while(tmp) {
          int r = next_reg();
          if(start < 0) start = r;

          if(!compile(tmp->nd_head, r)) return false;
          tmp = tmp->nd_next;
        }

        add_op8(kCreateArray);
        add_op8(dest);
        add_op8(start);
        add_op8(node->nd_alen);

        reset_reg(start);
      }
      break;

    case NODE_BLOCK_PASS:
      {
        int r = next_reg();
        if(!compile(node->nd_body, r)) return false;

        node = node->nd_iter;

        switch(nd_type(node)) {
        case NODE_FCALL:
          tmp = node->nd_args;

          if(nd_type(tmp) == NODE_ARRAY) {
            while(tmp) {
              if(!compile(tmp->nd_head, next_reg())) return false;
              tmp = tmp->nd_next;
            }
            add_op8(kSetArgs);
            add_op8(r+1);
            add_op8(node->nd_args->nd_alen);

          } else if(nd_type(tmp) == NODE_ARGSCAT) {
            int ah = next_reg();
            int at = next_reg();

            if(!compile(tmp->nd_head, ah)) return false;
            if(!compile(tmp->nd_body, at)) return false;

            add_op8(kSetArgsFromCat);
            add_op8(ah);
            add_op8(at);
          } else {
            break;
          }

          add_op8(kPrivateCallWithBlock);
          add_op8(dest);
          add_op8(add_literal(node->nd_mid));
          add_op8(r);

          reset_reg(r);
          return true;

        case NODE_CALL:
          if(!compile(node->nd_recv, next_reg())) return false;

          tmp = node->nd_args;

          if(nd_type(tmp) != NODE_ARRAY) goto bp_unsup;

          while(tmp) {
            if(!compile(tmp->nd_head, next_reg())) return false;
            tmp = tmp->nd_next;
          }

          add_op8(kSetArgs);
          add_op8(r+2);
          add_op8(node->nd_args->nd_alen);

          add_op8(kCallWithBlock);
          add_op8(dest);
          add_op8(add_literal(node->nd_mid));
          add_op8(r);

          reset_reg(r);
          return true;
        }

bp_unsup:
        printf("unsupported block_pass form: %s, %s\n",
               node_name(node),
               node_name(node->nd_args));
        return false;

      }
      break;

    case NODE_LVAR:
      add_op8(kGetLocal);
      add_op8(dest);
      add_op8(node->nd_cnt);
      break;

    case NODE_LASGN:
      if(!compile(node->nd_value, dest)) return false;
      add_op8(kSetLocal);
      add_op8(dest);
      add_op8(node->nd_cnt);
      break;

    case NODE_STR:
      add_op8(kCreateString);
      add_op8(dest);
      add_op8(add_literal(node->nd_lit));
      break;

    case NODE_WHILE:
    case NODE_UNTIL:
    case NODE_BREAK:
    case NODE_NEXT:
    case NODE_REDO:
    case NODE_RETRY:
    case NODE_SPLAT:
    case NODE_TO_ARY:
    case NODE_SVALUE:
    case NODE_YIELD:
    case NODE_AND:
    case NODE_OR:
    case NODE_NOT:
    case NODE_DOT2:
    case NODE_DOT3:
    case NODE_RETURN:
    case NODE_VCALL:
    case NODE_ATTRASGN:
    case NODE_SCOPE:
    case NODE_IVAR:
    case NODE_IASGN:

    case NODE_COLON2:
    case NODE_COLON3:
    case NODE_HASH:
    case NODE_ZARRAY:
    case NODE_EVSTR:


    case NODE_DSTR:
    case NODE_DXSTR:
    case NODE_DREGX:
    case NODE_DREGX_ONCE:
    case NODE_DSYM:
    case NODE_XSTR:

    case NODE_NTH_REF:
    case NODE_BACK_REF:
    case NODE_CVAR:
    case NODE_CVDECL:
    case NODE_CVASGN:

    case NODE_GVAR:
    case NODE_GASGN:

    case NODE_DVAR:
    case NODE_DASGN:
    case NODE_DASGN_CURR:

    case NODE_DEFN:
    case NODE_DEFS:
    case NODE_UNDEF:
    case NODE_ALIAS:
    case NODE_VALIAS:
    case NODE_CLASS:
    case NODE_SCLASS:
    case NODE_MODULE:
    case NODE_DEFINED:

    case NODE_OP_ASGN1:
    case NODE_OP_ASGN2:
    case NODE_OP_ASGN_AND:
    case NODE_OP_ASGN_OR:
    case NODE_MASGN:
    case NODE_CDECL:
    case NODE_SUPER:
    case NODE_ZSUPER:
    case NODE_ARGSPUSH:
    case NODE_ARGSCAT:
    case NODE_RESCUE:
    case NODE_ENSURE:
    case NODE_ITER:
    case NODE_FOR:
    case NODE_CASE:
    case NODE_WHEN:
    case NODE_MATCH:
    case NODE_MATCH2:
    case NODE_MATCH3:

    case NODE_FLIP2:
    case NODE_FLIP3:
    case NODE_POSTEXE:
    case NODE_OPT_N:
    default:
      printf("failed on %s\n", node_name(node));
      return false;
    }

    return true;
  }
};

extern "C" int compile_node(NODE* blk) {
  RegisterCompiler comp;
  CompiledCode* cc = comp.compile_top(blk->nd_next);
  if(cc) {
    blk->flags |= FL_FREEZE;
    blk->u2.value = (VALUE)&cc->caller_obj;
  }
}

void runtime_mark(void*) {
  g_runtime->mark();
}

VALUE hook_global;

extern "C" void init_runtime() {
  g_runtime = new Runtime;

  id_plus = rb_intern("+");

  hook_global = Data_Wrap_Struct(rb_cObject, runtime_mark, 0, 0);
  rb_global_variable(&hook_global);
}
