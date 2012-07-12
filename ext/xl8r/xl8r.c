#include "ruby.h"
#include "node.h"
#include "env.h"
#include "util.h"
#include "rubysig.h"

#include <dlfcn.h>

static void
scope_dup(scope)
    struct SCOPE *scope;
{
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

struct iter {
    int iter;
    struct iter *prev;
};

static struct iter **int_ruby_iter;
#define ruby_iter (*int_ruby_iter)

static unsigned long *int_frame_unique;
#define frame_unique (*int_frame_unique)

#define PUSH_FRAME() do {		\
    volatile struct FRAME _frame;	\
    _frame.prev = ruby_frame;		\
    _frame.tmp  = 0;			\
    _frame.node = ruby_current_node;	\
    _frame.iter = ruby_iter->iter;	\
    _frame.argc = 0;			\
    _frame.flags = 0;			\
    _frame.uniq = frame_unique++;	\
    ruby_frame = &_frame

#define POP_FRAME()  			\
    ruby_current_node = _frame.node;	\
    ruby_frame = _frame.prev;		\
} while (0)

#define ITER_NOT 0
#define ITER_PRE 1
#define ITER_CUR 2
#define ITER_PAS 3

#define PUSH_ITER(i) do {		\
    struct iter _iter;			\
    _iter.prev = ruby_iter;		\
    _iter.iter = (i);			\
    ruby_iter = &_iter

#define POP_ITER()			\
    ruby_iter = _iter.prev;		\
} while (0)

struct tag {
    rb_jmpbuf_t buf;
    struct FRAME *frame;
    struct iter *iter;
    VALUE tag;
    VALUE retval;
    struct SCOPE *scope;
    VALUE dst;
    struct tag *prev;
    int blkid;
};

static struct tag **int_prot_tag;
#define prot_tag (*int_prot_tag)

#define PUSH_TAG(ptag) do {		\
    struct tag _tag;			\
    _tag.retval = Qnil;			\
    _tag.frame = ruby_frame;		\
    _tag.iter = ruby_iter;		\
    _tag.prev = prot_tag;		\
    _tag.scope = ruby_scope;		\
    _tag.tag = ptag;			\
    _tag.dst = 0;			\
    _tag.blkid = 0;			\
    prot_tag = &_tag

#define PROT_NONE   Qfalse	/* 0 */
#define PROT_THREAD Qtrue	/* 2 */
#define PROT_FUNC   INT2FIX(0)	/* 1 */
#define PROT_LOOP   INT2FIX(1)	/* 3 */
#define PROT_LAMBDA INT2FIX(2)	/* 5 */
#define PROT_YIELD  INT2FIX(3)	/* 7 */

#define EXEC_TAG()    ruby_setjmp(((void)0), prot_tag->buf)

#define JUMP_TAG(st) do {		\
    ruby_frame = prot_tag->frame;	\
    ruby_iter = prot_tag->iter;		\
    ruby_longjmp(prot_tag->buf,(st));	\
} while (0)

#define POP_TAG()			\
    prot_tag = _tag.prev;		\
} while (0)

#define TAG_DST() (_tag.dst == (VALUE)ruby_frame->uniq)

#define TAG_RETURN	0x1
#define TAG_BREAK	0x2
#define TAG_NEXT	0x3
#define TAG_RETRY	0x4
#define TAG_REDO	0x5
#define TAG_RAISE	0x6
#define TAG_THROW	0x7
#define TAG_FATAL	0x8
#define TAG_MASK	0xf

#define PUSH_CLASS(c) do {		\
    volatile VALUE _class = ruby_class;	\
    ruby_class = (c)

#define POP_CLASS() ruby_class = _class; \
} while (0)

#define PUSH_SCOPE() do {		\
    volatile int _vmode = scope_vmode;	\
    struct SCOPE * volatile _old;	\
    NEWOBJ(_scope, struct SCOPE);	\
    OBJSETUP(_scope, 0, T_SCOPE);	\
    _scope->local_tbl = 0;		\
    _scope->local_vars = 0;		\
    _scope->flags = 0;			\
    _old = ruby_scope;			\
    ruby_scope = _scope;		\
    scope_vmode = SCOPE_PUBLIC

#define main_thread rb_main_thread
#define curr_thread rb_curr_thread

static struct SCOPE **int_top_scope;
#define top_scope (*int_top_scope)


#define POP_SCOPE() 			\
    if (ruby_scope->flags & SCOPE_DONT_RECYCLE) {\
	if (_old) scope_dup(_old);	\
    }					\
    if (!(ruby_scope->flags & SCOPE_MALLOC)) {\
	ruby_scope->local_vars = 0;	\
	ruby_scope->local_tbl  = 0;	\
	if (!(ruby_scope->flags & SCOPE_DONT_RECYCLE) && \
	    ruby_scope != top_scope) {	\
	    rb_gc_force_recycle((VALUE)ruby_scope);\
	}				\
    }					\
    ruby_scope->flags |= SCOPE_NOSTACK;	\
    ruby_scope = _old;			\
    scope_vmode = _vmode;		\
} while (0)

#define PUSH_CLASS(c) do {		\
    volatile VALUE _class = ruby_class;	\
    ruby_class = (c)

#define POP_CLASS() ruby_class = _class; \
} while (0)

extern struct RVarmap *ruby_dyna_vars;
#define PUSH_VARS() do { \
    struct RVarmap * volatile _old; \
    _old = ruby_dyna_vars; \
    ruby_dyna_vars = 0

#define POP_VARS() \
    if (_old && (ruby_scope->flags & SCOPE_DONT_RECYCLE)) {\
	if (RBASIC(_old)->flags) /* unless it's already recycled */ \
	    FL_SET(_old, DVAR_DONT_RECYCLE); \
    }\
    ruby_dyna_vars = _old; \
} while (0)

#define DVAR_DONT_RECYCLE FL_USER2

#ifdef C_ALLOCA
# define TMP_PROTECT NODE * volatile tmp__protect_tmp=0
# define TMP_ALLOC(n)							\
    (tmp__protect_tmp = NEW_NODE(NODE_ALLOCA,				\
				 ALLOC_N(VALUE,n),tmp__protect_tmp,n),	\
     (void*)tmp__protect_tmp->nd_head)
#else
# define TMP_PROTECT typedef int foobazzz
# define TMP_ALLOC(n) ALLOCA_N(VALUE,n)
#endif

typedef struct event_hook {
    rb_event_hook_func_t func;
    rb_event_t events;
    struct event_hook *next;
} rb_event_hook_t;

static rb_event_hook_t **int_event_hooks;

#define event_hooks (*int_event_hooks)

#define EXEC_EVENT_HOOK(event, node, self, id, klass) \
    do { \
	rb_event_hook_t *hook = event_hooks; \
        rb_event_hook_func_t hook_func; \
        rb_event_t events; \
	\
	while (hook) { \
            hook_func = hook->func; \
            events = hook->events; \
            hook = hook->next; \
	    if (events & event) \
		(*hook_func)(event, node, self, id, klass); \
	} \
    } while (0)

static int *int_scope_vmode;
#define scope_vmode (*int_scope_vmode)

#define SCOPE_PUBLIC    0
#define SCOPE_PRIVATE   1
#define SCOPE_PROTECTED 2
#define SCOPE_MODFUNC   5
#define SCOPE_MASK      7
#define SCOPE_SET(f)  (scope_vmode=(f))
#define SCOPE_TEST(f) (scope_vmode&(f))

#define ruby_cbase (ruby_cref->nd_clss)

struct BLOCK {
    NODE *var;
    NODE *body;
    VALUE self;
    struct FRAME frame;
    struct SCOPE *scope;
    VALUE klass;
    NODE *cref;
    int iter;
    int vmode;
    int flags;
    int uniq;
    struct RVarmap *dyna_vars;
    VALUE orig_thread;
    VALUE wrapper;
    VALUE block_obj;
    struct BLOCK *outer;
    struct BLOCK *prev;
};

#define BLOCK_D_SCOPE 1
#define BLOCK_LAMBDA  2

#ifdef USE_CONTEXT

NORETURN(static void rb_jump_context(rb_jmpbuf_t, int));
static inline void
rb_jump_context(env, val)
    rb_jmpbuf_t env;
    int val;
{
    env->status = val;
    setcontext(&env->context);
    abort();			/* ensure noreturn */
}
/*
 * PRE_GETCONTEXT and POST_GETCONTEXT is a magic for getcontext, gcc,
 * IA64 register stack and SPARC register window combination problem.
 *
 * Assume following code sequence.
 * 
 * 1. set a register in the register stack/window such as r32/l0.
 * 2. call getcontext.
 * 3. use the register.
 * 4. update the register for other use.
 * 5. call setcontext indirectly (or directly).
 *
 * This code should be run as 1->2->3->4->5->3->4.
 * But after second getcontext return (second 3),
 * the register is broken (updated).
 * It's because getcontext/setcontext doesn't preserve the content of the
 * register stack/window.
 *
 * setjmp also doesn't preserve the content of the register stack/window.
 * But it has not the problem because gcc knows setjmp may return twice.
 * gcc detects setjmp and generates setjmp safe code.
 *
 * So setjmp calls before and after the getcontext call makes the code
 * somewhat safe.
 * It fix the problem on IA64.
 * It is not required that setjmp is called at run time, since the problem is
 * register usage.
 *
 * Since the magic setjmp is not enough for SPARC,
 * inline asm is used to prohibit registers in register windows.
 *
 * Since the problem is fixed at gcc 4.0.3, the magic is applied only for
 * prior versions of gcc.
 * http://gcc.gnu.org/bugzilla/show_bug.cgi?id=21957
 * http://gcc.gnu.org/bugzilla/show_bug.cgi?id=22127
 */
#  define GCC_VERSION_BEFORE(major, minor, patchlevel) \
    (defined(__GNUC__) && !defined(__INTEL_COMPILER) && \
     ((__GNUC__ < (major)) ||  \
      (__GNUC__ == (major) && __GNUC_MINOR__ < (minor)) || \
      (__GNUC__ == (major) && __GNUC_MINOR__ == (minor) && __GNUC_PATCHLEVEL__ < (patchlevel))))
#  if GCC_VERSION_BEFORE(4,0,3) && (defined(sparc) || defined(__sparc__))
#    ifdef __pic__
/*
 * %l7 is excluded for PIC because it is PIC register.
 * http://lists.freebsd.org/pipermail/freebsd-sparc64/2006-January/003739.html
 */
#      define PRE_GETCONTEXT \
	 ({ __asm__ volatile ("" : : :  \
	    "%o0", "%o1", "%o2", "%o3", "%o4", "%o5", "%o7", \
	    "%l0", "%l1", "%l2", "%l3", "%l4", "%l5", "%l6", \
	    "%i0", "%i1", "%i2", "%i3", "%i4", "%i5", "%i7"); })
#    else
#      define PRE_GETCONTEXT \
	 ({ __asm__ volatile ("" : : :  \
	    "%o0", "%o1", "%o2", "%o3", "%o4", "%o5", "%o7", \
	    "%l0", "%l1", "%l2", "%l3", "%l4", "%l5", "%l6", "%l7", \
	    "%i0", "%i1", "%i2", "%i3", "%i4", "%i5", "%i7"); })
#    endif
#    define POST_GETCONTEXT PRE_GETCONTEXT
#  elif GCC_VERSION_BEFORE(4,0,3) && defined(__ia64)
static jmp_buf function_call_may_return_twice_jmp_buf;
int function_call_may_return_twice_false_1 = 0;
int function_call_may_return_twice_false_2 = 0;
#    define PRE_GETCONTEXT \
       (function_call_may_return_twice_false_1 ? \
        setjmp(function_call_may_return_twice_jmp_buf) : \
        0)
#    define POST_GETCONTEXT \
       (function_call_may_return_twice_false_2 ? \
        setjmp(function_call_may_return_twice_jmp_buf) : \
        0)
#  elif defined(__PPC64__)
#    define JUST_BEFORE_SETJMP(extra_save, j) ((void)0)
#    define JUST_AFTER_SETJMP(extra_save, j) ((j)->status ? (void)0 : (extra_save))
#  elif defined(__FreeBSD__) && __FreeBSD__ < 7
/*
 * workaround for FreeBSD/i386 getcontext/setcontext bug.
 * clear the carry flag by (0 ? ... : ...).
 * FreeBSD PR 92110 http://www.freebsd.org/cgi/query-pr.cgi?pr=92110
 * [ruby-dev:28263]
 */
static int volatile freebsd_clear_carry_flag = 0;
#    define PRE_GETCONTEXT \
       (freebsd_clear_carry_flag ? (freebsd_clear_carry_flag = 0) : 0)
#  endif
#  ifndef PRE_GETCONTEXT
#    define PRE_GETCONTEXT 0
#  endif
#  ifndef POST_GETCONTEXT
#    define POST_GETCONTEXT 0
#  endif
#  ifndef JUST_BEFORE_SETJMP
#    define JUST_BEFORE_SETJMP(extra_save, j) (extra_save)
#  endif
#  ifndef JUST_AFTER_SETJMP
#    define JUST_AFTER_SETJMP(extra_save, j) ((void)0)
#  endif
#  define ruby_longjmp(env, val) rb_jump_context(env, val)
#  define ruby_setjmp(extra_save, j) ((j)->status = 0, \
     JUST_BEFORE_SETJMP(extra_save, j), \
     PRE_GETCONTEXT, \
     getcontext(&(j)->context), \
     POST_GETCONTEXT, \
     JUST_AFTER_SETJMP(extra_save, j), \
     (j)->status)
#else
#  define ruby_setjmp(extra_save, env) \
     ((extra_save), RUBY_SETJMP(env))
#  define ruby_longjmp(env,val) RUBY_LONGJMP(env,val)
#  ifdef __CYGWIN__
int _setjmp(), _longjmp();
#  endif
#endif

#define NOEX_TAINTED 8
#define NOEX_SAFE(n) ((n) >> 4)
#define NOEX_WITH(n, v) ((n) | (v) << 4)
#define NOEX_WITH_SAFE(n) NOEX_WITH(n, ruby_safe_level)

NORETURN(static void localjump_error(const char*, VALUE, int));
static void
localjump_error(mesg, value, reason)
    const char *mesg;
    VALUE value;
    int reason;
{
  VALUE exc = rb_exc_new2(rb_eLocalJumpError, mesg);
  ID id;

  rb_iv_set(exc, "@exit_value", value);
  switch (reason) {
    case TAG_BREAK:
      id = rb_intern("break"); break;
    case TAG_REDO:
      id = rb_intern("redo"); break;
    case TAG_RETRY:
      id = rb_intern("retry"); break;
    case TAG_NEXT:
      id = rb_intern("next"); break;
    case TAG_RETURN:
      id = rb_intern("return"); break;
    default:
      id = rb_intern("noreason"); break;
  }
  rb_iv_set(exc, "@reason", ID2SYM(id));
  rb_exc_raise(exc);
}

NORETURN(static void jump_tag_but_local_jump _((int,VALUE)));
static void
jump_tag_but_local_jump(state, val)
    int state;
    VALUE val;
{

  if (val == Qundef) val = prot_tag->retval;
  switch (state) {
    case 0:
      break;
    case TAG_RETURN:
      localjump_error("unexpected return", val, state);
      break;
    case TAG_BREAK:
      localjump_error("unexpected break", val, state);
      break;
    case TAG_NEXT:
      localjump_error("unexpected next", val, state);
      break;
    case TAG_REDO:
      localjump_error("unexpected redo", Qnil, state);
      break;
    case TAG_RETRY:
      localjump_error("retry outside of rescue clause", Qnil, state);
      break;
    default:
      break;
  }
  JUMP_TAG(state);
}

static VALUE (*mri_eval)(VALUE self, NODE* n);
static void (*mri_assign)(VALUE self, NODE* lhs, VALUE val, int pcall);

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

static NODE* setup_args(NODE* body, VALUE recv,
                       int argc, VALUE* argv, VALUE* local_vars)
{
  NODE *node = 0;
  int i, nopt = 0;

  if (nd_type(body) == NODE_ARGS) {
    node = body;
    body = 0;
  }
  else if (nd_type(body) == NODE_BLOCK) {
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

VALUE xl8r_call0(VALUE klass, VALUE recv, ID id, ID oid,
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
      {
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
        }
        else {
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
          result = mri_eval(recv, body);
        }
        else if (state == TAG_RETURN && TAG_DST()) {
          result = prot_tag->retval;
          state = 0;
        }
        POP_TAG();
        if (event_hooks) {
          EXEC_EVENT_HOOK(RUBY_EVENT_RETURN, ruby_current_node, recv, id, klass);
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
      }
      break;

    default:
      rb_bug("Unknown node used as method");
      break;
  }
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

void Init_xl8r_ext() {
  void* addr = dlsym(RTLD_DEFAULT, "rb_apply");

  printf("appl = %p\n", addr);
  /*

  void *a2 = (void*)(((long)addr) - diff);


  */

  void* call0 = APPLY_OFFSET(addr, OFFSET_CALL0);
  printf("call0 = %p\n", call0);

  int_ruby_iter = APPLY_OFFSET(addr, OFFSET_RUBY_ITER);
  int_frame_unique = APPLY_OFFSET(addr, OFFSET_FRAME_UNIQUE);
  int_prot_tag = APPLY_OFFSET(addr, OFFSET_PROT_TAG);
  int_top_scope = APPLY_OFFSET(addr, OFFSET_TOP_SCOPE);
  int_event_hooks = APPLY_OFFSET(addr, OFFSET_EVENT_HOOKS);
  int_scope_vmode = APPLY_OFFSET(addr, OFFSET_SCOPE_VMODE);

  mri_eval = APPLY_OFFSET(addr, OFFSET_RB_EVAL);
  mri_assign = APPLY_OFFSET(addr, OFFSET_ASSIGN);

  uintptr_t dest = ((uintptr_t)xl8r_call0) - (((uintptr_t)call0) + 6);
  uint8_t* buf = (uint8_t*)call0;

  mprotect((void*)(((uintptr_t)buf) & ~0xfff), 4096,
           PROT_READ | PROT_WRITE | PROT_EXEC);

  buf[0] = 0x66;
  buf[1] = 0xe9;
  buf[2] = ((dest >> 0)  & 0xff);
  buf[3] = ((dest >> 8)  & 0xff);
  buf[4] = ((dest >> 16) & 0xff);
  buf[5] = ((dest >> 24) & 0xff);
}
