
#ifdef cplusplus
extern "C" {
#endif

void scope_dup(struct SCOPE* scope);
  
struct iter {
    int iter;
    struct iter *prev;
};

extern struct iter **int_ruby_iter;
#define ruby_iter (*int_ruby_iter)

extern unsigned long *int_frame_unique;
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

extern struct tag **int_prot_tag;
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

extern struct SCOPE **int_top_scope;
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

extern rb_event_hook_t **int_event_hooks;

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

extern int *int_scope_vmode;
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

static void
localjump_error(const char* mesg, VALUE value, int reason) {
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

static void
jump_tag_but_local_jump(int state, VALUE val) {
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

extern VALUE (*mri_eval)(VALUE self, NODE* n);
extern void (*mri_assign)(VALUE self, NODE* lhs, VALUE val, int pcall);

extern VALUE (*mri_call)(VALUE klass, VALUE recv, ID mid,
                  int argc, const VALUE* argv, int scope, VALUE self);

NODE* setup_args(NODE* body, VALUE recv,
                       int argc, VALUE* argv, VALUE* local_vars);

extern void* mri_blk_free;

extern struct BLOCK **int_ruby_block;
extern unsigned long *int_block_unique;

#define ruby_block (*int_ruby_block)
#define block_unique (*int_block_unique)

#ifdef cplusplus
}
#endif
