struct expr;
typedef struct expr expr;
struct expr {
  enum {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _letcc_expr,
    _throw_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union {
    struct { void *_cexp; } _const;
    struct { void *_n; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_nexpr1; void *_nexpr2; } _mult;
    struct { void *_nexp; } _subr1;
    struct { void *_nexp; } _zero;
    struct { void *_body; } _letcc;
    struct { void *_kexp; void *_vexp; } _throw;
    struct { void *_exp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *exprr_const(void *cexp);
void *exprr_var(void *n);
void *exprr_if(void *test, void *conseq, void *alt);
void *exprr_mult(void *nexpr1, void *nexpr2);
void *exprr_subr1(void *nexp);
void *exprr_zero(void *nexp);
void *exprr_letcc(void *body);
void *exprr_throw(void *kexp, void *vexp);
void *exprr_let(void *exp, void *body);
void *exprr_lambda(void *body);
void *exprr_app(void *rator, void *rand);

void *e, *env, *k, *arg, *y, *clos, *a;

void (*pc)();

void valuer__m__ofr__m__cps();
struct closure;
typedef struct closure closure;
struct closure {
  enum {
    _make_closure
  } tag;
  union {
    struct { void *_body; void *_env; } _make;
  } u;
};

void *closurer_make(void *body, void *env);

void applyr__m__closure();
struct envr;
typedef struct envr envr;
struct envr {
  enum {
    _empty_envr,
    _extend_envr
  } tag;
  union {
    struct { char dummy; } _empty;
    struct { void *_a; void *_env; } _extend;
  } u;
};

void *envrr_empty();
void *envrr_extend(void *a, void *env);

void applyr__m__env();
struct kt;
typedef struct kt kt;
struct kt {
  enum {
    _letccr__m__k_kt,
    _outerr__m__kr__m__mult_kt,
    _innerr__m__kr__m__mult_kt,
    _kr__m__subr1_kt,
    _kr__m__zero_kt,
    _kr__m__if_kt,
    _outerr__m__kr__m__throw_kt,
    _innerr__m__kr__m__throw_kt,
    _outerr__m__kr__m__let_kt,
    _innerr__m__kr__m__let_kt,
    _innerr__m__kr__m__application_kt,
    _outerr__m__kr__m__application_kt,
    _kr__m__extendenv_kt,
    _emptyr__m__k_kt
  } tag;
  union {
    struct { void *_body; void *_k; } _letccr__m__k;
    struct { void *_xr1; void *_env; void *_k; } _outerr__m__kr__m__mult;
    struct { void *_xr2; void *_k; } _innerr__m__kr__m__mult;
    struct { void *_k; } _kr__m__subr1;
    struct { void *_k; } _kr__m__zero;
    struct { void *_conseq; void *_alt; void *_env; void *_k; } _kr__m__if;
    struct { void *_kr__m__exp; void *_env; void *_k; } _outerr__m__kr__m__throw;
    struct { void *_vr__m__exp; void *_k; } _innerr__m__kr__m__throw;
    struct { void *_env; void *_body; void *_k; } _outerr__m__kr__m__let;
    struct { void *_body; void *_k; } _innerr__m__kr__m__let;
    struct { void *_randr__ex__; void *_k; } _innerr__m__kr__m__application;
    struct { void *_rator; void *_env; void *_k; } _outerr__m__kr__m__application;
    struct { void *_body; void *_k; } _kr__m__extendenv;
    struct { void *_unmount; } _emptyr__m__k;
  } u;
};

void *ktr_letccr__m__k(void *body, void *k);
void *ktr_outerr__m__kr__m__mult(void *xr1, void *env, void *k);
void *ktr_innerr__m__kr__m__mult(void *xr2, void *k);
void *ktr_kr__m__subr1(void *k);
void *ktr_kr__m__zero(void *k);
void *ktr_kr__m__if(void *conseq, void *alt, void *env, void *k);
void *ktr_outerr__m__kr__m__throw(void *kr__m__exp, void *env, void *k);
void *ktr_innerr__m__kr__m__throw(void *vr__m__exp, void *k);
void *ktr_outerr__m__kr__m__let(void *env, void *body, void *k);
void *ktr_innerr__m__kr__m__let(void *body, void *k);
void *ktr_innerr__m__kr__m__application(void *randr__ex__, void *k);
void *ktr_outerr__m__kr__m__application(void *rator, void *env, void *k);
void *ktr_kr__m__extendenv(void *body, void *k);
void *ktr_emptyr__m__k(void *unmount);

void applyr__m__k();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

