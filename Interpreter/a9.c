#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "a9.h"

void *ktr_letccr__m__k(void *body, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letccr__m__k_kt;
  _data->u._letccr__m__k._body = body;
  _data->u._letccr__m__k._k = k;
  return (void *)_data;
}

void *ktr_outerr__m__kr__m__mult(void *xr1, void *env, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _outerr__m__kr__m__mult_kt;
  _data->u._outerr__m__kr__m__mult._xr1 = xr1;
  _data->u._outerr__m__kr__m__mult._env = env;
  _data->u._outerr__m__kr__m__mult._k = k;
  return (void *)_data;
}

void *ktr_innerr__m__kr__m__mult(void *xr2, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr__m__kr__m__mult_kt;
  _data->u._innerr__m__kr__m__mult._xr2 = xr2;
  _data->u._innerr__m__kr__m__mult._k = k;
  return (void *)_data;
}

void *ktr_kr__m__subr1(void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__subr1_kt;
  _data->u._kr__m__subr1._k = k;
  return (void *)_data;
}

void *ktr_kr__m__zero(void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__zero_kt;
  _data->u._kr__m__zero._k = k;
  return (void *)_data;
}

void *ktr_kr__m__if(void *conseq, void *alt, void *env, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__if_kt;
  _data->u._kr__m__if._conseq = conseq;
  _data->u._kr__m__if._alt = alt;
  _data->u._kr__m__if._env = env;
  _data->u._kr__m__if._k = k;
  return (void *)_data;
}

void *ktr_outerr__m__kr__m__throw(void *kr__m__exp, void *env, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _outerr__m__kr__m__throw_kt;
  _data->u._outerr__m__kr__m__throw._kr__m__exp = kr__m__exp;
  _data->u._outerr__m__kr__m__throw._env = env;
  _data->u._outerr__m__kr__m__throw._k = k;
  return (void *)_data;
}

void *ktr_innerr__m__kr__m__throw(void *vr__m__exp, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr__m__kr__m__throw_kt;
  _data->u._innerr__m__kr__m__throw._vr__m__exp = vr__m__exp;
  _data->u._innerr__m__kr__m__throw._k = k;
  return (void *)_data;
}

void *ktr_outerr__m__kr__m__let(void *env, void *body, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _outerr__m__kr__m__let_kt;
  _data->u._outerr__m__kr__m__let._env = env;
  _data->u._outerr__m__kr__m__let._body = body;
  _data->u._outerr__m__kr__m__let._k = k;
  return (void *)_data;
}

void *ktr_innerr__m__kr__m__let(void *body, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr__m__kr__m__let_kt;
  _data->u._innerr__m__kr__m__let._body = body;
  _data->u._innerr__m__kr__m__let._k = k;
  return (void *)_data;
}

void *ktr_innerr__m__kr__m__application(void *randr__ex__, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr__m__kr__m__application_kt;
  _data->u._innerr__m__kr__m__application._randr__ex__ = randr__ex__;
  _data->u._innerr__m__kr__m__application._k = k;
  return (void *)_data;
}

void *ktr_outerr__m__kr__m__application(void *rator, void *env, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _outerr__m__kr__m__application_kt;
  _data->u._outerr__m__kr__m__application._rator = rator;
  _data->u._outerr__m__kr__m__application._env = env;
  _data->u._outerr__m__kr__m__application._k = k;
  return (void *)_data;
}

void *ktr_kr__m__extendenv(void *body, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__extendenv_kt;
  _data->u._kr__m__extendenv._body = body;
  _data->u._kr__m__extendenv._k = k;
  return (void *)_data;
}

void *ktr_emptyr__m__k(void *unmount) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__k_kt;
  _data->u._emptyr__m__k._unmount = unmount;
  return (void *)_data;
}

void *envrr_empty() {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _empty_envr;
  return (void *)_data;
}

void *envrr_extend(void *a, void *env) {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extend_envr;
  _data->u._extend._a = a;
  _data->u._extend._env = env;
  return (void *)_data;
}

void *closurer_make(void *body, void *env) {
closure* _data = (closure*)malloc(sizeof(closure));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _make_closure;
  _data->u._make._body = body;
  _data->u._make._env = env;
  return (void *)_data;
}

void *exprr_const(void *cexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_expr;
  _data->u._const._cexp = cexp;
  return (void *)_data;
}

void *exprr_var(void *n) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_expr;
  _data->u._var._n = n;
  return (void *)_data;
}

void *exprr_if(void *test, void *conseq, void *alt) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *exprr_mult(void *nexpr1, void *nexpr2) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_expr;
  _data->u._mult._nexpr1 = nexpr1;
  _data->u._mult._nexpr2 = nexpr2;
  return (void *)_data;
}

void *exprr_subr1(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_expr;
  _data->u._subr1._nexp = nexp;
  return (void *)_data;
}

void *exprr_zero(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_expr;
  _data->u._zero._nexp = nexp;
  return (void *)_data;
}

void *exprr_letcc(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letcc_expr;
  _data->u._letcc._body = body;
  return (void *)_data;
}

void *exprr_throw(void *kexp, void *vexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throw_expr;
  _data->u._throw._kexp = kexp;
  _data->u._throw._vexp = vexp;
  return (void *)_data;
}

void *exprr_let(void *exp, void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_expr;
  _data->u._let._exp = exp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *exprr_lambda(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *exprr_app(void *rator, void *rand) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
e = (void *)exprr_let(exprr_lambda(exprr_lambda(exprr_if(exprr_zero(exprr_var((void *)0)),exprr_const((void *)1),exprr_mult(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_subr1(exprr_var((void *)0))))))),exprr_mult(exprr_letcc(exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_throw(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_const((void *)4))))),exprr_const((void *)5)));
env = (void *)envrr_empty();
pc = &valuer__m__ofr__m__cps;
mount_tram();
printf("Factorial of 5: %d\n", (int)arg);}

void applyr__m__k()
{
kt* _c = (kt*)k;
switch (_c->tag) {
case _emptyr__m__k_kt: {
void *unmount = _c->u._emptyr__m__k._unmount;
_trstr *trstr = (_trstr *)unmount;
longjmp(*trstr->jmpbuf, 1);
break; }
case _innerr__m__kr__m__application_kt: {
void *randr__ex__ = _c->u._innerr__m__kr__m__application._randr__ex__;
void *kr1 = _c->u._innerr__m__kr__m__application._k;
k = (void *)kr1;
a = (void *)randr__ex__;
clos = (void *)arg;
pc = &applyr__m__closure;
break; }
case _outerr__m__kr__m__application_kt: {
void *rator = _c->u._outerr__m__kr__m__application._rator;
void *envr1 = _c->u._outerr__m__kr__m__application._env;
void *kr1 = _c->u._outerr__m__kr__m__application._k;
k = (void *)ktr_innerr__m__kr__m__application(arg,kr1);
e = (void *)rator;
env = (void *)envr1;
pc = &valuer__m__ofr__m__cps;
break; }
case _innerr__m__kr__m__let_kt: {
void *body = _c->u._innerr__m__kr__m__let._body;
void *kr1 = _c->u._innerr__m__kr__m__let._k;
k = (void *)kr1;
e = (void *)body;
env = (void *)arg;
pc = &valuer__m__ofr__m__cps;
break; }
case _outerr__m__kr__m__let_kt: {
void *envr1 = _c->u._outerr__m__kr__m__let._env;
void *body = _c->u._outerr__m__kr__m__let._body;
void *kr1 = _c->u._outerr__m__kr__m__let._k;
k = (void *)kr1;
env = (void *)envrr_extend(arg,envr1);
e = (void *)body;
pc = &valuer__m__ofr__m__cps;
break; }
case _innerr__m__kr__m__throw_kt: {
void *vr__m__exp = _c->u._innerr__m__kr__m__throw._vr__m__exp;
void *kr1 = _c->u._innerr__m__kr__m__throw._k;
k = (void *)arg;
arg = (void *)vr__m__exp;
pc = &applyr__m__k;
break; }
case _outerr__m__kr__m__throw_kt: {
void *kr__m__exp = _c->u._outerr__m__kr__m__throw._kr__m__exp;
void *envr1 = _c->u._outerr__m__kr__m__throw._env;
void *kr1 = _c->u._outerr__m__kr__m__throw._k;
k = (void *)ktr_innerr__m__kr__m__throw(arg,kr1);
env = (void *)envr1;
e = (void *)kr__m__exp;
pc = &valuer__m__ofr__m__cps;
break; }
case _kr__m__if_kt: {
void *conseq = _c->u._kr__m__if._conseq;
void *alt = _c->u._kr__m__if._alt;
void *envr1 = _c->u._kr__m__if._env;
void *kr1 = _c->u._kr__m__if._k;
if(arg) {
  k = (void *)kr1;
env = (void *)envr1;
e = (void *)conseq;
pc = &valuer__m__ofr__m__cps;

} else {
  k = (void *)kr1;
env = (void *)envr1;
e = (void *)alt;
pc = &valuer__m__ofr__m__cps;

}
break; }
case _kr__m__zero_kt: {
void *kr1 = _c->u._kr__m__zero._k;
k = (void *)kr1;
arg = (void *)(arg == 0);
pc = &applyr__m__k;
break; }
case _kr__m__subr1_kt: {
void *kr1 = _c->u._kr__m__subr1._k;
k = (void *)kr1;
arg = (void *)(void *)((int)arg - 1);
pc = &applyr__m__k;
break; }
case _innerr__m__kr__m__mult_kt: {
void *xr2 = _c->u._innerr__m__kr__m__mult._xr2;
void *kr1 = _c->u._innerr__m__kr__m__mult._k;
k = (void *)kr1;
arg = (void *)(void *)((int)arg * (int)xr2);
pc = &applyr__m__k;
break; }
case _outerr__m__kr__m__mult_kt: {
void *xr1 = _c->u._outerr__m__kr__m__mult._xr1;
void *envr1 = _c->u._outerr__m__kr__m__mult._env;
void *kr1 = _c->u._outerr__m__kr__m__mult._k;
k = (void *)ktr_innerr__m__kr__m__mult(arg,kr1);
e = (void *)xr1;
env = (void *)envr1;
pc = &valuer__m__ofr__m__cps;
break; }
case _kr__m__extendenv_kt: {
void *body = _c->u._kr__m__extendenv._body;
void *kr1 = _c->u._kr__m__extendenv._k;
k = (void *)kr1;
env = (void *)arg;
e = (void *)body;
pc = &valuer__m__ofr__m__cps;
break; }
case _letccr__m__k_kt: {
void *body = _c->u._letccr__m__k._body;
void *kr1 = _c->u._letccr__m__k._k;
k = (void *)kr1;
env = (void *)arg;
e = (void *)body;
pc = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__env()
{
envr* _c = (envr*)env;
switch (_c->tag) {
case _empty_envr: {
printf("error");break; }
case _extend_envr: {
void *narg = _c->u._extend._a;
void *oldEnv = _c->u._extend._env;
if((y == 0)) {
  arg = (void *)narg;
pc = &applyr__m__k;

} else {
  y = (void *)(void *)((int)y - 1);
env = (void *)oldEnv;
pc = &applyr__m__env;

}
break; }
}
}

void applyr__m__closure()
{
closure* _c = (closure*)clos;
switch (_c->tag) {
case _make_closure: {
void *body = _c->u._make._body;
void *envr1 = _c->u._make._env;
env = (void *)envrr_extend(a,envr1);
e = (void *)body;
pc = &valuer__m__ofr__m__cps;
break; }
}
}

void valuer__m__ofr__m__cps()
{
expr* _c = (expr*)e;
switch (_c->tag) {
case _const_expr: {
void *expr = _c->u._const._cexp;
arg = (void *)expr;
pc = &applyr__m__k;
break; }
case _mult_expr: {
void *xr1 = _c->u._mult._nexpr1;
void *xr2 = _c->u._mult._nexpr2;
k = (void *)ktr_outerr__m__kr__m__mult(xr1,env,k);
e = (void *)xr2;
pc = &valuer__m__ofr__m__cps;
break; }
case _subr1_expr: {
void *x = _c->u._subr1._nexp;
k = (void *)ktr_kr__m__subr1(k);
e = (void *)x;
pc = &valuer__m__ofr__m__cps;
break; }
case _zero_expr: {
void *x = _c->u._zero._nexp;
k = (void *)ktr_kr__m__zero(k);
e = (void *)x;
pc = &valuer__m__ofr__m__cps;
break; }
case _if_expr: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
k = (void *)ktr_kr__m__if(conseq,alt,env,k);
e = (void *)test;
pc = &valuer__m__ofr__m__cps;
break; }
case _letcc_expr: {
void *body = _c->u._letcc._body;
env = (void *)envrr_extend(k,env);
e = (void *)body;
pc = &valuer__m__ofr__m__cps;
break; }
case _throw_expr: {
void *kr__m__exp = _c->u._throw._kexp;
void *vr__m__exp = _c->u._throw._vexp;
k = (void *)ktr_outerr__m__kr__m__throw(kr__m__exp,env,k);
e = (void *)vr__m__exp;
pc = &valuer__m__ofr__m__cps;
break; }
case _let_expr: {
void *er1 = _c->u._let._exp;
void *body = _c->u._let._body;
k = (void *)ktr_outerr__m__kr__m__let(env,body,k);
e = (void *)er1;
pc = &valuer__m__ofr__m__cps;
break; }
case _var_expr: {
void *expr = _c->u._var._n;
y = (void *)expr;
pc = &applyr__m__env;
break; }
case _lambda_expr: {
void *body = _c->u._lambda._body;
arg = (void *)closurer_make(body,env);
pc = &applyr__m__k;
break; }
case _app_expr: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
k = (void *)ktr_outerr__m__kr__m__application(rator,env,k);
e = (void *)rand;
pc = &valuer__m__ofr__m__cps;
break; }
}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
k= (void *)ktr_emptyr__m__k(dismount);
for(;;) {
pc();
}
}
return 0;
}
