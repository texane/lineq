/* https://en.wikipedia.org/wiki/Symbolic_computation */
/* https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor */
/* https://en.wikipedia.org/wiki/Factorization_of_polynomials */


#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>


/* static linear equation parser */

/* the basic idea is to transform a linear equation symbolic */
/* form (ie. CALC4 = (IN4 + IN7) / 2) into a vector,scalar */
/* synthetic form (a and b variables in the code). This form */
/* is suitable for y = ax + b evaluation. Reductions are */
/* made, allowing for non trivial symbolic representations. */

/* symbolic form */
/* CALCx = A1 * IN1 + A2 * IN2 + ... + B */

/* parsed tokens */
/* channel names */
/* real numbers */
/* parenthesis */
/* equal, plus, minus, add, sub, div, mul */

/* typical operators and priorities */

/* distributivity */
/* CALCx = (IN1 + IN2) / 2 + ... + 4 */
/* results in */
/* CALCx = IN1 * 0.5 + IN2 * 0.5 + ... + 4 */

/* reduction */
/* CALCx = ((IN1 + 3) + (IN2 + 5)) / 2 */
/* results in */
/* CALCx = IN0 * 0.5 + IN1 * 0.5 + 4 */


typedef struct lineq_tok
{
  enum
  {
    LINEQ_TOK_EQ = 0,
    LINEQ_TOK_MUL,
    LINEQ_TOK_DIV,
    LINEQ_TOK_ADD,
    LINEQ_TOK_SUB,
    LINEQ_TOK_LPAR,
    LINEQ_TOK_RPAR,
    LINEQ_TOK_REAL,
    LINEQ_TOK_CALC,
    LINEQ_TOK_IN,
    LINEQ_TOK_INVALID
  } type;

  union
  {
    size_t id;
    double real;
  } val;

} lineq_tok_t;

static unsigned int is_ws(char c)
{
  return (c == ' ') || (c == '\t');
}

static unsigned int is_digit(char c)
{
  return (c >= '0') && (c <= '9');
}

static unsigned int is_sign(char c)
{
  return (c == '-') || (c == '+');
}

static unsigned int is_real(char c)
{
  return (c == '.') || is_digit(c) || is_sign(c);
}

static void skip_ws(const char** s, size_t* len)
{
  for (; *len && is_ws(**s); ++*s, --*len) ;
}

static unsigned int is_real_zero(double real)
{
  static const double epsilon = 0.000000001;
  return (real >= (epsilon * -1.0)) && (real <= epsilon);
}

static int parse_real(const char** s, size_t* len, double* x)
{
  char buf[32];
  size_t i;

  skip_ws(s, len);

  for (i = 0; i != sizeof(buf); ++i)
  {
    if (*len == 0) break ;
    if (is_real(**s) == 0) break ;
    buf[i] = **s;
    ++*s;
    --*len;
  }

  if ((i == 0) || (i == sizeof(buf))) goto on_error;
  buf[i] = 0;

  errno = 0;
  *x = strtod(buf, NULL);
  if (errno) goto on_error;

  return 0;

 on_error:
  return -1;
}

static int parse_in_or_calc
(const char** s, size_t* len, size_t* x, const char* p, size_t n)
{
  skip_ws(s, len);

  if (*len <= n) goto on_error;
  if (memcmp(*s, p, n)) goto on_error;

  *x = (size_t)((*s)[n] - '1');

  *s += n + 1;
  *len -= n + 1;

  return 0;

 on_error:
  return -1;
}

static int parse_calc(const char** s, size_t* len, size_t* x)
{
  return parse_in_or_calc(s, len, x, "CALC", sizeof("CALC") - 1);
}

static int parse_in(const char** s, size_t* len, size_t* x)
{
  return parse_in_or_calc(s, len, x, "IN", sizeof("IN") - 1);
}

__attribute__((unused))
static void print_tok(const lineq_tok_t* tok)
{
  printf("%s: ", __FUNCTION__);

  switch (tok->type)
  {
  case LINEQ_TOK_EQ: printf("="); break;
  case LINEQ_TOK_MUL: printf("*"); break;
  case LINEQ_TOK_DIV: printf("/"); break;
  case LINEQ_TOK_ADD: printf("+"); break;
  case LINEQ_TOK_SUB: printf("-"); break;
  case LINEQ_TOK_LPAR: printf("("); break;
  case LINEQ_TOK_RPAR: printf(")"); break;
  case LINEQ_TOK_REAL: printf("%lf", tok->val.real); break;
  case LINEQ_TOK_CALC: printf("CALC%zu", 1 + tok->val.id); break;
  case LINEQ_TOK_IN: printf("IN%zu", 1 + tok->val.id); break;
  case LINEQ_TOK_INVALID:
  default: printf("invalid"); break;
  }

  printf("\n");
}

static int parse_tok(const char** s, size_t* len, lineq_tok_t* tok)
{
  skip_ws(s, len);

  if (*len == 0) goto on_error;

  tok->type = LINEQ_TOK_INVALID;
  switch (**s)
  {
  case '=': tok->type = LINEQ_TOK_EQ; break ;
  case '*': tok->type = LINEQ_TOK_MUL; break ;
  case '+': tok->type = LINEQ_TOK_ADD; break ;
  case '-': tok->type = LINEQ_TOK_SUB; break ;
  case '/': tok->type = LINEQ_TOK_DIV; break ;
  case '(': tok->type = LINEQ_TOK_LPAR; break ;
  case ')': tok->type = LINEQ_TOK_RPAR; break ;
  default: break ;
  }

  if (tok->type != LINEQ_TOK_INVALID)
  {
    *s += 1;
    *len -= 1;
    goto on_success;
  }

  if (parse_real(s, len, &tok->val.real) == 0)
  {
    tok->type = LINEQ_TOK_REAL;
    goto on_success;
  }

  if (parse_in(s, len, &tok->val.id) == 0)
  {
    tok->type = LINEQ_TOK_IN;
    goto on_success;
  }

  if (parse_calc(s, len, &tok->val.id) == 0)
  {
    tok->type = LINEQ_TOK_CALC;
    goto on_success;
  }

 on_error:
  return -1;

 on_success:
  /* print_tok(tok); */
  return 0;
}

static int parse_addsub_expr
(const char**, size_t*, double*, size_t, double*);

static int parse_one_term
(const char** s, size_t* len, double* a, size_t n, double* b)
{
  /* parse a single term */

  lineq_tok_t tok;
  double sign;
  size_t i;

  for (i = 0; i != n; ++i) a[i] = 0.0;
  *b = 0.0;

  /* there may be a string of leading signs */

  sign = 1.0;

  while (1)
  {
    if (parse_tok(s, len, &tok)) goto on_error;

    if (tok.type == LINEQ_TOK_SUB)
    {
      sign *= -1.0;
      continue ;
    }
    else if (tok.type == LINEQ_TOK_ADD)
    {
      continue ;
    }
    else
    {
      break ;
    }
  }

  if (tok.type == LINEQ_TOK_LPAR)
  {
    if (parse_addsub_expr(s, len, a, n, b)) goto on_error;
    if (parse_tok(s, len, &tok)) goto on_error;
    if (tok.type != LINEQ_TOK_RPAR) goto on_error;
    goto on_success;
  }
  else if (tok.type == LINEQ_TOK_IN)
  {
    if (tok.val.id >= n) goto on_error;
    a[tok.val.id] = 1.0;
    goto on_success;
  }
  else if (tok.type == LINEQ_TOK_REAL)
  {
    *b = tok.val.real;
    goto on_success;
  }
  else
  {
    goto on_error;
  }

 on_success:

  /* apply sign */
  for (i = 0; i != n; ++i) a[i] *= sign;
  *b *= sign;

  return 0;

 on_error:
  return -1;
}

static int parse_addsub_expr
(const char** s, size_t* len, double* a, size_t n, double* b)
{
  /* xxx{l,r} convention for left or right side */
  /* TODO: left term can be remove, use a and b */

  lineq_tok_t tok;
  size_t i;
  double sign;
  double* ar = NULL;
  double br;
  double konst;
  int err = -1;
  const char* saved_s;
  size_t saved_len;

  ar = malloc(n * sizeof(double));
  if (ar == NULL) goto on_error;

  /* parse left term */
  if (parse_one_term(s, len, a, n, b)) goto on_error;

  while (1)
  {
    /* terminate expression parsing */
    saved_s = *s;
    saved_len = *len;
    if (parse_tok(s, len, &tok)) break ;

    if (tok.type == LINEQ_TOK_RPAR)
    {
      /* one token lookahead, restore string state */
      *s = saved_s;
      *len = saved_len;
      break ;
    }

    if ((tok.type == LINEQ_TOK_ADD) || (tok.type == LINEQ_TOK_SUB))
    {
      /* parse right expression */
      if (parse_addsub_expr(s, len, ar, n, &br)) goto on_error;

      /* reduce left op right into left */
      if (tok.type == LINEQ_TOK_ADD) sign = 1.0;
      else sign = -1.0;
      for (i = 0; i != n; ++i) a[i] += ar[i] * sign;
      *b += br * sign;
    }
    else if ((tok.type == LINEQ_TOK_DIV) || (tok.type == LINEQ_TOK_MUL))
    {
      /* parse right as one single term, non recursive */
      if (parse_one_term(s, len, ar, n, &br)) goto on_error;

      /* detect constant term. possible forms: */
      /* konst mul term (form 1) */
      /* term mul konst (form 2) */
      /* term div konst (form 3) */

      /* assume form 2 or form 3. a non zero coeff means form 1. */
      konst = br;
      for (i = 0; (i != n) && is_real_zero(ar[i]); ++i) ;

      if (i != n)
      {
	/* form 1. check and swap sides. */
	konst = *b;
	for (i = 0; (i != n) && (is_real_zero(a[i])); ++i) a[i] = ar[i];
	*b = br;
	if (i != n) goto on_error;
      }

      /* inverse constant in case of division */
      if (tok.type == LINEQ_TOK_DIV)
      {
	if (is_real_zero(konst))
	{
	  for (i = 0; i != n; ++i) a[i] = 0.0;
	  *b = 0.0;
	}
	else
	{
	  for (i = 0; i != n; ++i) a[i] /= konst;
	  *b /= konst;
	}
      }
      else
      {
	for (i = 0; i != n; ++i) a[i] *= konst;
	*b *= konst;
      }
    }
  }

  err = 0;

 on_error:
  if (ar != NULL) free(ar);
  return err;
}

static int parse_lineq
(const char* s, size_t* calc_id, double* a, size_t n, double* b)
{
  /* parse a linear equation string */
  /* s the lineq string */
  /* i the resulting left channel index */
  /* a the resulting vector containing A terms */
  /* n the vector size */
  /* b the scalar containing B term */

  lineq_tok_t tok;
  size_t len;

  len = strlen(s);

  /* left hand side */

  if (parse_tok(&s, &len, &tok)) goto on_error;
  if (tok.type != LINEQ_TOK_CALC) goto on_error;
  *calc_id = tok.val.id;

  /* equals */

  if (parse_tok(&s, &len, &tok)) goto on_error;
  if (tok.type != LINEQ_TOK_EQ) goto on_error;

  /* right hand side */

  if (parse_addsub_expr(&s, &len, a, n, b)) goto on_error;

  return 0;

 on_error:
  return -1;
}


static void print_lineq(const double* a, size_t n, double b)
{
  char plus = ' ';
  size_t i;

  for (i = 0; i != n; ++i)
  {
    if (is_real_zero(a[i])) continue ;
    printf(" %c %lf * IN%zu", plus, a[i], i + 1);
    plus = '+';
  }

  if (is_real_zero(b) == 0) printf(" %c %lf", plus, b);

  printf("\n");
}


int main(int ac, char** av)
{
  static const char* s[] =
  {
    "CALC4 = 3.15",
    "CALC4 = 2 * 3.15",
    "CALC4 = 10 / 5 + 3",
    "CALC4 = 30 / 5 / 3",
    "CALC4 = 30 / (1 + 1)",
    "CALC4 = 30 / 5 / (1 + 1)",

    "CALC4 = -IN2",
    "CALC4 = --IN2",
    "CALC4 = --(IN2)",

    "CALC4 = -(IN2 + 5)",

    "CALC4 = IN2 + 1 - 3",
    "CALC4 = 3.15 * IN3",
    "CALC4 = IN3 * 3.15",
    "CALC4 = 2 * IN3 * 3",
    "CALC4 = (24 * IN3 * 2) / 3",
    "CALC4 = 2 * (24 * IN3 * 2) / 3",

    "CALC4 = (IN2 + IN3 + IN4) / 3",
    "CALC4 = (IN2 + IN3 + IN4 + 2 * IN5) / 4",
    "CALC4 = 3 + (IN2 + IN3 + IN4 + IN5) / 4",

    "CALC4 = (IN2 + IN3 + 8) / 2",
    "CALC4 = (IN2 + 3 + IN3 + 5) / 2",
    "CALC4 = ((IN2 + 3) + (IN3 + 5)) / 2",
    "CALC4 = ((IN2 + 3) + (IN3 + 5)) / 2",

    "CALC4 = 4.68 + ((IN2 + 3) + (IN3 + 5)) / 2",
    "CALC4 = ((IN2 + 3) + (IN3 + 5)) / 2 + 4.68",

    NULL
  };

  static const size_t n = 8;
  double a[n];
  double b;
  size_t i;
  size_t calc_id;

  for (i = 0; s[i] != NULL; ++i)
  {
    if (parse_lineq(s[i], &calc_id, a, n, &b))
    {
      printf("error[%zu] %s\n", i, s[i]);
      return -1;
    }

    printf("\n");
    printf("xxxx\n");

    printf("xxxx [%zu] %s\n", i, s[i]);
    printf("xxxx [%zu] CALC%zu = ", i, calc_id + 1);

    print_lineq(a, n, b);

    printf("xxxx\n");
    printf("\n");
  }


  return 0;
}
