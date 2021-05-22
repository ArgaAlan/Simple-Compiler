import ply.lex as lex

# Code based on the PLY basic example documentation https://www.dabeaz.com/ply/ply.html#ply_nn0

tokens = ('NUMBER', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'EXP', 'LPAREN', 'RPAREN')

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EXP = r'\^'
t_LPAREN = r'\('
t_RPAREN = r'\)'


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


t_ignore = ' \t'


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()

data = '''
 3 + 4 * 10
   + -20 *2
 (5 ^ 2)
 '''

lexer.input(data)

for tok in lexer:
     print(tok)
