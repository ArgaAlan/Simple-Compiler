import ply.lex as lex
import ply.yacc as yacc

# Code based on the PLY basic example documentation https://www.dabeaz.com/ply/ply.html#ply_nn0

reserved = {
    'and': 'AND',
    'or': 'OR',
    'int': 'INT',
    'float': 'FLOAT',
    'string': 'STRING',
    'boolean': 'BOOLEAN',
    'true': 'TRUE',
    'false': 'FALSE',
    'if': 'IF',
    'elif': 'ELIF',
    'else': 'ELSE',
    'do': 'DO',
    'while': 'WHILE',
    'for': 'FOR'
}

tokens = ['FLOATV',
          'INTV',
          'PLUS',
          'MINUS',
          'TIMES',
          'DIVIDE',
          'EXP',
          'LPAREN',
          'RPAREN',
          'LKEY',
          'RKEY',
          'EQ',
          'EQC',
          'NOTEQC',
          'BIGGEREQ',
          'SMALLEREQ',
          'BIGGER',
          'SMALLER',
          'STRINGV',
          'ID'] + list(reserved.values())

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EXP = r'\^'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LKEY = r'{'
t_RKEY = r'}'
t_EQ = r'='
t_EQC = r'=='
t_NOTEQC = r'!='
t_BIGGEREQ = r'>='
t_SMALLEREQ = r'<='
t_BIGGER = r'>'
t_SMALLER = r'<'

def t_FLOATV(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INTV(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r'"[a-zA-Z_][a-zA-Z_0-9]*"'
    t.type = reserved.get(t.value, 'STRINGV')  # Check for reserved words
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')  # Check for reserved words
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


t_ignore = ' \t'

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()

precedence = (
    ('left','PLUS','MINUS'),
    ('left','TIMES','DIVIDE'),
    )

names = { }

def p_statement_assign(t):
    'statement : ID EQ expression'
    names[t[1]] = t[3]

def p_statement_expr(t):
    'statement : expression'
    print(t[1])

def p_expression_binop(t):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression'''
    if t[2] == '+'  : t[0] = t[1] + t[3]
    elif t[2] == '-': t[0] = t[1] - t[3]
    elif t[2] == '*': t[0] = t[1] * t[3]
    elif t[2] == '/': t[0] = t[1] / t[3]

def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = t[2]

def p_expression_intv(t):
    'expression : INTV'
    t[0] = t[1]

def p_expression_floatv(t):
    'expression : FLOATV'
    t[0] = t[1]

def p_expression_id(t):
    'expression : ID'
    try:
        t[0] = names[t[1]]
    except LookupError:
        print("Undefined id '%s'" % t[1])
        t[0] = 0

def p_error(t):
    print("Syntax error at '%s'" % t.value)

parser = yacc.yacc()

while True:
    try:
        s = input('calc > ')   # Use raw_input on Python 2
    except EOFError:
        break
    parser.parse(s)
