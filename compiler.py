import ply.lex as lex

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

data = '''
 do {
    a = a + 5
 } while(a<50)
 '''

lexer.input(data)

for tok in lexer:
     print(tok)
