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
    'for': 'FOR',
    'print': 'PRINT'
}

tokens = ['FLOATV',
          'INTV',
          'EQC',
          'NOTEQC',
          'BIGGEREQ',
          'SMALLEREQ',
          'STRINGV',
          'ID'] + list(reserved.values())

literals = ['=', '+', '-', '*', '/', '^', '(', ')', '{', '}', '<', '>', ';']

t_EQC = r'=='
t_NOTEQC = r'!='
t_BIGGEREQ = r'>='
t_SMALLEREQ = r'<='


def t_FLOATV(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t


def t_INTV(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_STRING(t):
    r'".*"'
    t.value = t.value.replace("\"", "")
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


lex.lex()

precedence = (
    ('right', '='),
    ('left', 'EQC', 'NOTEQC'),
    ('left', '+', '-'),
    ('left', '*', '/'),
    ('left', '^'),
    ('left', 'AND', 'OR'),
    ('nonassoc', '<', '>', 'BIGGEREQ', 'SMALLEREQ'),
    ('right', 'UMINUS')
)

names = {}
prog = {}

def p_start(p):
    '''prog : statement'''
    global prog
    prog = p[1]


def p_statement(p):
    '''statement : conditional statement
                 | while statement
                 | for statement
                 | declare ';' statement
                 | print ';' statement
                 | none'''
    if len(p) > 2:  
        if p[2] == ';':
            p[2] = p[3]
        p[0] = (p[1], ) + p[2]
    else:
        p[0] = ()


def p_none(p):
    'none :'
    pass


def p_conditional(p):
    '''conditional : if elif else'''
    p[0] = ('conditional', p[1], p[2], p[3])


def p_if(p):
    '''if : IF '(' expression ')' '{' statement '}' '''
    p[0] = ('if', p[3], p[6])


def p_elif(p):
    '''elif : ELIF '(' expression ')' '{' statement '}' elif
                 | none'''
    if len(p) > 2:  
        p[0] = (('elif', p[3], p[6]), ) + p[8]
    else:
        p[0] = ()


def p_else(p):
    '''else : ELSE '{' statement '}'
            | none'''
    if len(p) > 2:  
        p[0] = ('else', p[3])


def p_while(p):
    '''while : WHILE '(' expression ')' '{' statement '}'
             | DO '{' statement '}' WHILE '(' expression ')' ';' '''
    if p[1] == "while":
        p[0] = ('while', p[3], p[6])
    else:
        p[0] = ('do-while', p[7], p[3])


def p_for(p):
    '''for : FOR '(' declarationAssign ';' expression ';' declareAssign ')' '{' statement '}' '''
    p[0] = ('for', p[3], p[5], p[7], p[10])


def p_type(p):
    '''type : INT
            | FLOAT
            | STRING
            | BOOLEAN'''
    p[0] = p[1]


def p_declare(p):
    '''declare : declaration
               | declarationAssign
               | declareAssign'''
    p[0] = p[1]


def p_declaration(p):
    '''declaration : type ID'''
    p[0] = ('declare', p[1], p[2])


def p_declarationAssign(p):
    '''declarationAssign : type ID '=' expression'''
    p[0] = ('declareAssign', p[1], p[2], p[4])


def p_declareAssign(p):
    '''declareAssign : ID '=' expression'''
    p[0] = ('assign', p[1], p[3])
    p[0] = ('assign', p[1], p[3])


def p_print(p):
    'print : PRINT expression'
    # print(p[2])
    p[0] = ('print', p[2])


def p_expression_operation(p):
    '''expression : expression '+' expression
                  | expression '-' expression
                  | expression '*' expression
                  | expression '/' expression
                  | expression '^' expression
                  | expression EQC expression
                  | expression NOTEQC expression
                  | expression BIGGEREQ expression
                  | expression SMALLEREQ expression
                  | expression '>' expression
                  | expression '<' expression
                  | expression AND expression
                  | expression OR expression'''
    p[0] = ('operation', p[1], p[2], p[3])


def p_expression_uminus(p):
    '''expression : '-' expression %prec UMINUS'''
    p[0] = -p[2]


def p_expression_group(p):
    '''expression : '(' expression ')' '''
    p[0] = p[2]


def p_expression_number(p):
    '''expression : INTV
                  | FLOATV
                  | STRINGV
                  | boolval'''
    p[0] = p[1]


def p_boolVal(p):
    '''boolval : TRUE
               | FALSE'''
    if p[1] == "true":
        p[0] = True
    elif p[1] == "false":
        p[0] = False


def p_expression_ID(p):
    "expression : ID"
    p[0] = p[1]




def p_error(t):
    if t:
        print("Syntax error at '%s'" % t.value)
    else:
        print("Syntax error at EOF")


yacc.yacc()
file = open("script.txt", "r")
s = file.read()
yacc.parse(s)

print('==== Tree ====')
print(prog)
