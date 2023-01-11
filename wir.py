#######################################
# TOKEN CLASS
#######################################

WT_INT = 'INT'
WT_FLOAT = 'FLOAT'
WT_LEFTPAR = 'LEFTPAR'
WT_RIGHTPAR = 'RIGHTPAR'
WT_MUL = 'MUL'
WT_DIV = 'DIV'
WT_PLUS = 'PLUS'
WT_MINUS = 'MINUS'

DIGITS = '0123456789'
SUPPORTED_CHARS = '()*/+-'

class Token:
    def __init__(self, wir_type, value=None):
        self.type = wir_type
        self.value = value
    
    def __repr__(self):
        if self.value:
            return f'{self.type}: {self.value}'
        else:
            return f'{self.type}'

#######################################
# LEXER CLASS
#######################################

class Lexer:
    def __init__(self, file_name, text):
        self.text = text
        self.file_name = file_name
        self.pos = LinePosition(-1, 0, -1, file_name, text)
        self.curr_char = None
        self.next_char()
    
    def next_char(self):
        self.pos.next(self.curr_char)
        self.curr_char = self.text[self.pos.ind] if self.pos.ind < len(self.text) else None

    def make_tokens(self):
        tkns = []

        while self.curr_char is not None:
            if self.curr_char == ' ' or self.curr_char == '\t':
                self.next_char()
            elif self.curr_char in SUPPORTED_CHARS:
                curr_char_tkn = self.make_op_tkn()
                tkns.append(curr_char_tkn)
                self.next_char()
            elif self.curr_char in DIGITS:
                tkns.append(self.make_num_tkn())
            else:
                start_pos = self.pos.copy_pos()
                unknown = self.curr_char
                self.next_char()
                return [], UnsupportedCharacterError(start_pos, self.pos, unknown)
        return tkns, None

    def make_op_tkn(self):
        if self.curr_char == '(':
            return Token(WT_LEFTPAR)
        elif self.curr_char == ')':
            return Token(WT_RIGHTPAR)
        elif self.curr_char == '*':
            return Token(WT_MUL)
        elif self.curr_char == '/':
            return Token(WT_DIV)
        elif self.curr_char == '+':
            return Token(WT_PLUS)
        elif self.curr_char == '-':
            return Token(WT_MINUS)
        return

    def make_num_tkn(self):
        num = ''
        decimal_tracker = 0

        while self.curr_char is not None and (self.curr_char in DIGITS or self.curr_char == '.'):
            if self.curr_char == '.':
                if decimal_tracker > 0:
                    break
                decimal_tracker += 1
            num += self.curr_char
            self.next_char()
        
        if decimal_tracker == 0:
            return Token(WT_INT, str(num))
        else:
            return Token(WT_FLOAT, float(num))

#######################################
# LINE_POSITION CLASS
#######################################

class LinePosition:
    def __init__(self, ind, line_num, col_num, file_name, file_txt):
        self.ind = ind
        self.line_num = line_num
        self.col_num = col_num
        self.file_name = file_name
        self.file_txt = file_txt
    
    def next(self, curr_char=None):
        self.ind += 1
        self.col_num += 1

        if curr_char == '\n':
            self.line_num += 1
            self.col_num = 0

        return self
    
    def copy_pos(self):
        return LinePosition(self.ind, self.line_num, self.col_num, self.file_name, self.file_txt)
    
#######################################
# NODE CLASSES (for parser)
#######################################

class NumNode:
    def __init__(self, num_tkn):
        self.num_tkn = num_tkn
    
    def __repr__(self):
        return f'{self.num_tkn}'
    
class OpNode:
    def __init__(self, left, op_tkn, right):
        self.left = left
        self.op_tkn = op_tkn
        self.right = right
    
    def __repr__(self):
        return f'ON({self.left}, {self.op_tkn}, {self.right})'

#######################################
# PARSER HANDLER
#######################################

class Parser:
    def __init__(self, tkns):
        self.tkns = tkns
        self.tkn_ind = -1
        self.curr_tkn = None
        self.next()
    
    def next(self):
        self.tkn_ind += 1
        if self.tkn_ind < len(self.tkns):
            self.curr_tkn = self.tkns[self.tkn_ind]
        return self.curr_tkn
    
    
    #expr
    def pm_func(self):
        l = self.md_func()

        while self.curr_tkn.type in (WT_PLUS, WT_MINUS):
            tkn = self.curr_tkn
            self.next()
            r = self.md_func()
            l = OpNode(l, tkn, r)
        
        return l
        
    #term
    def md_func(self):
        l = self.num_func()

        while self.curr_tkn.type in (WT_MUL, WT_DIV):
            tkn = self.curr_tkn
            self.next()
            r = self.num_func()
            l = OpNode(l, tkn, r)
        
        return l

    #factor
    def num_func(self):
        if self.curr_tkn.type == WT_INT or self.curr_tkn == WT_FLOAT:
            num_node = NumNode(self.curr_tkn)
            self.next()
            return num_node
    
    def parse(self):
        return self.pm_func()

#######################################
# RUN HANDLER
#######################################

def run_program(file_name, text):
    new_lexer = Lexer(file_name, text)
    tkns, err = new_lexer.make_tokens()
    if err:
        return None, err

    create_parser = Parser(tkns)
    gen_tree = create_parser.parse()

    return gen_tree, None

#######################################
# ERROR HANDLER
#######################################

class Error:
    def __init__(self, start_pos, end_pos, title, desc):
        self.start_pos = start_pos
        self.end_pos = end_pos
        self.title = title
        self.desc = desc
    
    def string_form(self):
        return f'{self.title}: {self.desc} \n in File {self.start_pos.file_name} \n at line {self.start_pos.line_num + 1}'

class UnsupportedCharacterError(Error):
     def __init__(self, start_pos, end_pos, desc):
         super().__init__(start_pos, end_pos, "UnsupportedCharacterError", desc)

class IllegalSyntaxError(Error):
    def __init__(self, start_pos, end_pos, desc):
        super().__init__(start_pos, end_pos, "IllegalSyntaxError", desc)

