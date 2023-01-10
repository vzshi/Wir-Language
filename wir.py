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
    def __init__(self, text):
        self.text = text
        self.pos = -1
        self.curr_char = None
        self.next_char()
    
    def next_char(self):
        self.pos += 1
        self.curr_char = self.text[self.pos] if self.pos < len(self.text) else None

    def make_tokens(self):
        tkns = []

        while self.curr_char is not None:
            if self.curr_char == ' ' or self.curr_char == '\t':
                self.next_char()
            elif self.curr_char == '(':
                tkns.append(Token(WT_LEFTPAR))
                self.next_char()
            elif self.curr_char == ')':
                tkns.append(Token(WT_RIGHTPAR))
                self.next_char()
            elif self.curr_char == '*':
                tkns.append(Token(WT_MUL))
                self.next_char()
            elif self.curr_char == '/':
                tkns.append(Token(WT_DIV))
                self.next_char()
            elif self.curr_char == '+':
                tkns.append(Token(WT_PLUS))
                self.next_char()
            elif self.curr_char == '-':
                tkns.append(Token(WT_MINUS))
                self.next_char()
            elif self.curr_char in DIGITS:
                tkns.append(self.make_num_tkn())
            else:
                unknown = self.curr_char
                self.next_char()
                return [], IllegalCharacterError(unknown)
        return tkns, None

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
    def __init__(self, ind, line_num, col_num):
        self.ind = ind
        self.line_num = line_num
        self.col_num = col_num
    
    def next(self, curr_char):
        self.ind += 1
        self.col_num += 1

        if curr_char == '\n':
            self.line_num += 1
            self.col_num = 0

    
#######################################
# RUN HANDLER
#######################################

def run_program(text):
    new_lexer = Lexer(text)
    tkns, err = new_lexer.make_tokens()
    return tkns, err

#######################################
# ERROR HANDLER
#######################################

class Error:
    def __init__(self, title, desc):
        self.title = title
        self.desc = desc
    
    def string_form(self):
        return f'{self.title}: {self.desc}'

class IllegalCharacterError(Error):
     def __init__(self, desc):
         super().__init__("IllegalCharacterError", desc)
