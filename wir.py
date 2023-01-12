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
WT_EOF = 'EOF'

DIGITS = '0123456789'
SUPPORTED_CHARS = '()*/+-'

class Token:
    def __init__(self, wir_type, value=None, start_pos=None, end_pos=None):
        self.type = wir_type
        self.value = value
        
        if start_pos:
            self.start_pos = start_pos.copy_pos()
            self.end_pos = start_pos.copy_pos()
            self.end_pos.next()
        
        if end_pos:
            self.end_pos = end_pos.copy_pos()
    
    def __repr__(self):
        if self.value:
            return f'{self.type}: {self.value}'
        else:
            return f'{self.type}'

#######################################
# VALUES CLASSES
#######################################

class Num:
    def __init__(self, value):
        self.value = value
        self.set_pos()
    
    def set_pos(self, start_pos=None, end_pos=None):
        self.start_pos = start_pos
        self.end_pos = end_pos
        return self

    def basic_ops(self, other_num, operation):
        if isinstance(other_num, Num):
            if operation == '/':
                if other_num.value == 0:
                    return None, RuntimeError(other_num.start_pos, other_num.end_pos
                                            "Division by Zero")
            return Num(eval(f'{self.value} {operation} {other_num.value}')), None
        
    def __repr__(self):
        return str(self.value)

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
        
        tkns.append(Token(WT_EOF, start_pos=self.pos))
        return tkns, None

    def make_op_tkn(self):
        if self.curr_char == '(':
            return Token(WT_LEFTPAR, start_pos=self.pos)
        elif self.curr_char == ')':
            return Token(WT_RIGHTPAR, start_pos=self.pos)
        elif self.curr_char == '*':
            return Token(WT_MUL, start_pos=self.pos)
        elif self.curr_char == '/':
            return Token(WT_DIV, start_pos=self.pos)
        elif self.curr_char == '+':
            return Token(WT_PLUS, start_pos=self.pos)
        elif self.curr_char == '-':
            return Token(WT_MINUS, start_pos=self.pos)
        return

    def make_num_tkn(self):
        num = ''
        decimal_tracker = 0
        start_pos = self.pos.copy_pos()

        while self.curr_char is not None and (self.curr_char in DIGITS or self.curr_char == '.'):
            if self.curr_char == '.':
                if decimal_tracker > 0:
                    break
                decimal_tracker += 1
            num += self.curr_char
            self.next_char()
        
        if decimal_tracker == 0:
            return Token(WT_INT, str(num), start_pos, self.pos)
        else:
            return Token(WT_FLOAT, float(num), start_pos, self.pos)

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
        self.start_pos = self.num_tkn.start_pos
        self.end_pos = self.num_tkn.end_pos
    
    def __repr__(self):
        return f'{self.num_tkn}'
    
class BOpNode:
    def __init__(self, left, op_tkn, right):
        self.left = left
        self.op_tkn = op_tkn
        self.right = right
        self.start_pos = self.left.start_pos
        self.end_pos = self.right.end_pos
    
    def __repr__(self):
        return f'BON({self.left}, {self.op_tkn}, {self.right})'

class UOpNode:
    def __init__(self, op_tkn, node):
        self.op_tkn = op_tkn
        self.node = node
        self.start_pos = self.op_tkn.start_pos
        self.end_pos = self.op_tkn.end_pos

    def __repr__(self):
        return f'UON({self.op_tkn}, {self.node})'

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
        result = ParseResult()
        l = result.register(self.md_func())
        if result.err: return result

        while self.curr_tkn.type in (WT_PLUS, WT_MINUS):
            tkn = self.curr_tkn
            result.register(self.next())
            r = result.register(self.md_func())
            if result.err: return result
            l = BOpNode(l, tkn, r)
        
        return result.success(l)
        
    #term
    def md_func(self):
        result = ParseResult()
        l = result.register(self.num_func())
        if result.err: return result

        while self.curr_tkn.type in (WT_MUL, WT_DIV):
            tkn = self.curr_tkn
            result.register(self.next())
            r = result.register(self.num_func())
            if result.err: return result
            l = BOpNode(l, tkn, r)
        
        return result.success(l)

    #factor
    def num_func(self):
        result = ParseResult()
        tkn = self.curr_tkn

        if tkn.type in (WT_PLUS, WT_MINUS):
            result.register(self.next())
            num_node = result.register(self.num_func())
            if result.err: return result
            return result.success(UOpNode(tkn, num_node))
        
        elif self.curr_tkn.type == WT_INT or self.curr_tkn == WT_FLOAT:
            result.register(self.next())
            return result.success(NumNode(tkn))

        elif tkn.type == WT_LEFTPAR:
            result.register(self.next())
            pm_node = result.register(self.pm_func())
            if result.err: return result
            if self.curr_tkn.type == WT_RIGHTPAR:
                result.register(self.next())
                return result.success(pm_node)
            else:
                return result.failure(IllegalSyntaxError(
                    self.curr_tkn.start_pos, self.curr_tkn.end_pos, 
                    "Expected ')'"
                    ))

        return result.failure(IllegalSyntaxError(tkn.start_pos, tkn.end_pos, "Expected Int or Float"))
    
    def parse(self):
        result = self.pm_func()
        if not result.err and self.curr_tkn.type is not WT_EOF:
            result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "Expected '+', '-', '*', or '/'"))
        return result

#######################################
# PARSE RESULT HANDLER
#######################################

class ParseResult:
    def __init__(self):
        self.err = None
        self.pr_node = None

    def register(self, res):
        if isinstance(res, ParseResult):
            if res.err: self.err = res.err
            return res.pr_node
        
        return res

    def success(self, pr_node):
        self.pr_node = pr_node
        return self

    def failure(self, err):
        self.err = err
        return self

#######################################
# INTERPRETER HANDLER
#######################################

class Interpreter:
    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit)
        return method(node)
    
    def no_visit(self, node):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    def visit_NumNode(self, node):
        rs = RuntimeResult()
        return rs.success(
            Num(node.num_tkn.value).set_pos(
            node.start_pos, node.end_pos))

    def visit_BOpNode(self, node):
        rs = RuntimeResult()
        l = rs.register(self.visit(node.left))
        if rs.err: return rs
        r = rs.register(self.visit(node.right))
        if rs.err: return rs

        if node.op_tkn.type == WT_PLUS:
            result, err = l.basic_ops(r, '+')
        elif node.op_tkn.type == WT_MINUS:
            result, err = l.basic_ops(r, '-')
        elif node.op_tkn.type == WT_MUL:
            result, err = l.basic_ops(r, '*')
        elif node.op_tkn.type == WT_DIV:
            result, err = l.basic_ops(r, '/')

        if err:
            return rs.failure(err)
        
        return rs.success(result.set_pos(node.start_pos, node.end_pos))

    def visit_UOpNode(self, node):
        rs = RuntimeResult()
        number = rs.register(self.visit(node.node))
        if rs.err: return rs

        err = None

        if node.op_tkn.type == WT_MINUS:
            number, err = number.basic_ops(Num(-1), '*')

        if err:
            return rs.failure(err)
        
        return rs.success(number.set_pos(node.start_pos, node.end_pos))

#######################################
# RUN HANDLER
#######################################

def run_program(file_name, text):
    new_lexer = Lexer(file_name, text)
    tkns, err = new_lexer.make_tokens()
    
    if err: return None, err

    create_parser = Parser(tkns)
    gen_tree = create_parser.parse()

    if gen_tree.err: return None, gen_tree.err

    interp = Interpreter()
    result = interp.visit(gen_tree.pr_node)

    return result.value, result.err

#######################################
# RUNTIME RESULT CLASS 
#######################################

class RuntimeResult:
    def __init__(self):
        self.value = None
        self.err = None
    
    def register(self, result):
        if result.err: self.err = result.err
        return result.value
    
    def success(self, value):
        self.value = value
        return self
    
    def failure(self, err):
        self.err = err
        return self

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
         super().__init__(start_pos, end_pos, "Unsupported Character", desc)

class IllegalSyntaxError(Error):
    def __init__(self, start_pos, end_pos, desc):
        super().__init__(start_pos, end_pos, "Illegal Syntax", desc)

class RuntimeError(Error):
    def __init__(self, start_pos, end_pos, desc):
        super().__init__(start_pos, end_pos, "Runtime Error", desc)


