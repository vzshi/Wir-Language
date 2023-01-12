import string

#######################################
# TOKEN CLASS
#######################################

WT_INT = 'INT'
WT_FLOAT = 'FLOAT'
WT_LEFTPAR = 'LEFTPAR'
WT_RIGHTPAR = 'RIGHTPAR'
WT_POW = 'POW'
WT_MUL = 'MUL'
WT_DIV = 'DIV'
WT_PLUS = 'PLUS'
WT_MINUS = 'MINUS'
WT_EOF = 'EOF'
WT_KEYWORD = 'KEYWORD'
WT_IDENTIFIER = 'IDENTIFIER'
WT_EQ = 'EQ'

DIGITS = '0123456789'
LETTERS = string.ascii_letters
KEYWORDS = ['Var']
SUPPORTED_CHARS = '()*/+-^='

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
    
    def matches(self, type_, value):
        return self.type == type_ and self.value == value

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
        self.set_context()
    
    def set_pos(self, start_pos=None, end_pos=None):
        self.start_pos = start_pos
        self.end_pos = end_pos
        return self

    def basic_ops(self, other_num, operation):
        if isinstance(other_num, Num):
            if operation == '/' and other_num.value == '0':
                return None, RuntimeError(other_num.start_pos, other_num.end_pos, "Division by Zero", self.context)
            if operation == '^':
                return Num(eval(f'{self.value} ** {other_num.value}')).set_context(self.context), None
            return Num(eval(f'{self.value} {operation} {other_num.value}')).set_context(self.context), None

    def copy_pos(self):
        c = Num(self.value)
        c.set_pos(self.start_pos, self.end_pos)
        c.set_context(self.context)
        return c

    def set_context(self, context=None):
        self.context = context
        return self
    
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
            elif self.curr_char in LETTERS:
                tkns.append(self.make_identifier())
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
        elif self.curr_char == '^':
            return Token(WT_POW, start_pos=self.pos)
        elif self.curr_char == '*':
            return Token(WT_MUL, start_pos=self.pos)
        elif self.curr_char == '/':
            return Token(WT_DIV, start_pos=self.pos)
        elif self.curr_char == '+':
            return Token(WT_PLUS, start_pos=self.pos)
        elif self.curr_char == '-':
            return Token(WT_MINUS, start_pos=self.pos)
        elif self.curr_char == '=':
            return Token(WT_EQ, start_pos=self.pos)
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
    
    def make_identifier(self):
        iden_str = ''
        start_pos = self.pos.copy_pos()

        while self.curr_char is not None and self.curr_char in LETTERS + DIGITS + '_':
            iden_str += self.curr_char
            self.next_char()
        
        tkn_type = WT_KEYWORD if iden_str in KEYWORDS else WT_IDENTIFIER

        return Token(tkn_type, iden_str, start_pos, self.pos)

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

class AssignVNode:
    def __init__(self, var_name_tkn, value_node):
        self.var_name_tkn = var_name_tkn
        self.value_node = value_node
        self.start_pos = var_name_tkn.start_pos
        self.end_pos = value_node.end_pos

class AccessVNode:
    def __init__(self, var_name_tkn):
        self.var_name_tkn = var_name_tkn
        self.start_pos = self.var_name_tkn.start_pos
        self.end_pos = self.var_name_tkn.end_pos


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
    
    def bin_op(self, func1, operations, func2=None):
        result = ParseResult()
        l = result.register(func1())
        if result.err: return result

        while self.curr_tkn.type in operations:
            tkn = self.curr_tkn
            result.register_next()
            self.next()
            r = result.register(func2())
            if result.err: return result
            l = BOpNode(l, tkn, r)
        
        return result.success(l)
    
    #expr
    def pm_func(self):
        result = ParseResult()

        if self.curr_tkn.matches(WT_KEYWORD, 'Var'):
            result.register_next()
            self.next()
            if self.curr_tkn.type != WT_IDENTIFIER:
                return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "Identifier Expected"))
            var_name = self.curr_tkn
            result.register_next()
            self.next()
            if self.curr_tkn.type != WT_EQ:
                return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'=' Expected"))
            result.register_next()
            self.next()
            expression = result.register(self.pm_func())
            if result.err: return result
            return result.success(AssignVNode(var_name, expression))

        new_node = result.register(self.bin_op(self.md_func, (WT_PLUS, WT_MINUS), self.md_func))

        if result.err:
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "Expected int, float, identifier, Var, '+', '-', or '('"))

        return result.success(new_node)
        
    #term
    def md_func(self):
        return self.bin_op(self.num_func, (WT_MUL, WT_DIV), self.num_func)

    #factor
    def num_func(self):
        result = ParseResult()
        tkn = self.curr_tkn

        if tkn.type in (WT_PLUS, WT_MINUS):
            result.register_next()
            self.next()
            num_node = result.register(self.num_func())
            if result.err: return result
            return result.success(UOpNode(tkn, num_node))

        return self.power()
    
    def power(self):
        result = ParseResult()
        l = result.register(self.pow_func())
        if result.err: return result

        while self.curr_tkn.type in (WT_POW, ):
            tkn = self.curr_tkn
            result.register_next()
            self.next()
            r = result.register(self.num_func())
            if result.err: return result
            l = BOpNode(l, tkn, r)
        
        return result.success(l)

    #atom
    def pow_func(self):
        result = ParseResult()
        tkn = self.curr_tkn

        if self.curr_tkn.type == WT_INT or self.curr_tkn == WT_FLOAT:
            result.register_next()
            self.next()
            return result.success(NumNode(tkn))

        elif tkn.type == WT_IDENTIFIER:
            result.register_next()
            self.next()
            return result.success(AccessVNode(tkn))

        elif tkn.type == WT_LEFTPAR:
            result.register_next()
            self.next()
            pm_node = result.register(self.pm_func())
            if result.err: return result
            if self.curr_tkn.type == WT_RIGHTPAR:
                result.register_next()
                self.next()
                return result.success(pm_node)
            else:
                return result.failure(IllegalSyntaxError(
                    self.curr_tkn.start_pos, self.curr_tkn.end_pos, 
                    "Expected ')'"
                    ))

        return result.failure(IllegalSyntaxError(
                    tkn.start_pos, tkn.end_pos, 
                    "Expected int, float, identifier, '+', '-', or '('"
                    ))

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
        self.next_count = 0

    def register(self, res):
        self.next_count += res.next_count
        if res.err: self.err = res.err
        return res.pr_node

    def register_next(self):
        self.next_count += 1

    def success(self, pr_node):
        self.pr_node = pr_node
        return self

    def failure(self, err):
        if not self.err or self.next_count == 0:
            self.err = err
        return self

#######################################
# INTERPRETER HANDLER
#######################################

class Interpreter:
    def visit(self, node, context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit)
        return method(node, context)
    
    def no_visit(self, node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined')
    
    def visit_AccessVNode(self, node, context):
        result = RuntimeResult()
        var_name = node.var_name_tkn.value
        value = context.symbol_table.get(var_name)

        if not value:
            return result.failure(RuntimeError(node.start_pos, node.end_pos, f"'{var_name} is not defined", context))
        
        value = value.copy_pos().set_pos(node.start_pos, node.end_pos)
        return result.success(value)
    
    def visit_AssignVNode(self, node, context):
        result = RuntimeResult()
        var_name = node.var_name_tkn.value
        value = result.register(self.visit(node.value_node, context))
        if result.err: return result

        context.symbol_table.set(var_name, value)
        return result.success(value)


    def visit_NumNode(self, node, context):
        return RuntimeResult().success(
            Num(node.num_tkn.value).set_context(context).set_pos(
            node.start_pos, node.end_pos))

    def visit_BOpNode(self, node, context):
        rs = RuntimeResult()
        l = rs.register(self.visit(node.left, context))
        if rs.err: return rs
        r = rs.register(self.visit(node.right, context))
        if rs.err: return rs

        result, err = None, None

        if node.op_tkn.type == WT_PLUS:
            result, err = l.basic_ops(r, '+')
        elif node.op_tkn.type == WT_MINUS:
            result, err = l.basic_ops(r, '-')
        elif node.op_tkn.type == WT_MUL:
            result, err = l.basic_ops(r, '*')
        elif node.op_tkn.type == WT_DIV:
            result, err = l.basic_ops(r, '/')
        elif node.op_tkn.type == WT_POW:
            result, err = l.basic_ops(r, '^') 

        if err:
            return rs.failure(err)
        else:
            return rs.success(result.set_pos(node.start_pos, node.end_pos))

    def visit_UOpNode(self, node, context):
        rs = RuntimeResult()
        number = rs.register(self.visit(node.node, context))
        if rs.err: return rs

        err = None

        if node.op_tkn.type == WT_MINUS:
            number, err = number.basic_ops(Num(-1), '*')

        if err:
            return rs.failure(err)
        else:
            return rs.success(number.set_pos(node.start_pos, node.end_pos))

#######################################
# SYMBOL TABLE CLASS
#######################################

class SymbolTable:
    def __init__(self):
        self.symbols = {}

        #keeps track of global symbol table
        self.parent = None
    
    def get(self, name):
        value = self.symbols.get(name, None)
        if not value and self.parent:
            return self.parent.get(name)
        return value
    
    def set(self, name, value):
        self.symbols[name] = value
    
    def remove(self, name):
        del self.symbols[name]

#######################################
# RUN HANDLER
#######################################

global_sym_table = SymbolTable()
global_sym_table.set("null", Num(0))

def run_program(file_name, text):
    new_lexer = Lexer(file_name, text)
    tkns, err = new_lexer.make_tokens()
    
    if err: return None, err

    create_parser = Parser(tkns)
    gen_tree = create_parser.parse()

    if gen_tree.err: return None, gen_tree.err

    interp = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_sym_table
    result = interp.visit(gen_tree.pr_node, context)

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
    def __init__(self, start_pos, end_pos, desc, context):
        super().__init__(start_pos, end_pos, "Runtime Error", desc)
        self.context = context
    
    def string_form(self):
        result = self.generate_traceback()
        result += f'{self.title}: {self.desc}'
        return result
    
    def generate_traceback(self):
        result = ''
        pos = self.start_pos
        context = self.context

        while context:
            result = f'\tIn File {pos.file_name}, at line {str(pos.line_num + 1)}, in {context.display_name} \n' + result
            pos = context.parent_entry_pos
            context = context.parent

        return 'Traceback (most recent call last):\n' + result


#######################################
# CONTEXT CLASS (for errors)
#######################################

class Context:
    def __init__(self, display_name, parent=None, parent_entry_pos=None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None