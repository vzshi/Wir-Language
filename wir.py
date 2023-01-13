import string
import os
import math

#######################################
# TOKEN CLASS
#######################################

WT_INT = 'INT'
WT_FLOAT = 'FLOAT'
WT_LEFTPAR = 'LEFTPAR'
WT_RIGHTPAR = 'RIGHTPAR'
WT_LEFTBRACKET = 'LEFTBRACKET'
WT_RIGHTBRACKET = 'RIGHTBRACKET'
WT_POW = 'POW'
WT_MUL = 'MUL'
WT_DIV = 'DIV'
WT_PLUS = 'PLUS'
WT_MINUS = 'MINUS'
WT_EOF = 'EOF'
WT_KEYWORD = 'KEYWORD'
WT_IDENTIFIER = 'IDENTIFIER'
WT_EQ = 'EQ'
WT_EQUALS = 'EQUALS'
WT_NE = 'NE'
WT_LT = 'LT'
WT_GT = 'GT'
WT_LTE = 'LTE'
WT_GTE = 'GTE'
WT_COMMA = 'COMMA'
WT_ARROW = 'ARROW'
WT_STRING = 'STRING'

DIGITS = '0123456789'
LETTERS = string.ascii_letters
KEYWORDS = ['Var', 'And', 'Or', 'Not', 
            'If', 'Then', 'Otherwise', 'Else',
            'For', 'To', 'Inc', 'Do', 'While', 
            'Func']
SUPPORTED_CHARS = '()*/+-^=<>!,[]'

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

class Value:
    def __init__(self):
        self.set_pos()
        self.set_context()
    
    def set_pos(self, start_pos=None, end_pos=None):
        self.start_pos = start_pos
        self.end_pos = end_pos

        return self
    
    def set_context(self, context=None):
        self.context = context

        return self
    
    def basic_ops(self, other, operation):
        return None, self.illegal_op(other)

    def comp_ops(self, other, comparison):
        return None, self.illegal_op(other)

    def is_true(self):
        return False

    def notted(self, other=None):
        return None, self.illegal_op(other)

    def copy_pos(self):
        raise Exception("No 'copy_pos' method defined")
    
    def illegal_op(self, other=None):
        if not other: other = self
        return RuntimeError(self.start_pos, self.end_pos, 'Illegal Operation', self.context)

class Num(Value):
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
        else:
            return None, Value.illegal_op(self, other_num)
            
    def comp_ops(self, other_num, comparison):
        if isinstance(other_num, Num):
            return Num(eval(f'{self.value} {comparison} {other_num.value}')).set_context(self.context), None
        else:
            return None, Value.illegal_op(self, other_num)

    def is_true(self):
        return self.value

    def notted(self):
        return Num(True if self.value == '0' or self.value == 0 else False).set_context(self.context), None

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
    
Num.null = Num(0)
Num.true = Num(1)
Num.false = Num(0)
Num.pi = Num(math.pi)

class FunctionSkeleton(Value):
    def __init__(self, name):
        super().__init__()
        self.name = name or '<none>'

    def gen_new_context(self):
        new_con = Context(self.name, self.context, self.start_pos)
        new_con.symbol_table = SymbolTable(new_con.parent.symbol_table)
        return new_con
    
    def check_num_args(self, arg_names, args):
        result = RuntimeResult()

        if len(args) > len(arg_names):
            return result.failure(RuntimeError(self.start_pos, self.end_pos, f"{len(args) - len(arg_names)} less args needs to be passed into '{self.name}'", self.context))
            
        elif len(args) < len(arg_names):
            return result.failure(RuntimeError(self.start_pos, self.end_pos, f"{len(arg_names) - len(args)} more args needs to be passed into '{self.name}'", self.context))

        return result.success(None)

    def popu_args(self, arg_names, args, top_cont):
        for i in range(len(args)):
            arg_title = args_names[i]
            arg_val = args[i]
            arg_val.set_context(top_cont)
            top_cont.symbol_table.set(arg_title, arg_val)

    def check_and_pop_args(self, arg_names, args, top_cont):
        result = RuntimeResult()
        result.register(self.check_num_args(arg_names, args))
        if result.err: return result

        self.popu_args(arg_names, args, top_cont)
        return result.success(None)

class BuiltInFunction(FunctionSkeleton):
    def __init__(self, name):
        super().__init__(name)

    def run(self, args):
        result = RuntimeResult()
        top_cont = self.gen_new_context()

        func_name = f'run_{self.name}'
        func = getattr(self, func_name, self.no_visit)

        result.register(self.check_and_pop_args(func.arg_names, args, top_cont))
        if result.err: return result

        ret_val = result.register(method(top_cont))
        if result.err: return result

        return result.success(ret_val)
    
    def no_visit(self, node, context):
        raise Exception("No run_{self.name} method found")
    
    def copy_pos(self):
        c = BuiltInFunction(self.name)
        c.set_pos(self.start_pos, self.end_pos)
        c.set_context(self.context)
        return c

    def __repr__(self):
        return f"<Built-In Func {self.name}"
    
    def run_print(self, top_cont):
        print(str(top_cont.symbol_table.get('value')))
        return RuntimeResult().success(Num.null)
    run_print.arg_names = ['value']

    def run_print_and_ret(self, top_cont):
        return RuntimeResult().success(String(str(top_cont.symbol_table.get('value'))))
    run_print.arg_names = ['value']

    def run_input(self, top_cont):
        text = input()
        return RuntimeResult().success(String(text))
    run_input.arg_names = []

    def run_input(self, top_cont):
        text = input()
        return RuntimeResult().success(String(text))
    run_input.arg_names = []

    def run_input_to_int(self, top_cont):
        while True:
            text = input()
            try:
                num = int(text)
                break
            except ValueError:
                print(f"'{text}' must be an integer")
        return RuntimeResult().success(Num(num))
    run_input.arg_names = []

    def run_clear(self, top_cont):
        os.system('clear')
        return RuntimeResult().success(Num.null)
    run_clear.arg_names = []

    def run_is_num(self, top_cont):
        is_num = isinstance(top_cont.symbol_table.get("value"), Num)
        return RuntimeResult().success(Num.true if is_num else Num.false)
    run_is_num.arg_names = ["value"]

    def run_is_str(self, top_cont):
        is_str = isinstance(top_cont.symbol_table.get("value"), String)
        return RuntimeResult().success(Num.true if is_str else Num.false)
    run_is_str.arg_names = ["value"]

    def run_is_lst(self, top_cont):
        is_lst = isinstance(top_cont.symbol_table.get("value"), List)
        return RuntimeResult().success(Num.true if is_lst else Num.false)
    run_is_lst.arg_names = ["value"]

    def run_is_func(self, top_cont):
        is_func = isinstance(top_cont.symbol_table.get("value"), FunctionSkeleton)
        return RuntimeResult().success(Num.true if is_func else Num.false)
    run_is_func.arg_names = ["value"]

    def run_append(self, top_cont):
        lst = top_cont.symbol_table.get('list')
        val = top_cont.symbol_table.get('value')

        if not isinstance(lst, List):
            return RuntimeResult().failure(RuntimeError(self.start_pos, self.end_pos, "First argument must be type List"), top_cont)
        
        lst.elements.append(value)
        return RuntimeResult().success(Num.null)
    run_append.arg_names = ['list', 'value']

    def run_append(self, top_cont):
        lst = top_cont.symbol_table.get('list')
        ind = top_cont.symbol_table.get('index')

        if not isinstance(lst, List):
            return RuntimeResult().failure(RuntimeError(self.start_pos, self.end_pos, "First argument must be type List"), top_cont)
        
        if not isinstance(ind, Num):
            return RuntimeResult().failure(RuntimeError(self.start_pos, self.end_pos, "Second argument must be type Num"), top_cont)
        
        try:
            popped = lst.elements.pop(int(ind.value))
        except:
            return RuntimeResult().failure(RuntimeError(self.start_pos, self.end_pos, 'Index is out of bounds'), top_cont)

        return RuntimeResult().success(popped)
    run_append.arg_names = ['list', 'index']

    def run_extend(self, top_cont):
        lst1 = top_cont.symbol_table.get("lst1")
        lst2 = top_cont.symbol_table.get("lst2")

        if not isinstance(lst1, List):
            return RuntimeResult().failure(RuntimeError(self.start_pos, self.end_pos, "First argument must be type List"), top_cont)
        
        if not isinstance(lst2, List):
            return RuntimeResult().failure(RuntimeError(self.start_pos, self.end_pos, "Second argument must be type List"), top_cont)
        
        lst1.elements.extend(lst2.elements)
        return RuntimeResult().success(Num.null)
    run_extend.arg_names = ['lst1', 'lst2']

BuiltInFunction.print = BuiltInFunction('print')
BuiltInFunction.print_and_ret = BuiltInFunction('print_and_ret')
BuiltInFunction.input = BuiltInFunction('input')
BuiltInFunction.input_to_int = BuiltInFunction('input_to_int')
BuiltInFunction.clear = BuiltInFunction('clear')
BuiltInFunction.is_num = BuiltInFunction('is_num')
BuiltInFunction.is_str = BuiltInFunction('is_str')
BuiltInFunction.is_lst = BuiltInFunction('is_lst')
BuiltInFunction.is_func = BuiltInFunction('is_func')
BuiltInFunction.append = BuiltInFunction('append')
BuiltInFunction.pop = BuiltInFunction('pop')
BuiltInFunction.extend = BuiltInFunction('extend')

class Function(FunctionSkeleton):
    def __init__(self, name, body, arg_names):
        super().__init__(name)
        self.body = body
        self.arg_names = arg_names
    
    def run(self, args):
        result = RuntimeResult()
        interp = Interpreter()
        context = self.gen_new_context()
        
        result.register(self.check_and_pop_args(self.arg_names, args, context))
        if result.err: return result
        
        val = result.register(interp.visit(self.body, context))
        if result.err: return result

        return result.success(val)

    def copy_pos(self):
        c = Function(self.name, self.body, self.arg_names)
        c.set_pos(self.start_pos, self.end_pos)
        c.set_context(self.context)
        return c
    
    def __repr__(self):
        return f'<Func {self.name}>'

class String(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value
    
    def basic_ops(self, other, operation):
        if isinstance(other, String) and operation == '+':
            return String(self.value + other.value).set_context(self.context), None
        elif isinstance(other, Num) and operation == '*':
            return String(self.value * int(other.value)).set_context(self.context), None
        return None, Value.illegal_op(self, other)
    
    def is_true(self):
        return len(self.value) > 0

    def copy_pos(self):
        c = String(self.value)
        c.set_pos(self.start_pos, self.end_pos)
        c.set_context(self.context)
        return c
    
    def __repr__(self):
        return f'"{self.value}"'

class List(Value):
    def __init__(self, elements):
        super().__init__()
        self.elements = elements
    
    def basic_ops(self, other, operation):
        if operation == '+':
            copy_lst = self.copy_pos()
            copy_lst.elements.append(other)
            return copy_lst, None
        elif operation == '-':
            if isinstance(other, Number):
                copy_lst = self.copy_pos()
                try:
                    copy_lst.elements.pop(int(other.value))
                except:
                    return None, RuntimeError(other.start_pos, other.end_pos, "Given index is out of bounds", self.context)
            else:
                None, Value.illegal_op(self, other)
        elif operation == '*':
            if isinstance(other, List):
                copy_lst = self.copy_pos()
                copy_lst.elements.extend(other.elements)
                return copy_lst, None
            else:
                return None, Value.illegal_op(self, other)
        elif operation == '^':
            if isinstance(other, Num):
                copy_lst = self.copy_pos()
                try:
                    return copy_lst.elements[int(other.value)], None
                except:
                    return None, RuntimeError(other.start_pos, other.end_pos, "Given index is out of bounds", self.context)
            else:
                None, Value.illegal_op(self, other)
    
    def copy_pos(self):
        c = List(self.elements)
        c.set_pos(self.start_pos, self.end_pos)
        c.set_context(self.context)
        return c
    
    def __repr__(self):
        return f'[{", ".join([str(el) for el in self.elements])}]'

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
                curr_char_tkn, err = self.make_op_tkn()
                if err:
                    return [], err
                tkns.append(curr_char_tkn)
                self.next_char()
            elif self.curr_char in DIGITS:
                tkns.append(self.make_num_tkn())
            elif self.curr_char in LETTERS:
                tkns.append(self.make_identifier())
            elif self.curr_char == '"':
                tkns.append(self.make_string_tkn())
            else:
                start_pos = self.pos.copy_pos()
                unknown = self.curr_char
                self.next_char()
                return [], UnsupportedCharacterError(start_pos, self.pos, unknown)
        
        tkns.append(Token(WT_EOF, start_pos=self.pos))
        return tkns, None

    def make_string_tkn(self):
        string = ''
        start_pos = self.pos.copy_pos()
        esc_char = False
        self.next_char()

        esc_chars = {'n': '\n', 't': '\t'}

        while self.curr_char is not None and (self.curr_char != '"' or esc_char):
            if esc_char:
                string += esc_chars.get(self.curr_char, self.curr_char)
                esc_char = False
            else:
                if self.curr_char == '\\':
                    esc_char = True
                else:
                    string += self.curr_char
            self.next_char()

        self.next_char()
        return Token(WT_STRING, string, start_pos, self.pos)

    def make_op_tkn(self):
        if self.curr_char == '(':
            return Token(WT_LEFTPAR, start_pos=self.pos), None
        elif self.curr_char == ')':
            return Token(WT_RIGHTPAR, start_pos=self.pos), None
        elif self.curr_char == '[':
            return Token(WT_LEFTBRACKET, start_pos=self.pos), None
        elif self.curr_char == ']':
            return Token(WT_RIGHTBRACKET, start_pos=self.pos), None
        elif self.curr_char == '^':
            return Token(WT_POW, start_pos=self.pos), None
        elif self.curr_char == '*':
            return Token(WT_MUL, start_pos=self.pos), None
        elif self.curr_char == '/':
            return Token(WT_DIV, start_pos=self.pos), None
        elif self.curr_char == '+':
            return Token(WT_PLUS, start_pos=self.pos), None
        elif self.curr_char == '-':
            return Token(WT_MINUS, start_pos=self.pos), None
        elif self.curr_char == ',':
            return Token(WT_COMMA, start_pos=self.pos), None
        elif self.curr_char == '!':
            tkn, err = self.make_not_equals()
            if err: return [], err
            return tkn, None
        elif self.curr_char == '=':
            return self.make_equals()
        elif self.curr_char == '<':
            return self.make_lt()
        elif self.curr_char == '>':
            return self.make_gt()

        return None, None
    
    def make_not_equals(self):
        start_pos = self.pos.copy_pos()
        self.next_char()

        if self.curr_char == '=':
            self.next_char()
            return Token(WT_NE, start_pos=start_pos, end_pos=self.pos), None
        
        self.next_char()
        return None, ExpectedCharError(start_pos, self.pos, "'=' Expected (after '!')")

    def make_equals(self):
        tkn_type = WT_EQ
        start_pos = self.pos.copy_pos()
        self.next_char()

        if self.curr_char == '=':
            self.next_char()
            tkn_type = WT_EQUALS
        
        elif self.curr_char == '>':
            self.next_char()
            tkn_type = WT_ARROW
        
        return Token(tkn_type, start_pos=start_pos, end_pos=self.pos), None

    def make_lt(self):
        tkn_type = WT_LT
        start_pos = self.pos.copy_pos()
        self.next_char()

        if self.curr_char == '=':
            self.next_char()
            tkn_type = WT_LTE
        
        return Token(tkn_type, start_pos=start_pos, end_pos=self.pos), None

    def make_gt(self):
        tkn_type = WT_GT
        start_pos = self.pos.copy_pos()
        self.next_char()

        if self.curr_char == '=':
            self.next_char()
            tkn_type = WT_GTE
        
        return Token(tkn_type, start_pos=start_pos, end_pos=self.pos), None

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

class StringNode:
    def __init__(self, str_tkn):
        self.str_tkn = str_tkn
        self.start_pos = self.str_tkn.start_pos
        self.end_pos = self.str_tkn.end_pos
    
    def __repr__(self):
        return f'{self.num_tkn}'

class ListNode:
    def __init__(self, elements, start_pos, end_pos):
        self.elements = elements
        self.start_pos = start_pos
        self.end_pos = end_pos
    
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

class IfNode:
    def __init__(self, cases, else_case):
        self.cases = cases
        self.else_case = else_case
        self.start_pos = self.cases[0][0].start_pos
        self.end_pos = (self.else_case or self.cases[len(self.cases) - 1][0]).end_pos

class ForNode:
    def __init__(self, var_name, start_val, end_val, inc, body):
        self.var_name = var_name
        self.start_val = start_val
        self.end_val = end_val
        self.inc = inc
        self.body = body
        self.start_pos = self.var_name.start_pos
        self.end_pos = self.body.end_pos

class WhileNode:
    def __init__(self, cond, body):
        self.cond = cond
        self.body = body
        self.start_pos = self.cond.start_pos
        self.end_pos = self.body.end_pos

class FuncNode:
    def __init__(self, var_name, args, body):
        self.var_name = var_name
        self.args = args
        self.body = body

        if self.var_name:
            self.start_pos = self.var_name.start_pos
        elif len(self.args) > 0:
            self.start_pos = self.args[0].start_pos
        else:
            self.start_pos = body.start_pos
        
        self.end_pos = self.body.end_pos

class CallNode:
    def __init__(self, called_node, args):
        self.called_node = called_node
        self.args = args
        self.start_pos = self.called_node.start_pos

        if len(self.args) > 0:
            self.end_pos = self.args[len(self.args) - 1].end_pos
        else:
            self.end_pos = self.called_node.end_pos

        

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

        while self.curr_tkn.type in operations or (self.curr_tkn.type, self.curr_tkn.value) in operations:
            tkn = self.curr_tkn
            result.register_next()
            self.next()
            r = result.register(func2())
            if result.err: return result
            l = BOpNode(l, tkn, r)
        
        return result.success(l)
    
    def comp_func(self):
        result = ParseResult()

        if self.curr_tkn.matches(WT_KEYWORD, 'Not'):
            operation_tkn = self.curr_tkn
            result.register_next()
            self.next()
            node = result.register(self.comp_func())
            if result.err: return result
            return result.success(UOpNode(operation_tkn, node))
        
        node = result.register(self.bin_op(self.arith_func, (WT_EQUALS, WT_NE, WT_LT, WT_GT, WT_LTE, WT_GTE), self.arith_func))
    
        if result.err:
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "Expected int, float, identifier, 'Not', '+', '-', '[', or '('"))

        return result.success(node)

    def arith_func(self):
        return self.bin_op(self.md_func, (WT_PLUS, WT_MINUS), self.md_func)

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

        new_node = result.register(self.bin_op(self.comp_func, ((WT_KEYWORD, "And"), (WT_KEYWORD, "Or")), self.comp_func))

        if result.err:
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "Expected int, float, identifier, 'Var', 'If', 'For', 'While', 'Func' '+', '-', '[', or '('"))

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
        return self.bin_op(self.call, (WT_POW, ), self.num_func)
    
    def call(self):
        result = ParseResult()
        curr_atom = result.register(self.pow_func())
        if result.err: return result

        if self.curr_tkn.type == WT_LEFTPAR:
            result.register_next()
            self.next()
            args = []

            if self.curr_tkn.type == WT_RIGHTPAR:
                result.register_next()
                self.next()
            else:
                args.append(result.register(self.pm_func()))
                if result.err:
                    return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "')', 'Var', 'For', 'While', 'Func', int, float, identifier, '+', '-', '[', or '(' Expected"))

                while self.curr_tkn.type == WT_COMMA:
                    result.register_next()
                    self.next()

                    args.append(result.register(self.pm_func()))
                    if result.err: return result
                
                if self.curr_tkn.type != WT_RIGHTPAR:
                    return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "',' or ')' Expected"))

                result.register_next()
                self.next()
            
            return result.success(CallNode(curr_atom, args))
        return result.success(curr_atom)

    #atom
    def pow_func(self):
        result = ParseResult()
        tkn = self.curr_tkn

        if self.curr_tkn.type == WT_INT or self.curr_tkn == WT_FLOAT:
            result.register_next()
            self.next()
            return result.success(NumNode(tkn))

        elif self.curr_tkn.type == WT_STRING:
            result.register_next()
            self.next()
            return result.success(StringNode(tkn))

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
                
        elif tkn.type == WT_LEFTBRACKET:
            lst = result.register(self.list_func())
            if result.err: return result
            return result.success(lst)

        elif tkn.matches(WT_KEYWORD, 'If'):
            expr = result.register(self.if_expr())
            if result.err: return result
            return result.success(expr)

        elif tkn.matches(WT_KEYWORD, 'For'):
            expr = result.register(self.for_expr())
            if result.err: return result
            return result.success(expr)
        
        elif tkn.matches(WT_KEYWORD, 'While'):
            expr = result.register(self.while_expr())
            if result.err: return result
            return result.success(expr)
        
        elif tkn.matches(WT_KEYWORD, 'Func'):
            expr = result.register(self.func_dec())
            if result.err: return result
            return result.success(expr)

        return result.failure(IllegalSyntaxError(
                    tkn.start_pos, tkn.end_pos, 
                    "Expected int, float, identifier, 'If', 'For', 'While', 'Func', '+', '-', '[' or '('"
                    ))
    
    def if_expr(self):
        result = ParseResult()
        total_cases = []
        else_case = None

        if not self.curr_tkn.matches(WT_KEYWORD, 'If'):
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'If' Expected"))
        
        result.register_next()
        self.next()

        cond = result.register(self.pm_func())
        if result.err: return result

        if not self.curr_tkn.matches(WT_KEYWORD,'Then'):
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'Then' Expected"))

        result.register_next()
        self.next()

        expr = result.register(self.pm_func())
        if result.err: return result
        total_cases.append((cond, expr))

        while self.curr_tkn.matches(WT_KEYWORD, 'Otherwise'):
            result.register_next()
            self.next()

            cond = result.register(self.pm_func())
            if result.err: return result

            if not self.curr_tkn.matches(WT_KEYWORD, 'Then'):
                return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'Then' Expected"))

            result.register_next()
            self.next()

            expr = result.register(self.pm_func())
            if result.err: return result
            total_cases.append((cond, expr))
        
        if self.curr_tkn.matches(WT_KEYWORD, 'Else'):
            result.register_next()
            self.next()

            expr = result.register(self.pm_func())
            if result.err: return result
            else_case = expr
        
        return result.success(IfNode(total_cases, else_case))

    def for_expr(self):
        result = ParseResult()

        if not self.curr_tkn.matches(WT_KEYWORD, 'For'):
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'For' Expected"))

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

        start_val = result.register(self.pm_func())
        if result.err: return result

        if not self.curr_tkn.matches(WT_KEYWORD, 'To'):
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'To' Expected"))

        result.register_next()
        self.next()

        end_val = result.register(self.pm_func())
        if result.err: return result

        if self.curr_tkn.matches(WT_KEYWORD, 'Inc'):
            result.register_next()
            self.next()

            inc_val = result.register(self.pm_func())
            if result.err: return result
        else:
            inc_val = None
        
        if not self.curr_tkn.matches(WT_KEYWORD, 'Do'):
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'Do' Expected"))

        result.register_next()
        self.next()

        body = result.register(self.pm_func())
        if result.err: return result

        return result.success(ForNode(var_name, start_val, end_val, inc_val, body))

    def while_expr(self):
        result = ParseResult()

        if not self.curr_tkn.matches(WT_KEYWORD, 'While'):
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'While' Expected"))

        result.register_next()
        self.next()

        cond = result.register(self.pm_func())
        if result.err: return result

        if not self.curr_tkn.matches(WT_KEYWORD, 'Do'):
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'Do' Expected"))
        
        body = result.register(self.pm_func())
        if result.err: return result

        return result.success(WhileNode(cond, body))
    
    def func_dec(self):
        result = ParseResult()

        if not self.curr_tkn.matches(WT_KEYWORD, 'Func'):
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'Func' Expected"))
        
        result.register_next()
        self.next()

        if self.curr_tkn.type == WT_IDENTIFIER:
            var_name = self.curr_tkn
            result.register_next()
            self.next()
            if self.curr_tkn.type != WT_LEFTPAR:
                return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'(' Expected"))
        else:
            var_name = None
            if self.curr_tkn.type != WT_LEFTPAR:
                return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "Identifier or '(' Expected"))

        result.register_next()
        self.next()
        args = []

        if self.curr_tkn.type == WT_IDENTIFIER:
            args.append(self.curr_tkn)
            result.register_next()
            self.next()

            while self.curr_tkn.type == WT_COMMA:
                result.register_next()
                self.next()

                if self.curr_tkn.type != WT_IDENTIFIER:
                    return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "Identifier Expected"))
                
                args.append(self.curr_tkn)
                result.register_next()
                self.next()
            
            if self.curr_tkn.type != WT_RIGHTPAR:
                return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "',' or ')' Expected"))
        else:
            if self.curr_tkn.type != WT_RIGHTPAR:
                return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "Identifier or ')' Expected"))

        result.register_next()
        self.next()

        if self.curr_tkn.type != WT_ARROW:
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'=>' Expected"))
        
        result.register_next()
        self.next()

        node = result.register(self.pm_func())
        if result.err: return result

        return result.success(FuncNode(var_name, args, node))
    
    def list_func(self):
        result = ParseResult()
        elements = []
        start_pos = self.curr_tkn.start_pos.copy_pos()

        if self.curr_tkn.type != WT_LEFTBRACKET:
            return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'[' Expected"))

        result.register_next()
        self.next()

        if self.curr_tkn.type == WT_RIGHTBRACKET:
            result.register_next()
            self.next()
        else:
            elements.append(result.register(self.pm_func()))
            if result.err:
                return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "'[', 'Var', 'For', 'While', 'Func', int, float, identifier, '+', '-', or '(' Expected"))

            while self.curr_tkn.type == WT_COMMA:
                result.register_next()
                self.next()

                elements.append(result.register(self.pm_func()))
                if result.err: return result
            
            if self.curr_tkn.type != WT_RIGHTBRACKET:
                return result.failure(IllegalSyntaxError(self.curr_tkn.start_pos, self.curr_tkn.end_pos, "',' or ']' Expected"))

            result.register_next()
            self.next()
        
        return result.success(ListNode(elements, start_pos, self.curr_tkn.end_pos.copy_pos()))

    def parse(self):
        result = self.pm_func()
        if not result.err and self.curr_tkn.type != WT_EOF:
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
            return result.failure(RuntimeError(node.start_pos, node.end_pos, f"'{var_name}' is not defined", context))
        
        value = value.copy_pos().set_context(context).set_pos(node.start_pos, node.end_pos)
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
        elif node.op_tkn.type == WT_EQUALS:
            result, err = l.comp_ops(r, '==')
        elif node.op_tkn.type == WT_NE:
            result, err = l.comp_ops(r, '!=')
        elif node.op_tkn.type == WT_LT:
            result, err = l.comp_ops(r, '<')
        elif node.op_tkn.type == WT_GT:
            result, err = l.comp_ops(r, '>')
        elif node.op_tkn.type == WT_LTE:
            result, err = l.comp_ops(r, '<=')
        elif node.op_tkn.type == WT_GTE:
            result, err = l.comp_ops(r, '>=')
        elif node.op_tkn.matches(WT_KEYWORD, 'And'):
            result, err = l.comp_ops(r, 'and')
        elif node.op_tkn.matches(WT_KEYWORD, 'Or'):
            result, err = l.comp_ops(r, 'or')
        

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
        elif node.op_tkn.matches(WT_KEYWORD, 'Not'):
            number, err = number.notted()

        if err:
            return rs.failure(err)
        else:
            return rs.success(number.set_pos(node.start_pos, node.end_pos))

    def visit_IfNode(self, node, context):
        result = RuntimeResult()

        for cond, expr in node.cases:
            cond_val = result.register(self.visit(cond, context))
            if result.err: return result

            if cond_val.is_true():
                expr_val = result.register(self.visit(expr, context))
                if result.err: return result
                return result.success(expr_val)

        if node.else_case:
            else_val = result.register(self.visit(node.else_case, context))
            if result.err: return result
            return result.success(else_val)
        
        return result.success(None)
    
    def visit_ForNode(self, node, context):
        result = RuntimeResult()
        elements = []

        start_val = result.register(self.visit(node.start_val, context))
        if result.err: return result

        end_val = result.register(self.visit(node.end_val, context))
        if result.err: return result

        if node.inc:
            inc_val = result.register(self.visit(node.inc, context))
            if result.err: return result
        else:
            inc_val = Num(1)
        
        i = int(start_val.value)
        int_end = int(end_val.value)

        if int(inc_val.value) >= 0:
            cond = lambda: i < int_end
        else:
            cond = lambda: i > int_end
        
        while cond():
            context.symbol_table.set(node.var_name.value, Num(i))
            i += int(inc_val.value)

            elements.append(result.register(self.visit(node.body, context)))
            if result.err: return result
        
        return result.success(List(elements).set_context(context).set_pos(node.start_pos, node.end_pos))

    def visit_WhileNode(self, node, context):
        result = RuntimeResult()
        elements = []

        while True:
            cond = result.register(self.visit(node.cond, context))
            if result.err: return result

            if not cond.is_true(): break

            elements.append(result.register(self.visit(node.body, context)))
            if result.err: return result

        return result.success(List(elements).set_context(context).set_pos(node.start_pos, node.end_pos))
    
    def visit_FuncNode(self, node, context):
        result = RuntimeResult()

        func_name = node.var_name.value if node.var_name else None
        body = node.body
        args = [args.value for args in node.args]
        func_val = Function(func_name, body, args).set_context(context).set_pos(node.start_pos, node.end_pos)

        if node.var_name:
            context.symbol_table.set(func_name, func_val)

        return result.success(func_val)
    
    def visit_CallNode(self, node, context):
        result = RuntimeResult()
        args = []

        vals_calling = result.register(self.visit(node.called_node, context))
        if result.err: return result
        vals_calling = vals_calling.copy_pos().set_pos(node.start_pos, node.end_pos)

        for arg in node.args:
            args.append(result.register(self.visit(arg, context)))
            if result.err: return result

        ret_val = result.register(vals_calling.run(args))
        if result.err: return result

        ret_val = ret_val.copy().set_pos(node.start_pos, node.end_pos).set_context(context)
        return result.success(ret_val)
    
    def visit_StringNode(self, node, context):
        return RuntimeResult().success(String(node.str_tkn.value).set_context(context).set_pos(node.start_pos, node.end_pos))

    def visit_ListNode(self, node, context):
        result = RuntimeResult()
        elements = []

        for el in node.elements:
            elements.append(result.register(self.visit(el, context)))
            if result.err: return result
        
        return result.success(List(elements).set_context(context).set_pos(node.start_pos, node.end_pos))


#######################################
# SYMBOL TABLE CLASS
#######################################

class SymbolTable:
    def __init__(self, parent=None):
        self.symbols = {}
        self.parent = parent

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
global_sym_table.set("Null", Num.null)
global_sym_table.set("True", Num.true)
global_sym_table.set("False", Num.false)
global_sym_table.set("Pi", Num.pi)
global_sym_table.set("Print", BuiltInFunction.print)
global_sym_table.set("PrintRet", BuiltInFunction.print_and_ret)
global_sym_table.set("Input", BuiltInFunction.input)
global_sym_table.set("ToInt", BuiltInFunction.input_to_int)
global_sym_table.set("Clear", BuiltInFunction.clear)
global_sym_table.set("IsNum", BuiltInFunction.is_num)
global_sym_table.set("IsStr", BuiltInFunction.is_str)
global_sym_table.set("IsList", BuiltInFunction.is_lst)
global_sym_table.set("IsFunc", BuiltInFunction.is_func)
global_sym_table.set("Append", BuiltInFunction.append)
global_sym_table.set("Pop", BuiltInFunction.pop)
global_sym_table.set("Extend", BuiltInFunction.extend)

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

class ExpectedCharError(Error):
    def __init__(self, start_pos, end_pos, desc):
        super().__init__(start_pos, end_pos, "Expected Character", desc)

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