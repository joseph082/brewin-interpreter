# from __future__ import annotations  # https://stackoverflow.com/questions/33533148/how-do-i-type-hint-a-method-with-the-type-of-the-enclosing-class
from intbase import InterpreterBase, ErrorType
from bparser import BParser, StringWithLineNumber

from typing import TypeGuard, Union, List, Tuple, Any, NoReturn, Self


# https://devblogs.microsoft.com/python/pylance-introduces-five-new-features-that-enable-type-magic-for-python-developers/
# ProgramElementType = Union[StringWithLineNumber, List['ProgramElementType']]
# ParsedProgramType = List[ProgramElementType]
# ProgramElementType = Union[StringWithLineNumber, Tuple['ProgramElementType']]
# ParsedProgramType = Tuple[ProgramElementType]
# FlattenedExprType = List[StringWithLineNumber]
PrintArgsType = Tuple[int | bool | str | StringWithLineNumber, ...]
PrintStatementType = Tuple[StringWithLineNumber, *PrintArgsType]

InputStringStatementType = Tuple[StringWithLineNumber, StringWithLineNumber]
InputIntStatementType = Tuple[StringWithLineNumber, StringWithLineNumber]

SetStatementType = Tuple[StringWithLineNumber, StringWithLineNumber, StringWithLineNumber]


StatementType = Tuple[Any, *Tuple[Any, ...]]

IfStatementType = Tuple[StringWithLineNumber, StatementType, StatementType, StatementType]  # second should be expression
BeginStatementType = Tuple[StringWithLineNumber, Tuple[StatementType, *Tuple[StatementType]]]
WhileStatementType = Tuple[StringWithLineNumber, StatementType, StatementType]  # second should be expression

MethodStatementType = Tuple[StringWithLineNumber, StringWithLineNumber, Tuple[StringWithLineNumber], StatementType]
FieldValueType = Union[int, StringWithLineNumber, bool, None, str]  # 'null' == None?
FieldStatementType = Tuple[StringWithLineNumber, StringWithLineNumber, StringWithLineNumber]
MethodOrFieldType = Union[MethodStatementType, FieldStatementType]

CallExpressionType = Tuple[StringWithLineNumber, StringWithLineNumber, StringWithLineNumber, StatementType, *Tuple[StatementType, ...]]  # expression instead of statement
NewExpressionType = Tuple[StringWithLineNumber, StringWithLineNumber]
ReturnExpressionType = Union[Tuple[StringWithLineNumber], Tuple[StringWithLineNumber, StatementType]]
ClassMembersType = Tuple[MethodOrFieldType, *Tuple[MethodOrFieldType, ...]]

# ClassStatementType = Tuple[StringWithLineNumber, StringWithLineNumber, MethodOrFieldType, *Tuple[MethodOrFieldType, ...]]
ClassStatementType = Tuple[StringWithLineNumber, StringWithLineNumber, *ClassMembersType]
# i think this types to at least one MethodOrFieldType? https://docs.python.org/3/library/typing.html#typing.TypeVarTuple
ParsedProgramType = Tuple[ClassStatementType, *Tuple[ClassStatementType, ...]]  # tuple of class statements


def is_print_statement(s: StatementType) -> TypeGuard[PrintStatementType]:
    return s[0] == InterpreterBase.PRINT_DEF


def is_input_str_statement(s: StatementType) -> TypeGuard[InputStringStatementType]:
    return s[0] == InterpreterBase.INPUT_STRING_DEF  # and is_StringWithLineNumber(s[1])


def is_input_int_statement(s: StatementType) -> TypeGuard[InputIntStatementType]:
    return s[0] == InterpreterBase.INPUT_INT_DEF  # and is_StringWithLineNumber(s[1])


def is_set_statement(s: StatementType) -> TypeGuard[SetStatementType]:
    return s[0] == InterpreterBase.SET_DEF


def is_call_expression(s: StatementType) -> TypeGuard[CallExpressionType]:
    return s[0] == InterpreterBase.CALL_DEF


def is_new_expression(s: StatementType) -> TypeGuard[NewExpressionType]:
    return s[0] == InterpreterBase.NEW_DEF


def is_while_statement(s: StatementType) -> TypeGuard[WhileStatementType]:
    return s[0] == InterpreterBase.WHILE_DEF


def is_if_statement(s: StatementType) -> TypeGuard[IfStatementType]:
    return s[0] == InterpreterBase.IF_DEF


def is_begin_statement(s: StatementType) -> TypeGuard[BeginStatementType]:
    return s[0] == InterpreterBase.BEGIN_DEF


def is_return_statement(s: StatementType) -> TypeGuard[ReturnExpressionType]:
    return s[0] == InterpreterBase.RETURN_DEF


def is_field_type(s) -> TypeGuard[FieldValueType]:
    return isinstance(s, (bool, int, str, StringWithLineNumber)) or s is None


def is_StringWithLineNumber(s) -> TypeGuard[StringWithLineNumber]:
    return isinstance(s, StringWithLineNumber)


# def is_parsed_program_type(p) -> TypeGuard[ParsedProgramType]:
#     return isinstance(p, list) and all(is_StringWithLineNumber(x) or is_parsed_program_type(x) for x in p)


def is_list(x) -> TypeGuard[List]:
    return isinstance(x, list)


def is_method_statement(m) -> TypeGuard[MethodStatementType]:
    return isinstance(m, tuple) and len(m) == 4 and m[0] == InterpreterBase.METHOD_DEF


def is_field_statement(f) -> TypeGuard[FieldStatementType]:
    return isinstance(f, tuple) and len(f) == 3 and f[0] == InterpreterBase.FIELD_DEF


def is_class_members_type(c) -> TypeGuard[ClassMembersType]:
    return all(is_method_statement(x) or is_field_statement(x) for x in c)


def is_class_statement(c) -> TypeGuard[ClassStatementType]:
    return isinstance(c, tuple) and len(c) >= 3 and c[0] == InterpreterBase.CLASS_DEF and is_StringWithLineNumber(c[1]) and is_class_members_type(c[2:])  # method/field


def is_statement(s) -> TypeGuard[StatementType]:
    return isinstance(s, tuple)


AddExpressionType = Tuple[StringWithLineNumber, StatementType, StatementType]  # +
IntStringComparisonExpressionType = Tuple[StringWithLineNumber, StatementType, StatementType]  # <, >, <=, >=
EqualityComparisonExpressionType = Tuple[StringWithLineNumber, StatementType, StatementType]  # ==, !=


def is_add_expression(e) -> TypeGuard[AddExpressionType]:
    return e[0] == '+'


def is_int_string_comparison_expression(e) -> TypeGuard[IntStringComparisonExpressionType]:
    return e[0] == '<' or e[0] == '>' or e[0] == '<=' or e[0] == '>='


def is_equality_comparison_expression(e) -> TypeGuard[EqualityComparisonExpressionType]:
    return e[0] == '==' or e[0] == '!='


class Nothing:
    def __init__(self) -> None:
        return


class Method:
    def __init__(self, name: StringWithLineNumber, method_params, expr) -> None:
        self.name = name
        self.params = {}  # name mapped to value
        for p in method_params:
            self.params[p] = None
        self.expr = expr

        return

    def mutate(self):
        self.name = StringWithLineNumber('asdf', 0)

    def get_top_level_statement(self):
        return self.expr

    def get_name(self) -> StringWithLineNumber:
        return self.name


class Field:
    def __init__(self, name: StringWithLineNumber, val) -> None:
        self.name = name
        if is_field_type(val):
            self.curr_val = val
            self.init_val = val
        else:
            raise Exception('invalid field value')
        return

    def get_name(self) -> StringWithLineNumber:
        return self.name

    def get_initial_value(self) -> FieldValueType:
        return self.init_val

    # def __copy__(self) -> Field:
    #     return Field(self.name, self.init_val)

    # def __deepcopy__(self, _memo) -> Field:
    #     return Field(self, self.name, self.init_val)


class ObjectDefinition:
    def __init__(self, interpreter: 'Interpreter') -> None:
        self.obj_methods = {}
        self.__obj_fields = {}
        self.interpreter = interpreter
        return
    # Interpret the specified method using the provided parameters

    def __find_method(self, method_name: str, line_num: int) -> Method | None:
        if method_name not in self.obj_methods:
            self.interpreter.error(ErrorType.NAME_ERROR, line_num=line_num)
            return
        return self.obj_methods[method_name]

    def add_method(self, name, method: Method) -> None:
        self.obj_methods[name] = method
        return

    def add_field(self, name: StringWithLineNumber, initial_val) -> None:
        self.__obj_fields[name] = Field(name, initial_val)
        return

    def call_method(self, method_name: str | StringWithLineNumber, line_num: int, parameters=()):
        method = self.__find_method(method_name, line_num)
        if method is None:
            return
        self.__current_method = method
        if len(parameters) != len(method.params):
            self.interpreter.error(ErrorType.TYPE_ERROR)
            return
        for i, p in enumerate(method.params):
            method.params[p] = parameters[i]
        statement = method.get_top_level_statement()
        result = self.__run_statement(statement)
        return result
    # runs/interprets the passed-in statement until completion and
    # gets the result, if any

    def __run_statement(self, statement):
        if not is_statement(statement):
            raise Exception('unexpected statement/expr {statement}')
        print(f'running statement: {statement}')
        result = self.interpreter.nothing
        if is_print_statement(statement):
            self.__execute_print_statement(statement)
        elif is_input_str_statement(statement):
            self.__execute_input_str_statement(statement)
        elif is_input_int_statement(statement):
            self.__execute_input_int_statement(statement)
        elif is_call_expression(statement):
            current_method = self.__current_method
            result = self.__execute_call_expression(statement)
            self.__current_method = current_method
        elif is_new_expression(statement):
            result = self.__execute_new_expression(statement)
        elif is_while_statement(statement):
            self.__execute_while_statement(statement)
        elif is_if_statement(statement):
            self.__execute_if_statement(statement)
        elif is_return_statement(statement):
            result = self.__execute_return_expression(statement)
        elif is_begin_statement(statement):
            self.__execute_all_sub_statements_of_begin_statement(statement)
        elif is_set_statement(statement):
            self.__execute_set_statement(statement)
        elif is_add_expression(statement):
            result = self.__execute_add_expression(statement)
        elif is_int_string_comparison_expression(statement):
            result = self.__execute_int_string_comparison_expression(statement)
        elif is_equality_comparison_expression(statement):
            result = self.__execute_equality_comparison_expression(statement)

        return result

    def __execute_print_statement(self, args: PrintStatementType) -> None:
        output_str = ''
        for a in args[1:]:
            if is_statement(a):
                # execute statement
                res = self.__run_statement(a)
                if res is not None:
                    output_str += self.__stringify(res)
            elif is_StringWithLineNumber(a):
                output_str += self.__stringify(a)
        self.interpreter.output(output_str)
        return

    def __execute_input_str_statement(self, statement: InputStringStatementType) -> None:
        # check function params, then class vars
        var_name = statement[1]
        input = self.interpreter.get_input()
        if not isinstance(input, str):
            raise TypeError("input isn't a str")
        if var_name in self.__current_method.params:
            self.__current_method.params[var_name] = input
        else:
            self.__obj_fields[var_name].curr_val = input
        return

    def __execute_input_int_statement(self, statement: InputIntStatementType) -> None:
        var_name = statement[1]
        input = self.interpreter.get_input()
        if not isinstance(input, str):
            raise TypeError("input isn't a str")
        if var_name in self.__current_method.params:
            self.__current_method.params[var_name] = input
        else:
            self.__obj_fields[var_name].curr_val = input
        return

    def __execute_all_sub_statements_of_begin_statement(self, statement: BeginStatementType) -> None:
        for sub_statement in statement[1:]:
            self.__run_statement(sub_statement)
        return

    def __execute_set_statement(self, statement: SetStatementType) -> None:
        var_name = statement[1]
        res = self.__parse_value(statement[2])
        if isinstance(res, Nothing):
            self.interpreter.error(ErrorType.TYPE_ERROR, line_num=statement[0].line_num)
            return
        # if is_statement(statement[2]):
        #     # execute statement
        #     res = self.__run_statement(statement[2])
        if var_name in self.__current_method.params:
            self.__current_method.params[var_name] = res
        elif var_name in self.__obj_fields:
            self.__obj_fields[var_name].curr_val = res
        else:
            self.interpreter.error(ErrorType.NAME_ERROR, line_num=statement[0].line_num)
        return

    def __parse_value(self, x: StatementType | StringWithLineNumber | str | int | Self | Nothing) -> int | StringWithLineNumber | bool | str | Self | Nothing | NoReturn:
        # https://stackoverflow.com/a/70932112 for Self
        if is_statement(x):
            res = self.__run_statement(x)
            if res is None:
                raise Exception('__parse_val called with None')
            return self.__parse_value(res)
        elif is_StringWithLineNumber(x):
            try:
                return int(x)
            except ValueError:
                pass
            except Exception as e:
                raise (e)  # unexpected exception
            if x[0] == '"' and x[-1] == '"':
                return x[1:-1]  # remove quotes
            if x == InterpreterBase.TRUE_DEF:
                return True
            if x == InterpreterBase.FALSE_DEF:
                return False
            if x[0] != '"' and x[-1] != '"':  # variable
                if x in self.__current_method.params:
                    return self.__current_method.params[x]
                return self.__obj_fields[x].curr_val
            else:
                raise Exception(f'unknown StringWithLineNumber as parse_value argument: {x}')
        elif isinstance(x, str):
            if x[0] == '"' and x[-1] == '"':
                return x[1:-1]
            return x
        elif isinstance(x, (int, ObjectDefinition, Nothing)):
            return x
        else:
            raise Exception('not a tuple, string, or int in parse_value')

    def __stringify(self, x) -> str | NoReturn:
        if x is True:
            return InterpreterBase.TRUE_DEF
        if x is False:
            return InterpreterBase.FALSE_DEF
        # if x is None:
        #     output_str += Interpreter.NULL_DEF
        # barista prints 'None' and idt any test case tests on this
        if isinstance(x, int):
            return str(x)
        if isinstance(x, str):
            try:
                int_val = int(x)
                return str(int_val)
            except ValueError:
                pass
            except Exception as e:
                raise e  # unexpected exception

            if x[0] == '"' and x[-1] == '"':
                return x[1:-1]  # remove quotes
            if x == InterpreterBase.TRUE_DEF or x == InterpreterBase.FALSE_DEF:
                return x
            if x[0] != '"' and x[-1] != '"':  # variable
                if x in self.__current_method.params:
                    value = self.__current_method.params[x]
                else:
                    value = self.__obj_fields[x].curr_val
                if isinstance(value, str):
                    return value

                return self.__stringify(value)

            raise Exception('unknown string in stringify')
        else:
            raise Exception(x, type(x), 'called stringify with invalid type.')

    def __execute_if_statement(self, statement: IfStatementType) -> None:
        expr_res = self.__parse_value(statement[1])
        if expr_res is True:
            self.__run_statement(statement[2])
        elif expr_res is False:
            self.__run_statement(statement[3])
        else:
            self.interpreter.error(ErrorType.TYPE_ERROR, line_num=statement[0].line_num)
        return

    def __execute_while_statement(self, statement: WhileStatementType) -> None:
        while (expr_res := self.__parse_value(statement[1])) is True:
            self.__run_statement(statement[2])
        if expr_res is False:
            return
        self.interpreter.error(ErrorType.TYPE_ERROR, line_num=statement[0].line_num)
        return

    def __execute_add_expression(self, expr: AddExpressionType) -> int | str:
        a = self.__parse_value(expr[1])
        b = self.__parse_value(expr[2])
        if isinstance(a, int) and isinstance(b, int):
            return a + b
        if isinstance(a, str) and isinstance(b, str):
            return f'"{a}{b}"'  # return with quotes to mark this as literal instead of a var
        self.interpreter.error(ErrorType.TYPE_ERROR, line_num=expr[0].line_num)
        return "_"

    def __execute_int_string_comparison_expression(self, expr: IntStringComparisonExpressionType) -> bool:
        a = self.__parse_value(expr[1])
        b = self.__parse_value(expr[2])
        comparison_functions = {'>': lambda x, y: x > y, '>=': lambda x, y: x >= y,
                                '<': lambda x, y: x < y, '<=': lambda x, y: x <= y
                                }
        if (isinstance(a, int) and isinstance(b, int)) or (isinstance(a, str) and isinstance(b, str)):
            return comparison_functions[expr[0]](a, b)
        self.interpreter.error(ErrorType.TYPE_ERROR, line_num=expr[0].line_num)
        return False

    def __execute_equality_comparison_expression(self, expr: EqualityComparisonExpressionType) -> bool:
        a = self.__parse_value(expr[1])
        b = self.__parse_value(expr[2])
        if (isinstance(a, int) and isinstance(b, int)) or (isinstance(a, str) and isinstance(b, str)) or (isinstance(a, bool) and isinstance(b, bool)):
            if expr[0] == '==':
                return a == b
            return a != b
        self.interpreter.error(ErrorType.TYPE_ERROR, line_num=expr[0].line_num)
        return False

    def __execute_call_expression(self, expr: CallExpressionType):
        call_str = expr[0]
        if is_StringWithLineNumber(call_str) and is_StringWithLineNumber(expr[2]) and call_str.line_num is not None:
            if expr[1] == InterpreterBase.ME_DEF:
                obj_instance = self
            else:
                obj_instance = self.__obj_fields[expr[1]].curr_val
            if obj_instance is None:
                self.interpreter.error(ErrorType.FAULT_ERROR, line_num=call_str.line_num)
                return
            return obj_instance.call_method(expr[2], call_str.line_num, tuple(map(self.__parse_value, expr[3:])))

    def __execute_new_expression(self, expr: NewExpressionType):
        if expr[1] not in self.interpreter.classes:
            self.interpreter.error(ErrorType.TYPE_ERROR, line_num=expr[0].line_num)
            return
        return self.interpreter.classes[expr[1]].instantiate_object()

    def __execute_return_expression(self, expr: ReturnExpressionType):
        if len(expr) == 1:
            return self.interpreter.nothing
        return self.__run_statement(expr[1])


"""
● Methods and fields may be defined in any order within the class; all methods and fields
are visible to all other methods inside the class regardless of the order they are defined
● Duplicate class names are not allowed. If a program defines two or more classes with
the same name you must generate an error of type ErrorType.NAME_ERROR by calling
InterpreterBase.error().
"""


class ClassDefinition:
    # constructor for a ClassDefinition
    def __init__(self, fields_or_methods: ClassMembersType, interpreter: 'Interpreter') -> None:  # https://stackoverflow.com/questions/33837918/type-hints-solve-circular-dependency
        self.my_methods = {}
        self.my_fields = {}
        self.interpreter = interpreter

        for expr in fields_or_methods:
            name = expr[1]
            if is_method_statement(expr):
                if name in self.my_methods:
                    self.interpreter.error(ErrorType.NAME_ERROR, line_num=name.line_num)  # they use expr[0].line_num
                self.my_methods[name] = Method(name, expr[2], expr[3])
            elif is_field_statement(expr):
                if name in self.my_fields:
                    self.interpreter.error(ErrorType.NAME_ERROR, line_num=name.line_num)  # todo
                try:
                    field_val = int(expr[2])
                except:
                    if expr[2] == InterpreterBase.TRUE_DEF:
                        field_val = True
                    elif expr[2] == InterpreterBase.FALSE_DEF:
                        field_val = False
                    elif expr[2] == InterpreterBase.NULL_DEF:
                        field_val = None
                    else:
                        field_val = expr[2][1:-1]
                self.my_fields[name] = Field(name, field_val)
            else:
                raise Exception('fields_or_methods expr: {expr} not parsed')
        if len(self.my_methods) == 0:
            raise Exception('no methods in class')
        return
    # uses the definition of a class to create and return an instance of it

    def instantiate_object(self) -> ObjectDefinition:
        obj = ObjectDefinition(self.interpreter)
        for name, method in self.my_methods.items():
            obj.add_method(name, method)
        for name, field in self.my_fields.items():
            obj.add_field(name, field.get_initial_value())
        return obj


class Interpreter(InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False) -> None:
        super().__init__(console_output, inp)  # call InterpreterBase’s constructor

        self.classes = {}  # self.classes = dict[StringWithLineNumber, Any]. doesn't work with in
        self.nothing = Nothing()
        return

    def __discover_all_classes_and_track_them(self, parsed_program: ParsedProgramType) -> None:
        for expr in parsed_program:
            if is_class_statement(expr):
                class_name = expr[1]
                if not is_StringWithLineNumber(class_name):  # also doesn't get correct type
                    raise Exception('not is_StringWithLineNumber')
                fields_or_methods = expr[2:]  # doesn't get correct type so use typeguard
                if not is_class_members_type(fields_or_methods):
                    return
                # print(id(fields_or_methods[0]), id(expr[2]))  # same ids
                if class_name in self.classes:
                    super().error(ErrorType.NAME_ERROR, line_num=class_name.line_num)  # TODO
                    return
                self.classes[class_name] = ClassDefinition(fields_or_methods, self)

        print(f'tuple    parsed_program: {parsed_program} in __discover_all_classes_and_track_them')
        print(f'self.classes: {self.classes.keys()} in __discover_all_classes_and_track_them')
        return

    def __find_definition_for_class(self, class_name: str) -> ClassDefinition:
        if class_name not in self.classes:
            raise Exception(f'__find_definition_for_class class_name {class_name} not in self.classes')
        return self.classes[class_name]

    def run(self, program_source: list[str]) -> None:  # def run(self, program):

        # parse the program into a more easily processed form
        result, parsed_program = BParser.parse(program_source)
        if result == False:
            return  # error
        # if not is_parsed_program_type(parsed_program):
        #     return
        if not is_list(parsed_program):
            return
        self.parsed_program_orig = parsed_program
        self.parsed_program = deeptuple(parsed_program)

        self.__discover_all_classes_and_track_them(self.parsed_program)
        class_def = self.__find_definition_for_class(InterpreterBase.MAIN_CLASS_DEF)
        obj = class_def.instantiate_object()

        # obj.run_method("main")
        obj.call_method(InterpreterBase.MAIN_FUNC_DEF, -1)  # has no params


# def test():
#     print('test', is_parsed_program_type(''), is_parsed_program_type(['']), is_parsed_program_type([StringWithLineNumber('', 0)]),
#           is_parsed_program_type(StringWithLineNumber('', 0)), is_parsed_program_type([StringWithLineNumber('', 0), StringWithLineNumber('', 0), [StringWithLineNumber('', 0), [StringWithLineNumber('', 0), StringWithLineNumber('', 0)]]]),)

def deep_tuple_helper(x):
    if isinstance(x, StringWithLineNumber):
        return x
    if isinstance(x, list):
        return tuple(deep_tuple_helper(x) for x in x)
    return x


def deeptuple(l: list) -> Tuple:
    return tuple(deep_tuple_helper(x) for x in l)


def print_line_nums(parsed_program) -> None:
    for item in parsed_program:
        if is_StringWithLineNumber(item):
            print(f'{item} was found on line {item.line_num}')
        else:
            print_line_nums(item)


def main():
    # all programs will be provided to your interpreter as a list of
    # python strings, just as shown here.
    program_source = ['(class main',
                      ' (method main ()',
                      ' (print "hello world!")',
                      ' ) # end of method',
                      ') # end of class']
    program_source = """(class main
 (field x 0)
 (field y "test")
 (method main ()
  (begin
   (set x "s")
   (set x true)
   (print x)
   (print "literal" 1 true false)
   # (inputi x)
   (inputs x)
   (print x)
   (inputi y)
   (print y)
  )
 )
)""".split('\n')
    # this is how you use our BParser class to parse a valid
    # Brewin program into python list format.
    result, parsed_program = BParser.parse(program_source)
    if result == True:
        print(f'original parsed_program: {parsed_program}')
    else:
        print('Parsing failed. There must have been a mismatched parenthesis.')

    # x: PrintStatementType = (StringWithLineNumber('d', 1), 'd')
    # print(type(x))

    # l = [Method(StringWithLineNumber('d', 1), [[], []])]
    # lt = deeptuple(l)
    # print(l[0], lt[0])
    # l[0].mutate()
    # print(l[0].get_name(), lt[0].get_name(), 'changed', id(l[0]), id(lt[0])) # points to same class instance
    # t = deeptuple(parsed_program[0])
    # print(parsed_program[0], t)
    # # parsed_program[0][2] = 'asdfad'
    # print(parsed_program[0], t)
    # print_line_nums(parsed_program)

    program_interpreter = Interpreter()
    program_interpreter.run(program_source)


if __name__ == '__main__':
    main()
