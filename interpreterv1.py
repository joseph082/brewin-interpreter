from intbase import InterpreterBase, ErrorType
from bparser import BParser, StringWithLineNumber

from typing import TypeGuard, Union, List, Tuple, Any


# https://devblogs.microsoft.com/python/pylance-introduces-five-new-features-that-enable-type-magic-for-python-developers/
# ProgramElementType = Union[StringWithLineNumber, List['ProgramElementType']]
# ParsedProgramType = List[ProgramElementType]
# ProgramElementType = Union[StringWithLineNumber, Tuple['ProgramElementType']]
# ParsedProgramType = Tuple[ProgramElementType]
# FlattenedExprType = List[StringWithLineNumber]
PrintArgsType = Tuple[int | bool | str | StringWithLineNumber, ...]
PrintStatementType = Tuple[StringWithLineNumber, *PrintArgsType]

StatementType = Tuple[Any]

MethodStatementType = Tuple[StringWithLineNumber, StringWithLineNumber, Tuple[StringWithLineNumber], StatementType]
FieldValueType = Union[int, StringWithLineNumber, bool, None]  # 'null' == None? # maybe add str too?
FieldStatementType = Tuple[StringWithLineNumber, StringWithLineNumber, FieldValueType]
MethodOrFieldType = Union[MethodStatementType, FieldStatementType]

ClassMembersType = Tuple[MethodOrFieldType, *Tuple[MethodOrFieldType, ...]]

# ClassStatementType = Tuple[StringWithLineNumber, StringWithLineNumber, MethodOrFieldType, *Tuple[MethodOrFieldType, ...]]
ClassStatementType = Tuple[StringWithLineNumber, StringWithLineNumber, *ClassMembersType]
# i think this types to at least one MethodOrFieldType? https://docs.python.org/3/library/typing.html#typing.TypeVarTuple
ParsedProgramType = Tuple[ClassStatementType, *Tuple[ClassStatementType, ...]]  # tuple of class statements


def is_a_print_statement(statement: StatementType) -> TypeGuard[PrintStatementType]:
    return statement[0] == InterpreterBase.PRINT_DEF


def is_an_input_str_statement(statement: StatementType) -> bool:
    return statement[0] == InterpreterBase.INPUT_STRING_DEF


def is_an_input_int_statement(statement: StatementType) -> bool:
    return statement[0] == InterpreterBase.INPUT_INT_DEF


def is_a_call_statement(statement: StatementType) -> bool:
    return statement[0] == InterpreterBase.CALL_DEF


def is_a_while_statement(statement: StatementType) -> bool:
    return statement[0] == InterpreterBase.WHILE_DEF


def is_an_if_statement(statement: StatementType) -> bool:
    return statement[0] == InterpreterBase.IF_DEF


def is_a_begin_statement(statement: StatementType) -> bool:
    return statement[0] == InterpreterBase.BEGIN_DEF


def is_a_return_statement(statement: StatementType) -> bool:
    return statement[0] == InterpreterBase.RETURN_DEF


def is_field_type(statement) -> TypeGuard[FieldValueType]:
    return isinstance(statement, bool) or isinstance(statement, int) or isinstance(statement, str) or isinstance(statement, StringWithLineNumber)


def is_StringWithLineNumber(statement) -> TypeGuard[StringWithLineNumber]:
    return isinstance(statement, StringWithLineNumber)


# def is_parsed_program_type(p) -> TypeGuard[ParsedProgramType]:
#     return isinstance(p, list) and all(is_StringWithLineNumber(x) or is_parsed_program_type(x) for x in p)


def is_list(x) -> TypeGuard[List]:
    return isinstance(x, list)


def is_method_statement(m) -> TypeGuard[MethodStatementType]:
    return isinstance(m, tuple) and len(m) == 4 and m[0] == InterpreterBase.METHOD_DEF


def is_field_statement(f) -> TypeGuard[MethodStatementType]:
    return isinstance(f, tuple) and len(f) == 3 and f[0] == InterpreterBase.FIELD_DEF


def is_class_members_type(c) -> TypeGuard[ClassMembersType]:
    return all(is_method_statement(x) or is_field_statement(x) for x in c)


def is_class_statement(c) -> TypeGuard[ClassStatementType]:
    return isinstance(c, tuple) and len(c) >= 3 and c[0] == InterpreterBase.CLASS_DEF and is_StringWithLineNumber(c[1]) and is_class_members_type(c[2:])  # method/field


def is_statement(s) -> TypeGuard[StatementType]:
    return isinstance(s, tuple)


class Method:
    def __init__(self, name: StringWithLineNumber, method_params, expr) -> None:
        self.name = name
        self.params = method_params
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
    def __init__(self, interpreter) -> None:
        self.obj_methods = {}
        self.obj_fields = {}
        self.interpreter = interpreter
        print(f'self.interpreter id: {id(self.interpreter)}')
        return
    # Interpret the specified method using the provided parameters

    def __find_method(self, method_name: str) -> Method:
        return self.obj_methods[method_name]

    def add_method(self, name, method: Method) -> None:
        # print(type (method))
        self.obj_methods[name] = method
        return

    def add_field(self, name: StringWithLineNumber, initial_val) -> None:
        self.obj_fields[name] = Field(name, initial_val)
        return

    def call_method(self, method_name: str, parameters=()):
        method = self.__find_method(method_name)
        statement = method.get_top_level_statement()
        result = self.__run_statement(statement)
        return result
    # runs/interprets the passed-in statement until completion and
    # gets the result, if any

    def __run_statement(self, statement):
        print(f'statement: {statement}')
        result = None
        if is_a_print_statement(statement):
            result = self.__execute_print_statement(statement)
        elif is_an_input_str_statement(statement):

            result = self.__execute_input_str_statement(statement)
        elif is_an_input_int_statement(statement):
            result = self.__execute_input_int_statement(statement)
        elif is_a_call_statement(statement):
            pass
            # result = self.__execute_call_statement(statement)
        elif is_a_while_statement(statement):
            pass
            # result = self.__execute_while_statement(statement)
        elif is_an_if_statement(statement):
            pass
            # result = self.__execute_if_statement(statement)
        elif is_a_return_statement(statement):
            pass
            # result = self.__execute_return_statement(statement)
        elif is_a_begin_statement(statement):
            pass
            # result = self.__execute_all_sub_statements_of_begin_statement(statement)
        #
        else:
            result = None

        return result

    def __execute_print_statement(self, args: PrintStatementType):
        output_str = ''
        for a in args[1:]:
            if is_statement(a):
                # execute statement
                res = self.__run_statement(a)
                print(type(res), 'type res')
                output_str += str(res)
            elif is_StringWithLineNumber(a):
                if a[0] == '"' and a[-1] == '"':
                    output_str += a[1:-1]  # remove quotess
                elif a[0] != '"' and a[-1] != '"':
                    raise Exception('this is a variable that needs its value to be read')
                else:
                    raise Exception('unknown print argument string')
            else:
                raise Exception('not a tuple or a string in print statement')
        self.interpreter.output(output_str)

    def __execute_input_str_statement(self, statement) -> str:
        # check function params, then class vars
        name = statement[1]
        if not is_StringWithLineNumber(name):
            raise Exception('__execute_input_str_statement')
        print(name)
        return ''

    def __execute_input_int_statement(self, statement) -> int:
        return 0


"""
You must meet the following requirements when supporting classes in your interpreter.
● Every Brewin program must have at least one class called main, with at least one
method named main in that class. This is where execution of your Brewin program
begins.
13
● Brewin programs may have zero or more additional classes beyond the main class
● Every Brewin class must have at least one method defined within it
● Every Brewin class may have zero or more fields defined within it
● Class names are case sensitive, and must start with an underscore or letter, and may
have underscores, letters and numbers
● Classes may be defined in any order within your source file; all classes are visible to all
other classes (e.g., for instantiating a new object) regardless of whether they're define
above or below the location of instantiation
● Methods and fields may be defined in any order within the class; all methods and fields
are visible to all other methods inside the class regardless of the order they are defined
● There are no official constructors or destructors in Brewin. If you want to define and call
your own methods in a class to perform initialization or destruction you may.
● Duplicate class names are not allowed. If a program defines two or more classes with
the same name you must generate an error of type ErrorType.NAME_ERROR by calling
InterpreterBase.error().
"""


class ClassDefinition:
    # constructor for a ClassDefinition
    def __init__(self, fields_or_methods: ClassMembersType, interpreter) -> None:
        self.my_methods = {}
        self.my_fields = {}
        self.interpreter = interpreter
        print(f'self.interpreter id: {id(self.interpreter)}')

        for expr in fields_or_methods:
            name = expr[1]
            if is_method_statement(expr):
                if name in self.my_methods:
                    self.interpreter.error(ErrorType.NAME_ERROR, line_num=name.line_num)  # they use expr[0].line_num
                self.my_methods[name] = Method(name, expr[2], expr[3])
            elif is_field_statement(expr):
                if name in self.my_fields:
                    self.interpreter.error(ErrorType.NAME_ERROR, line_num=name.line_num)  # todo
                self.my_fields[name] = Field(name, expr[2])
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
        for field in self.my_fields:
            obj.add_field(field.get_name(), field.get_initial_value())
        return obj


class Interpreter(InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False) -> None:
        super().__init__(console_output, inp)  # call InterpreterBase’s constructor

        self.classes = {}  # self.classes = dict[StringWithLineNumber, Any]. doesn't work with in
        return

    def __discover_all_classes_and_track_them(self, parsed_program: ParsedProgramType) -> None:
        for expr in parsed_program:
            if is_class_statement(expr):
                class_name = expr[1]
                fields_or_methods = expr[2:]  # doesn't get correct type so use typeguard
                if not is_class_members_type(fields_or_methods):
                    return
                # print(id(fields_or_methods[0]), id(expr[2]))  # same ids
                if class_name in self.classes:
                    super().error(ErrorType.NAME_ERROR, line_num=1)  # TODO
                    return
                self.classes[class_name] = ClassDefinition(fields_or_methods, self)

        print(f'parsed_program: {parsed_program} in __discover_all_classes_and_track_them', 1, 1)
        print(f'self.classes: {self.classes} in __discover_all_classes_and_track_them')
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
        obj.call_method(InterpreterBase.MAIN_FUNC_DEF)  # has no params


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
    # except:
    #     raise Exception('s')


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
    # this is how you use our BParser class to parse a valid
    # Brewin program into python list format.
    result, parsed_program = BParser.parse(program_source)
    if result == True:
        print(parsed_program)
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


main()
