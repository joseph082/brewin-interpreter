from intbase import InterpreterBase, ErrorType
from bparser import BParser, StringWithLineNumber

from typing import NoReturn, TypeGuard, Union, List

# https://devblogs.microsoft.com/python/pylance-introduces-five-new-features-that-enable-type-magic-for-python-developers/
program_element_type = Union[StringWithLineNumber, List['program_element_type']]
parsed_program_type = List[program_element_type]
flattened_expr_type = List[StringWithLineNumber]
# pri = print


def pri(*args) -> None:
    if True:
        print(*args)


def rai(ex: Exception) -> NoReturn:
    raise ex


class Method:
    def __init__(self, name: StringWithLineNumber, rest: list) -> None:
        self.name = name
        self.params = rest[0]
        self.expr = rest[1]

        return

    def get_top_level_statement(self):
        return self.expr

    def get_name(self) -> StringWithLineNumber:
        return self.name


class Field:
    def __init__(self, rest: list) -> None:
        self.val = rest[0]
        return


def is_a_print_statement(statement: list) -> bool:
    return statement[0] == InterpreterBase.PRINT_DEF


def is_an_input_str_statement(statement: list) -> bool:
    return statement[0] == InterpreterBase.INPUT_STRING_DEF


def is_an_input_int_statement(statement: list) -> bool:
    return statement[0] == InterpreterBase.INPUT_INT_DEF


def is_a_call_statement(statement: list) -> bool:
    return statement[0] == InterpreterBase.CALL_DEF


def is_a_while_statement(statement: list) -> bool:
    return statement[0] == InterpreterBase.WHILE_DEF


def is_StringWithLineNumber(statement) -> TypeGuard[StringWithLineNumber]:
    return isinstance(statement, StringWithLineNumber)


def is_parsed_program_type(p) -> TypeGuard[parsed_program_type]:

    return isinstance(p, list) and all(is_StringWithLineNumber(x) or is_parsed_program_type(x) for x in p)


class ObjectDefinition:
    def __init__(self) -> None:
        self.obj_methods = {}
        self.obj_fields = {}
        return
    # Interpret the specified method using the provided parameters

    def __find_method(self, method_name: str) -> Method:
        return self.obj_methods[method_name]

    def add_method(self, name, method: Method) -> None:
        # pri(type (method))
        self.obj_methods[name] = method
        return

    def add_field(self, name, field: Field) -> None:
        self.obj_fields[name] = field
        return

    def call_method(self, method_name: str, parameters=[]):
        method = self.__find_method(method_name)
        statement = method.get_top_level_statement()
        result = self.__run_statement(statement)
        return result
    # runs/interprets the passed-in statement until completion and
    # gets the result, if any

    def __run_statement(self, statement):
        pri(f'statement: {statement}')
        if is_a_print_statement(statement):
            result = self.__execute_print_statement(statement)
        elif is_an_input_str_statement(statement):

            result = self.__execute_input_str_statement(statement)
        elif is_an_input_int_statement(statement):
            result = self.__execute_input_int_statement(statement)
        elif is_a_call_statement(statement):
            result = self.__execute_call_statement(statement)
        elif is_a_while_statement(statement):
            result = self.__execute_while_statement(statement)
        elif is_an_if_statement(statement):
            result = self.__execute_if_statement(statement)
        elif is_a_return_statement(statement):
            result = self.__execute_return_statement(statement)
        elif is_a_begin_statement(statement):
            result = self.__execute_all_sub_statements_of_begin_statement(statement)
        #
        else:
            result = None
        return result

    def __execute_print_statement(self, args):
        s = ''
        for a in args[1:]:
            if isinstance(a, list):
                # execute statement
                res = self.__run_statement(a)
                pri(type(res))
                s += str(res)
            elif isinstance(a, str):
                s += a
            else:
                rai(Exception('not a list or a string in print statement'))
            pri(a, type(a))
        InterpreterBase().output(s)


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
    def __init__(self, fields_or_methods: list) -> None:
        self.my_methods = {}
        self.my_fields = {}
        for expr in fields_or_methods:
            expr_type, name, *rest = expr
            if expr_type == InterpreterBase.METHOD_DEF:
                if name in self.my_methods:
                    InterpreterBase().error(ErrorType.NAME_ERROR, line_num=0)  # todo
                self.my_methods[name] = Method(name, rest)
            elif expr_type == InterpreterBase.FIELD_DEF:
                if name in self.my_fields:
                    InterpreterBase().error(ErrorType.NAME_ERROR, line_num=0)  # todo
                self.my_fields[name] = Field(rest)
            else:
                raise Exception('fields_or_methods expr: {expr} not parsed')
        if len(self.my_methods) == 0:
            rai(Exception('no methods in class'))
        return
    # uses the definition of a class to create and return an instance of it

    def instantiate_object(self) -> ObjectDefinition:
        obj = ObjectDefinition()
        for name, method in self.my_methods.items():  # used to iterate over list, switch to dict TODO
            obj.add_method(name, method)
        for field in self.my_fields:
            obj.add_field(field.name(), field.initial_value())
        return obj


class Interpreter(InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False) -> None:
        super().__init__(console_output, inp)  # call InterpreterBase’s constructor

        self.classes = {}
        return

    def __discover_all_classes_and_track_them(self, parsed_program: parsed_program_type) -> None:
        for expr in parsed_program:
            if isinstance(expr, list) and len(expr) > 0 and expr[0] == InterpreterBase.CLASS_DEF:
                _, class_name, *fields_or_methods = expr  # don't needa check length because syntax is correct
                if is_StringWithLineNumber(class_name):
                    if class_name in self.classes:
                        super().error(ErrorType.NAME_ERROR, line_num=1)  # TODO
                        return
                    self.classes[class_name] = ClassDefinition(fields_or_methods)

        pri(f'parsed_program: {parsed_program} in __discover_all_classes_and_track_them', 1, 1)
        pri(f'self.classes: {self.classes} in __discover_all_classes_and_track_them')
        return

    def __find_definition_for_class(self, class_name: str) -> ClassDefinition:
        if class_name not in self.classes:
            rai(Exception(f'__find_definition_for_class class_name {class_name} not in self.classes'))  # rm
        return self.classes[class_name]

    def run(self, program_source: list[str]) -> None:  # def run(self, program):

        # parse the program into a more easily processed form
        result, parsed_program = BParser.parse(program_source)
        if result == False:
            return  # error
        if not is_parsed_program_type(parsed_program):
            return
        self.__discover_all_classes_and_track_them(parsed_program)
        class_def = self.__find_definition_for_class(InterpreterBase.MAIN_CLASS_DEF)
        obj = class_def.instantiate_object()
        # obj.run_method("main")
        obj.call_method(InterpreterBase.MAIN_FUNC_DEF)


# def test():
#     print('test', is_parsed_program_type(''), is_parsed_program_type(['']), is_parsed_program_type([StringWithLineNumber('', 0)]),
#           is_parsed_program_type(StringWithLineNumber('', 0)), is_parsed_program_type([StringWithLineNumber('', 0), StringWithLineNumber('', 0), [StringWithLineNumber('', 0), [StringWithLineNumber('', 0), StringWithLineNumber('', 0)]]]),)


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
    if not is_parsed_program_type(parsed_program):
        return
    if result == True:
        print(parsed_program)
    else:
        print('Parsing failed. There must have been a mismatched parenthesis.')
    interpreter = Interpreter()
    interpreter.run(program_source)


main()


def print_line_nums(parsed_program: parsed_program_type) -> None:
    for item in parsed_program:
        if isinstance(item, StringWithLineNumber):
            print(f'{item} was found on line {item.line_num}')
        else:
            print_line_nums(item)
