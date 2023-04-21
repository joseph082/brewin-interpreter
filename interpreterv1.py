from intbase import InterpreterBase
from bparser import BParser


class Interpreter(InterpreterBase):
    def __init__(self):
        return

    def __discover_all_classes_and_track_them(self, parsed_program):
        return

    def __find_definition_for_class(self, class_name):
        return

    def run(self, program_source):  # def run(self, program):

        # parse the program into a more easily processed form
        result, parsed_program = BParser.parse(program_source)
        return  # error
        self.__discover_all_classes_and_track_them(parsed_program)
        class_def = self.__find_definition_for_class("main")
        obj = class_def.instantiate_object()
        obj.run_method("main")


class ObjectDefinition:
    def __init__(self):
        return
    # Interpret the specified method using the provided parameters

    def call_method(self, method_name, parameters):
        method = self.__find_method(method_name)
        statement = method.get_top_level_statement()
        result = self.__run_statement(statement)
        return result
    # runs/interprets the passed-in statement until completion and
    # gets the result, if any

    def __run_statement(self, statement):
        if is_a_print_statement(statement):
            result = self.__execute_print_statement(statement)
        elif is_an_input_statement(statement):

            result = self.__execute_input_statement(statement)
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
        return result


class ClassDefinition:
    # constructor for a ClassDefinition
    def __init__(self,):
        self.my_methods = []
        self.my_fields = []
        return
    # uses the definition of a class to create and return an instance of it

    def instantiate_object(self):
        obj = ObjectDefinition()
        for method in self.my_methods:
            obj.add_method(method)
        for field in self.my_fields:
            obj.add_field(field.name(), field.initial_value())
        return obj


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


main()


def print_line_nums(parsed_program):
    for item in parsed_program:
        if type(item) is not list:
            print(f'{item} was found on line {item.line_num}')
        else:
            print_line_nums(item)
