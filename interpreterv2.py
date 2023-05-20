from bparser import BParser, StringWithLineNumber
from intbase import InterpreterBase, ErrorType
from enum import Enum
from typing import Self, TypeGuard, Union, ValuesView, Callable, NoReturn

# StatementType = tuple[StringWithLineNumber, *tuple[StringWithLineNumber, ...]]
StatementType = Union[tuple[StringWithLineNumber, *tuple[StringWithLineNumber, ...]], tuple['StatementType', *tuple['StatementType', ...]]]

ExpressionType = Union[StringWithLineNumber, StatementType]

PrintArgsType = tuple[Union[StringWithLineNumber, StatementType], ...]
PrintStatementType = tuple[StringWithLineNumber, *PrintArgsType]

InputStringStatementType = tuple[StringWithLineNumber, StringWithLineNumber]
InputIntStatementType = tuple[StringWithLineNumber, StringWithLineNumber]

SetStatementType = tuple[StringWithLineNumber, StringWithLineNumber, StatementType]

IfStatementType = Union[tuple[StringWithLineNumber, StatementType, StatementType, StatementType],
                        tuple[StringWithLineNumber, StatementType, StatementType]]
BeginStatementType = tuple[StringWithLineNumber, tuple[StatementType, *tuple[StatementType, ...]]]
WhileStatementType = tuple[StringWithLineNumber, StatementType, StatementType]

MethodStatementType = tuple[StringWithLineNumber, StringWithLineNumber, StringWithLineNumber, tuple[tuple[StringWithLineNumber, StringWithLineNumber], ...], StatementType]
FieldValueType = Union[int, StringWithLineNumber, bool, None, str, 'ObjectDef']
FieldStatementType = tuple[StringWithLineNumber, StringWithLineNumber,  StringWithLineNumber, StringWithLineNumber]
MethodOrFieldType = Union[MethodStatementType, FieldStatementType]

CallExpressionType = tuple[StringWithLineNumber, StatementType, StringWithLineNumber, *tuple[StatementType, ...]]
NewExpressionType = tuple[StringWithLineNumber, StringWithLineNumber]
ReturnExpressionType = Union[tuple[StringWithLineNumber], tuple[StringWithLineNumber, StatementType]]
ClassMembersType = tuple[MethodOrFieldType, *tuple[MethodOrFieldType, ...]]

# ClassStatementType = tuple[StringWithLineNumber, StringWithLineNumber, MethodOrFieldType, *tuple[MethodOrFieldType, ...]]
# ClassStatementType = tuple[StringWithLineNumber, StringWithLineNumber, *ClassMembersType]
ClassStatementType = Union[tuple[StringWithLineNumber, StringWithLineNumber, MethodOrFieldType, *tuple[MethodOrFieldType, ...]],
                           tuple[StringWithLineNumber, StringWithLineNumber, StringWithLineNumber, StringWithLineNumber, MethodOrFieldType, *tuple[MethodOrFieldType, ...]]]
ParsedProgramType = tuple[ClassStatementType, *tuple[ClassStatementType, ...]]  # tuple of class statements


def is_StringWithLineNumber(s) -> TypeGuard[StringWithLineNumber]:
    return isinstance(s, StringWithLineNumber)


def is_statement(s) -> TypeGuard[StatementType]:
    return isinstance(s, tuple) and len(s) >= 1  # and all(map(lambda x: is_StringWithLineNumber(x) or is_statement(x), s))


def is_expression(s) -> TypeGuard[ExpressionType]:
    return is_statement(s) or is_StringWithLineNumber(s)  # should be a variable


def is_print_statement(s: StatementType) -> TypeGuard[PrintStatementType]:
    return s[0] == InterpreterBase.PRINT_DEF and len(s) >= 2 and all(map(is_expression, s[1:]))


def is_input_str_statement(s: StatementType) -> TypeGuard[InputStringStatementType]:
    return s[0] == InterpreterBase.INPUT_STRING_DEF and len(s) == 2 and is_StringWithLineNumber(s[1])


def is_input_int_statement(s: StatementType) -> TypeGuard[InputIntStatementType]:
    return s[0] == InterpreterBase.INPUT_INT_DEF and len(s) == 2 and is_StringWithLineNumber(s[1])


def is_set_statement(s: StatementType) -> TypeGuard[SetStatementType]:
    return s[0] == InterpreterBase.SET_DEF and len(s) == 3 and is_StringWithLineNumber(s[1]) and is_expression(s[2])


def is_call_expression(s: StatementType) -> TypeGuard[CallExpressionType]:
    return s[0] == InterpreterBase.CALL_DEF and len(s) >= 3 and is_expression(s[1]) and is_StringWithLineNumber(s[2]) and all(map(is_expression, s[3:]))


def is_new_expression(s: StatementType) -> TypeGuard[NewExpressionType]:
    return s[0] == InterpreterBase.NEW_DEF and len(s) == 2 and is_StringWithLineNumber(s[1])


def is_while_statement(s: StatementType) -> TypeGuard[WhileStatementType]:
    return s[0] == InterpreterBase.WHILE_DEF and len(s) == 3 and is_expression(s[1]) and is_statement(s[2])


def is_if_statement(s: StatementType) -> TypeGuard[IfStatementType]:
    return s[0] == InterpreterBase.IF_DEF and (len(s) == 4 and is_statement(s[3]) or len(s) == 3) and is_statement(s[2]) and is_expression(s[1])


def is_begin_statement(s: StatementType) -> TypeGuard[BeginStatementType]:
    return s[0] == InterpreterBase.BEGIN_DEF and len(s) > 1 and all(map(is_statement, s[1:]))


def is_return_expression(s: StatementType) -> TypeGuard[ReturnExpressionType]:
    return s[0] == InterpreterBase.RETURN_DEF and (len(s) == 1 or len(s) == 2 and is_expression(s[1]))


# def is_field_type(s) -> TypeGuard[FieldValueType]:
#     return isinstance(s, (bool, int, str, StringWithLineNumber, ObjectDefinition)) or s is None


def is_method_statement(m) -> TypeGuard[MethodStatementType]:
    return is_statement(m) and len(m) == 5 and m[0] == InterpreterBase.METHOD_DEF


def is_field_statement(f) -> TypeGuard[FieldStatementType]:
    return is_statement(f) and len(f) == 4 and f[0] == InterpreterBase.FIELD_DEF


def is_class_members_type(c: tuple[MethodOrFieldType | StringWithLineNumber | StatementType, ...]) -> TypeGuard[ClassMembersType]:
    return all(is_method_statement(x) or is_field_statement(x) for x in c)


def is_class_statement(c) -> TypeGuard[ClassStatementType]:
    # method/field
    return is_statement(c) and len(c) >= 3 and c[0] == InterpreterBase.CLASS_DEF and is_StringWithLineNumber(c[1]) and (c[2] == InterpreterBase.INHERITS_DEF and is_StringWithLineNumber(c[3]) and is_class_members_type(c[4:]) or is_class_members_type(c[2:]))


def assignment_type_check(val: 'ValueDef', variable: 'VariableDef', err: Callable[[ErrorType], NoReturn]) -> None | NoReturn:
    if val.get_type() not in variable.allowed_types:
        err(ErrorType.TYPE_ERROR)

    if val.get_type() == Type.CLASS:
        actual_value = val.get_value()
        if actual_value is None:  # null.
            # todo. still needa check CLASS_NAME bc assigning unrelated null to other class isnt allowed
            pass
        else:
            assert isinstance(actual_value, ObjectDef)

        pass
        # todo, polymorphism

    return


class EnvironmentManager:
    """
    The EnvironmentManager class maintains the lexical environment for a construct.
    In project 1, this is just a mapping between each variable (aka symbol)
    in a brewin program and the value of that variable - the value that's passed in can be
    anything you like. In our implementation we pass in a Value object which holds a type
    and a value (e.g., Int, 10).
    """

    def __init__(self, interpreter: 'Interpreter', method: 'MethodDef'):
        self.__environments: list[dict[StringWithLineNumber, tuple[ValueDef, VariableDef]]] = [{}]
        self.interpreter = interpreter
        self.method = method

    def get(self, symbol: StringWithLineNumber) -> Union['ValueDef', None]:
        """
        Get data associated with variable name.
        """
        for env in reversed(self.__environments):
            if symbol in env:
                return env[symbol][0]

        return None
        # return self.__environments.get(symbol, Value(Type.NOTHING))

    def set(self, symbol: StringWithLineNumber, value: 'ValueDef', allowed_type: StringWithLineNumber) -> None:
        """
        Set data associated with a variable name.
        """
        assert is_StringWithLineNumber(allowed_type)
        allowed_types: set[Type] = set()
        allowed_classes: set[StringWithLineNumber] = set()
        type_mappings = {InterpreterBase.INT_DEF: Type.INT, InterpreterBase.BOOL_DEF: Type.BOOL, InterpreterBase.STRING_DEF: Type.STRING}
        if allowed_type not in type_mappings:
            allowed_types.add(Type.CLASS)
            allowed_classes.add(allowed_type)
        else:
            allowed_types.add(type_mappings[allowed_type])
        # TODO check type of value, set null class_name

        new_var = VariableDef(allowed_types, allowed_classes)
        assignment_type_check(value, new_var, self.interpreter.error)

        self.__environments[-1][symbol] = (value, new_var)

    def mutate(self, symbol: StringWithLineNumber, value: 'ValueDef') -> None:
        matched_env = None
        for env in reversed(self.__environments):
            if symbol in env:
                matched_env = env
                break

        assert matched_env is not None and symbol in matched_env
        matched_value, matched_variable = matched_env[symbol]
        assignment_type_check(value, matched_variable, self.interpreter.error)
        matched_value.mutate_value(value.get_value())


class Type(Enum):
    """Enum for all possible Brewin types."""
    # print(Type.INT.value)
    INT = 1
    BOOL = 2
    STRING = 3
    CLASS = 4
    NOTHING = 5


# Represents a value, which has a type and its value
class ValueDef:
    """A representation for a value that contains a type tag."""

    def __init__(self, value_type: Type, value: FieldValueType = None, **kwargs):
        # (value_type)
        self.__type = value_type
        self.__value = value
        if value_type == Type.CLASS:
            if value is None:
                self.class_name = StringWithLineNumber(InterpreterBase.NULL_DEF, 0)
            else:
                self.class_name = kwargs['class_name']
                assert is_StringWithLineNumber(self.class_name)
        else:
            self.class_name = None

    def get_type(self) -> Type:
        return self.__type

    def get_value(self) -> FieldValueType:
        return self.__value

    def mutate_value(self, val: FieldValueType) -> None:
        self.__value = val

    # def set2(self, other: Self):
    #     self.__type = other.get_type()
    #     self.__value = other.get_value()


class VariableDef:
    def __init__(self, allowed_types: set[Type], allowed_classes: set[StringWithLineNumber]):
        self.allowed_types = allowed_types
        self.allowed_classes = allowed_classes


def create_value(val: StringWithLineNumber, class_name: StringWithLineNumber = StringWithLineNumber('_', 0)) -> ValueDef | None:
    """
    Create a Value object from a Python value.
    """
    if val == InterpreterBase.TRUE_DEF:
        return ValueDef(Type.BOOL, True)
    if val == InterpreterBase.FALSE_DEF:
        return ValueDef(Type.BOOL, False)
    if val[0] == '"':
        return ValueDef(Type.STRING, val.strip('"'))
    if val.lstrip('-').isnumeric():
        return ValueDef(Type.INT, int(val))
    if val == InterpreterBase.NULL_DEF:
        return ValueDef(Type.CLASS, None, class_name=class_name)
    if val == InterpreterBase.NOTHING_DEF:
        return ValueDef(Type.NOTHING, None)
    # assert False
    return None


class MethodDef:
    """
    Wrapper struct for the definition of a member method.
    """

    def __init__(self, method_def: MethodStatementType):
        self.method_name = method_def[2]
        self.formal_params = method_def[3]
        self.code = method_def[4]
        self.return_type = method_def[1]


class FieldDef:
    """
    Wrapper struct for the definition of a member field.
    """

    def __init__(self, field_def: FieldStatementType):
        self.field_name = field_def[2]
        self.default_field_value = field_def[3]
        self.type = field_def[1]


class ClassDef:
    """
    Holds definition for a class:
        - list of fields (and default values)
        - list of methods

    class definition: [class classname [field1 field2 ... method1 method2 ...]]
    """

    def __init__(self, class_def: ClassStatementType, interpreter: 'Interpreter'):
        self.interpreter = interpreter
        self.name = class_def[1]
        self.__types = {InterpreterBase.INT_DEF, InterpreterBase.BOOL_DEF, InterpreterBase.STRING_DEF}
        if class_def[2] == InterpreterBase.INHERITS_DEF:
            members = class_def[4:]
            assert is_StringWithLineNumber(class_def[3])
            self.parent_class = class_def[3]
        else:
            members = class_def[2:]
            self.parent_class = StringWithLineNumber('', 0)
        assert is_class_members_type(members)
        self.__create_field_list(members)
        self.__create_method_list(members)

    def get_fields(self) -> ValuesView[FieldDef]:
        """
        Get a list of FieldDefs for *all* fields in the class.
        """
        return self.class_fields.values()

    def get_methods(self) -> ValuesView[MethodDef]:
        """
        Get a list of MethodDefs for *all* fields in the class.
        """
        return self.class_methods.values()

    def __create_field_list(self, class_body: ClassMembersType):
        self.class_fields: dict[StringWithLineNumber, FieldDef] = {}

        for member in class_body:
            if is_field_statement(member):
                if member[2] in self.class_fields:  # redefinition
                    self.interpreter.error(
                        ErrorType.NAME_ERROR,
                        "duplicate field " + member[1],
                        member[0].line_num,
                    )

                field_type = member[1]
                if field_type not in self.__types and field_type not in self.interpreter.class_index and field_type != self.name:
                    self.interpreter.error(ErrorType.TYPE_ERROR)
                self.class_fields[member[2]] = FieldDef(member)

    def __create_method_list(self, class_body: ClassMembersType):
        self.class_methods: dict[StringWithLineNumber, MethodDef] = {}
        for member in class_body:
            if is_method_statement(member):
                if member[2] in self.class_methods:  # redefinition
                    self.interpreter.error(
                        ErrorType.NAME_ERROR,
                        "duplicate method " + member[1],
                        member[0].line_num,
                    )
                return_type = member[1]
                if return_type != InterpreterBase.VOID_DEF and return_type not in self.__types and return_type not in self.interpreter.class_index:
                    self.interpreter.error(ErrorType.TYPE_ERROR)
                self.class_methods[member[2]] = MethodDef(member)
                # methods_defined_so_far.add(member[1])


class ObjectDef:
    STATUS_PROCEED = 0
    STATUS_RETURN = 1
    STATUS_NAME_ERROR = 2
    STATUS_TYPE_ERROR = 3

    def __init__(self, interpreter: 'Interpreter', class_def: ClassDef, trace_output):
        self.interpreter = interpreter  # objref to interpreter object. used to report errors, get input, produce output
        self.class_def = class_def  # take class body from 3rd+ list elements, e.g., ["class",classname", [classbody]]
        self.trace_output = trace_output
        self.__map_fields_to_values()
        self.__map_method_names_to_method_definitions()
        self.__create_map_of_operations_to_lambdas()  # sets up maps to facilitate binary and unary operations, e.g., (+ 5 6)

    def call_method(self, method_name: StringWithLineNumber, actual_params: tuple[ValueDef, ...], line_num_of_caller):
        """
        actual_params is a list of Value objects (all parameters are passed by value).

        The caller passes in the line number so we can properly generate an error message.
        The error is then generated at the source (i.e., where the call is initiated).
        """
        if method_name not in self.obj_methods:
            self.interpreter.error(
                ErrorType.NAME_ERROR,
                "unknown method " + method_name,
                line_num_of_caller,
            )
        method_info = self.obj_methods[method_name]
        if len(actual_params) != len(method_info.formal_params):
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                "invalid number of parameters in call to " + method_name,
                line_num_of_caller,
            )
        env = (
            EnvironmentManager(self.interpreter, method_info)
        )  # maintains lexical environment for function; just params for now
        # print(list(zip(method_info.formal_params, actual_params)))
        for formal, actual in zip(method_info.formal_params, actual_params):
            # assert is_StringWithLineNumber(formal)
            env.set(formal[1], actual, formal[0])
        # since each method has a single top-level statement, execute it.
        status, return_value = self.__execute_statement(env, method_info.code)
        # if the method explicitly used the (return expression) statement to return a value, then return that
        # value back to the caller

        return_type = env.method.return_type
        type_mappings = {InterpreterBase.INT_DEF: Type.INT, InterpreterBase.BOOL_DEF: Type.BOOL, InterpreterBase.STRING_DEF: Type.STRING, InterpreterBase.VOID_DEF: Type.NOTHING}

        if status == ObjectDef.STATUS_RETURN:
            if return_type in type_mappings:
                if type_mappings[return_type] != return_value.get_type():
                    self.interpreter.error(ErrorType.TYPE_ERROR)
            else:
                # todo check class type
                pass
            return return_value
        # The method didn't explicitly return a value, so return a value of type nothing
        # return Value(InterpreterBase.NOTHING_DEF)
        return_defaults = {InterpreterBase.INT_DEF: StringWithLineNumber('0', 0), InterpreterBase.BOOL_DEF: StringWithLineNumber(
            InterpreterBase.FALSE_DEF, 0), InterpreterBase.STRING_DEF: StringWithLineNumber('""', 0), InterpreterBase.VOID_DEF: StringWithLineNumber(InterpreterBase.NOTHING_DEF, 0)}
        if return_type in return_defaults:
            return create_value(return_defaults[return_type])

        # create default class object null
        return create_value(StringWithLineNumber(InterpreterBase.NULL_DEF, 0), return_type)

        # return ValueDef(Type.NOTHING)

    def __execute_statement(self, env: EnvironmentManager, code: StatementType):
        """
        returns (status_code, return_value) where:
        - status_code indicates if the next statement includes a return
            - if so, the current method should terminate
            - otherwise, the next statement in the method should run normally
        - return_value is a Value containing the returned value from the function
        """
        if self.trace_output or True:
            print(f"{code[0].line_num}: {code}")
        tok = code[0]
        if is_begin_statement(code):
            return self.__execute_begin(env, code)
        if is_set_statement(code):
            return self.__execute_set(env, code)
        if is_if_statement(code):
            return self.__execute_if(env, code)
        if is_call_expression(code):
            return self.__execute_call(env, code)
        if is_while_statement(code):
            return self.__execute_while(env, code)
        if is_return_expression(code):
            return self.__execute_return(env, code)
        if is_input_str_statement(code):
            return self.__execute_input(env, code, True)
        if is_input_int_statement(code):
            return self.__execute_input(env, code, False)
        if is_print_statement(code):
            return self.__execute_print(env, code)

        self.interpreter.error(
            ErrorType.SYNTAX_ERROR, "unknown statement " + str(tok), tok.line_num
        )

    # (begin (statement1) (statement2) ... (statementn))
    def __execute_begin(self, env: EnvironmentManager, code: BeginStatementType):
        for statement in code[1:]:
            status, return_value = self.__execute_statement(env, statement)
            if status == ObjectDef.STATUS_RETURN:
                return (
                    status,
                    return_value,
                )  # could be a valid return of a value or an error
        # if we run thru the entire block without a return, then just return proceed
        # we don't want the calling block to exit with a return
        return ObjectDef.STATUS_PROCEED, None

    # (call object_ref/me methodname param1 param2 param3)
    # where params are expressions, and expression could be a value, or a (+ ...)
    # statement version of a method call; there's also an expression version of a method call below
    def __execute_call(self, env: EnvironmentManager, code: CallExpressionType):
        assert is_StringWithLineNumber(code[0])
        return ObjectDef.STATUS_PROCEED, self.__execute_call_aux(
            env, code, code[0].line_num
        )

    # (set varname expression), where expression could be a value, or a (+ ...)
    def __execute_set(self, env: EnvironmentManager, code: SetStatementType):
        val = self.__evaluate_expression(env, code[2], code[0].line_num)
        self.__set_variable_aux(env, code[1], val, code[0].line_num)
        return ObjectDef.STATUS_PROCEED, None

    # (return expression) where expression could be a value, or a (+ ...)
    def __execute_return(self, env: EnvironmentManager, code: ReturnExpressionType):
        if len(code) == 1:
            # [return] with no return expression
            return ObjectDef.STATUS_RETURN, create_value(StringWithLineNumber(InterpreterBase.NOTHING_DEF, 0))
        return ObjectDef.STATUS_RETURN, self.__evaluate_expression(
            env, code[1], code[0].line_num
        )

    # (print expression1 expression2 ...) where expression could be a variable, value, or a (+ ...)
    def __execute_print(self, env: EnvironmentManager, code: PrintStatementType):
        output = ""
        assert is_StringWithLineNumber(code[0])
        for expr in code[1:]:
            # TESTING NOTE: Will not test printing of object references
            term = self.__evaluate_expression(env, expr, code[0].line_num)
            val = term.get_value()
            typ = term.get_type()
            if typ == Type.BOOL:
                val = "true" if val else "false"
            # document - will never print out an object ref
            output += str(val)
        self.interpreter.output(output)
        print(f'output: {output}')
        return ObjectDef.STATUS_PROCEED, None

    # (inputs target_variable) or (inputi target_variable) sets target_variable to input string/int
    def __execute_input(self, env: EnvironmentManager, code: InputIntStatementType, get_string: bool):
        inp = self.interpreter.get_input()
        assert isinstance(inp, str)
        if get_string:
            val = ValueDef(Type.STRING, inp)
        else:
            val = ValueDef(Type.INT, int(inp))

        self.__set_variable_aux(env, code[1], val, code[0].line_num)
        return ObjectDef.STATUS_PROCEED, None

    # helper method used to set either parameter variables or member fields; parameters currently shadow
    # member fields
    def __set_variable_aux(self, env: EnvironmentManager, var_name: StringWithLineNumber, value: ValueDef, line_num):
        # parameter shadows fields
        if value.get_type() == Type.NOTHING:
            self.interpreter.error(
                ErrorType.TYPE_ERROR, "can't assign to nothing " + var_name, line_num
            )
        param_val = env.get(var_name)
        if param_val is not None:  # in params
            # todo: check type. if class type and no class_name, set it here
            env.mutate(var_name, value)
            return

        if var_name not in self.obj_fields:
            self.interpreter.error(
                ErrorType.NAME_ERROR, "unknown variable " + var_name, line_num
            )
        field_var = self.obj_fields[var_name][1]
        assignment_type_check(value, field_var, self.interpreter.error)

        self.obj_fields[var_name][0].mutate_value(value.get_value())

    # (if expression (statement) (statement) ) where expression could be a boolean constant (e.g., true), member
    # variable without ()s, or a boolean expression in parens, like (> 5 a)
    def __execute_if(self, env: EnvironmentManager, code: IfStatementType):
        condition = self.__evaluate_expression(env, code[1], code[0].line_num)
        if condition.get_type() != Type.BOOL:
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                "non-boolean if condition " + ' '.join(x for x in code[1]),  # type: ignore
                code[0].line_num,
            )
        if condition.get_value():
            status, return_value = self.__execute_statement(
                env, code[2]
            )  # if condition was true
            return status, return_value
        if len(code) == 4:
            status, return_value = self.__execute_statement(
                env, code[3]
            )  # if condition was false, do else
            return status, return_value
        return ObjectDef.STATUS_PROCEED, None

    # (while expression (statement) ) where expression could be a boolean value, boolean member variable,
    # or a boolean expression in parens, like (> 5 a)
    def __execute_while(self, env: EnvironmentManager, code: WhileStatementType) -> tuple:
        while True:
            condition = self.__evaluate_expression(env, code[1], code[0].line_num)
            if condition.get_type() != Type.BOOL:
                self.interpreter.error(
                    ErrorType.TYPE_ERROR,
                    "non-boolean while condition " + ' '.join(x for x in code[1]),  # type: ignore
                    code[0].line_num,
                )
            if not condition.get_value():  # condition is false, exit loop immediately
                return ObjectDef.STATUS_PROCEED, None
            # condition is true, run body of while loop
            status, return_value = self.__execute_statement(env, code[2])
            if status == ObjectDef.STATUS_RETURN:
                return (
                    status,
                    return_value,
                )  # could be a valid return of a value or an error

    # given an expression, return a Value object with the expression's evaluated result
    # expressions could be: constants (true, 5, "blah"), variables (e.g., x), arithmetic/string/logical expressions
    # like (+ 5 6), (+ "abc" "def"), (> a 5), method calls (e.g., (call me foo)), or instantiations (e.g., new dog_class)
    def __evaluate_expression(self, env: EnvironmentManager, expr: ExpressionType, line_num_of_statement) -> ValueDef:
        if not isinstance(expr, tuple):
            assert is_StringWithLineNumber(expr)
            # locals shadow member variables
            val = env.get(expr)
            if val is not None:
                return val
            if expr in self.obj_fields:
                return self.obj_fields[expr][0]
            # need to check for variable name and get its value too
            value = create_value(expr)
            if value is not None:
                return value
            self.interpreter.error(
                ErrorType.NAME_ERROR,
                "invalid field or parameter " + expr,
                line_num_of_statement,
            )

        operator = expr[0]
        assert is_statement(expr) and is_StringWithLineNumber(operator)
        if operator in self.binary_op_list:
            operand1 = self.__evaluate_expression(env, expr[1], line_num_of_statement)
            operand2 = self.__evaluate_expression(env, expr[2], line_num_of_statement)
            if operand1.get_type() == operand2.get_type() and operand1.get_type() == Type.INT:
                if operator not in self.binary_ops[Type.INT]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid operator applied to ints",
                        line_num_of_statement,
                    )
                return self.binary_ops[Type.INT][operator](operand1, operand2)
            if operand1.get_type() == operand2.get_type() and operand1.get_type() == Type.STRING:
                if operator not in self.binary_ops[Type.STRING]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid operator applied to strings",
                        line_num_of_statement,
                    )
                return self.binary_ops[Type.STRING][operator](operand1, operand2)
            if operand1.get_type() == operand2.get_type() and operand1.get_type() == Type.BOOL:
                if operator not in self.binary_ops[Type.BOOL]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid operator applied to bool",
                        line_num_of_statement,
                    )
                return self.binary_ops[Type.BOOL][operator](operand1, operand2)
            if operand1.get_type() == operand2.get_type() and operand1.get_type() == Type.CLASS:
                if operator not in self.binary_ops[Type.CLASS]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid operator applied to class",
                        line_num_of_statement,
                    )
                return self.binary_ops[Type.CLASS][operator](operand1, operand2)
            # error what about an obj reference and null
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                f"operator {operator} applied to two incompatible types",
                line_num_of_statement,
            )
        if operator in self.unary_op_list:
            operand = self.__evaluate_expression(env, expr[1], line_num_of_statement)
            if operand.get_type() == Type.BOOL:
                if operator not in self.unary_ops[Type.BOOL]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid unary operator applied to bool",
                        line_num_of_statement,
                    )
                return self.unary_ops[Type.BOOL][operator](operand)

        # handle call expression: (call objref methodname p1 p2 p3)
        if is_call_expression(expr):
            return self.__execute_call_aux(env, expr, line_num_of_statement)
        # handle new expression: (new classname)
        if is_new_expression(expr):
            return self.__execute_new_aux(env, expr, line_num_of_statement)
        raise Exception("shouldn't be reached")

    # (new classname)
    def __execute_new_aux(self, _, code: NewExpressionType, line_num_of_statement):
        obj = self.interpreter.instantiate(code[1], line_num_of_statement)
        return ValueDef(Type.CLASS, obj, class_name=code[1])

    # this method is a helper used by call statements and call expressions
    # (call object_ref/me methodname p1 p2 p3)
    def __execute_call_aux(self, env: EnvironmentManager, code: CallExpressionType, line_num_of_statement):
        # determine which object we want to call the method on
        obj_name = code[1]
        if obj_name == InterpreterBase.ME_DEF:
            obj = self
        else:
            obj = self.__evaluate_expression(
                env, obj_name, line_num_of_statement
            ).get_value()
        # prepare the actual arguments for passing
        if obj is None:
            self.interpreter.error(
                ErrorType.FAULT_ERROR, "null dereference", line_num_of_statement
            )
        assert isinstance(obj, ObjectDef)
        actual_args = tuple(map(lambda expr: self.__evaluate_expression(env, expr, line_num_of_statement), code[3:]))
        assert is_StringWithLineNumber(code[2])
        return obj.call_method(code[2], actual_args, line_num_of_statement)

    def __map_method_names_to_method_definitions(self):
        self.obj_methods: dict[StringWithLineNumber, MethodDef] = {}
        for method in self.class_def.get_methods():
            self.obj_methods[method.method_name] = method

    def __map_fields_to_values(self):
        self.obj_fields: dict[StringWithLineNumber, tuple[ValueDef, VariableDef]] = {}
        for field in self.class_def.get_fields():
            type_mappings = {InterpreterBase.INT_DEF: Type.INT, InterpreterBase.BOOL_DEF: Type.BOOL, InterpreterBase.STRING_DEF: Type.STRING}
            allowed_types: set[Type] = set()
            allowed_classes: set[StringWithLineNumber] = set()

            if field.type not in type_mappings:
                allowed_types.add(Type.CLASS)
                allowed_classes.add(field.type)
            else:
                allowed_types.add(type_mappings[field.type])
            field_val = create_value(field.default_field_value, field.type)
            assert field_val is not None
            field_variable = VariableDef(allowed_types, allowed_classes)
            assignment_type_check(field_val, field_variable, self.interpreter.error)
            self.obj_fields[field.field_name] = (field_val, field_variable)

    def __create_map_of_operations_to_lambdas(self):
        self.binary_op_list = [
            "+",
            "-",
            "*",
            "/",
            "%",
            "==",
            "!=",
            "<",
            "<=",
            ">",
            ">=",
            "&",
            "|",
        ]
        self.unary_op_list = ["!"]
        self.binary_ops = {}  # : dict[Type, dict[str, Callable[[Value, Value], Value]]]
        self.binary_ops[Type.INT] = {
            "+": lambda a, b: ValueDef(Type.INT, a.get_value() + b.get_value()),
            "-": lambda a, b: ValueDef(Type.INT, a.get_value() - b.get_value()),
            "*": lambda a, b: ValueDef(Type.INT, a.get_value() * b.get_value()),
            "/": lambda a, b: ValueDef(
                Type.INT, a.get_value() // b.get_value()
            ),  # // for integer ops
            "%": lambda a, b: ValueDef(Type.INT, a.get_value() % b.get_value()),
            "==": lambda a, b: ValueDef(Type.BOOL, a.get_value() == b.get_value()),
            "!=": lambda a, b: ValueDef(Type.BOOL, a.get_value() != b.get_value()),
            ">": lambda a, b: ValueDef(Type.BOOL, a.get_value() > b.get_value()),
            "<": lambda a, b: ValueDef(Type.BOOL, a.get_value() < b.get_value()),
            ">=": lambda a, b: ValueDef(Type.BOOL, a.get_value() >= b.get_value()),
            "<=": lambda a, b: ValueDef(Type.BOOL, a.get_value() <= b.get_value()),
        }
        self.binary_ops[Type.STRING] = {
            "+": lambda a, b: ValueDef(Type.STRING, a.get_value() + b.get_value()),
            "==": lambda a, b: ValueDef(Type.BOOL, a.get_value() == b.get_value()),
            "!=": lambda a, b: ValueDef(Type.BOOL, a.get_value() != b.get_value()),
            ">": lambda a, b: ValueDef(Type.BOOL, a.get_value() > b.get_value()),
            "<": lambda a, b: ValueDef(Type.BOOL, a.get_value() < b.get_value()),
            ">=": lambda a, b: ValueDef(Type.BOOL, a.get_value() >= b.get_value()),
            "<=": lambda a, b: ValueDef(Type.BOOL, a.get_value() <= b.get_value()),
        }
        self.binary_ops[Type.BOOL] = {
            "&": lambda a, b: ValueDef(Type.BOOL, a.get_value() and b.get_value()),
            "|": lambda a, b: ValueDef(Type.BOOL, a.get_value() or b.get_value()),
            "==": lambda a, b: ValueDef(Type.BOOL, a.get_value() == b.get_value()),
            "!=": lambda a, b: ValueDef(Type.BOOL, a.get_value() != b.get_value()),
        }
        self.binary_ops[Type.CLASS] = {
            "==": lambda a, b: ValueDef(Type.BOOL, a.get_value() == b.get_value()),
            "!=": lambda a, b: ValueDef(Type.BOOL, a.get_value() != b.get_value()),
        }

        self.unary_ops: dict[Type, dict[str, Callable[[ValueDef], ValueDef]]] = {}
        self.unary_ops[Type.BOOL] = {
            "!": lambda a: ValueDef(Type.BOOL, not a.get_value()),
        }


def deeptuple(l: list) -> tuple:
    def deep_tuple_helper(x):
        if isinstance(x, StringWithLineNumber):
            return x
        if isinstance(x, list):
            return tuple(deep_tuple_helper(x) for x in x)
        return x
    return tuple(deep_tuple_helper(x) for x in l)


class Interpreter(InterpreterBase):
    """
    Main interpreter class that subclasses InterpreterBase.
    """

    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self.trace_output = trace_output
        self.main_object = None
        self.class_index = {}

    def run(self, program):
        """
        Run a program (an array of strings, where each item is a line of source code).
        Delegates parsing to the provided BParser class in bparser.py.
        """
        status, parsed_program = BParser.parse(program)
        if not status:
            super().error(
                ErrorType.SYNTAX_ERROR, f"Parse error on program: {parsed_program}"
            )
        assert isinstance(parsed_program, list)
        parsed_program = deeptuple(parsed_program)
        self.__map_class_names_to_class_defs(parsed_program)

        # instantiate main class
        invalid_line_num_of_caller = None
        self.main_object = self.instantiate(
            StringWithLineNumber(InterpreterBase.MAIN_CLASS_DEF, 0), invalid_line_num_of_caller
        )

        # call main function in main class; return value is ignored from main
        self.main_object.call_method(
            StringWithLineNumber(InterpreterBase.MAIN_FUNC_DEF, 0), (), invalid_line_num_of_caller
        )

        # program terminates!

    def instantiate(self, class_name: StringWithLineNumber, line_num_of_statement):
        """
        Instantiate a new class. The line number is necessary to properly generate an error
        if a `new` is called with a class name that does not exist.
        This reports the error where `new` is called.
        """
        if class_name not in self.class_index:
            super().error(
                ErrorType.TYPE_ERROR,
                f"No class named {class_name} found",
                line_num_of_statement,
            )
        class_def = self.class_index[class_name]
        obj = ObjectDef(
            self, class_def, self.trace_output
        )  # Create an object based on this class definition
        return obj

    def __map_class_names_to_class_defs(self, program: ParsedProgramType):
        self.class_index: dict[StringWithLineNumber, ClassDef] = {}
        for item in program:
            if is_class_statement(item):
                assert is_StringWithLineNumber(item[0])
                if item[1] in self.class_index:
                    super().error(
                        ErrorType.TYPE_ERROR,
                        f"Duplicate class name {item[1]}",
                        item[0].line_num,
                    )
                assert is_StringWithLineNumber(item[1])
                self.class_index[item[1]] = ClassDef(item, self)


def main():
    print(StringWithLineNumber('bool', 343) in {InterpreterBase.INT_DEF, InterpreterBase.BOOL_DEF, InterpreterBase.STRING_DEF}, 'ran interpreter, no tests')


if __name__ == '__main__':
    main()
