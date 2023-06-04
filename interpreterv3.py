from bparser import BParser, StringWithLineNumber
import copy
from intbase import InterpreterBase, ErrorType


class EnvironmentManager:
    """
    The EnvironmentManager class keeps a mapping between each variable name (aka symbol)
    in a brewin program and the VariableDef object, which stores the variable name, its type, and
    the current value that the variable is set to (which could be the same type or a subtype of
    the variable, in the case of object references).
    """

    def __init__(self):
        self.environment = [{}]

    # returns a VariableDef object
    def get(self, symbol):
        for env in reversed(self.environment):
            if symbol in env:
                return env[symbol]

        return None

    # create a new symbol in the most nested block's environment; error if
    # the symbol already exists in the most nested block
    # this is called for all variables defined in a let block or in formal parameters
    def create_new_symbol(self, symbol):
        if symbol not in self.environment[-1]:
            self.environment[-1][symbol] = None
            return True

        return False

    # set works with symbols that were already created via create_new_symbol().
    # this won't create a new symbol, only update its value to a new Value object
    # returns False if the set failed due to an unknown variable that wasn't found in any
    # of the nested blocks of the current function
    def set(self, symbol, value):
        for env in reversed(self.environment):
            if symbol in env:
                env[symbol] = value
                return True

        return False

    # used when we enter a nested block to create a new environment for that block
    def block_nest(self):
        self.environment.append({})  # [{}] -> [{}, {}]

    # used when we exit a nested block to discard the environment for that block
    def block_unnest(self):
        self.environment.pop()


# Enumerated type for our different language data types
class Type:
    def __init__(self, type_name, supertype_name=None):
        self.type_name = type_name
        self.supertype_name = supertype_name

    def __eq__(self, other):
        return (
            self.type_name == other.type_name
            and self.supertype_name == other.supertype_name
        )


# Represents a value, which has a type and its value
class Value:
    def __init__(self, type_obj, value=None):
        self.t = type_obj
        self.v = value

    def value(self):
        return self.v

    def set(self, other):
        self.t = other.t
        self.v = other.v

    def type(self):
        return self.t

    def is_null(self):
        return self.v == None and self.t != Type(InterpreterBase.NOTHING_DEF)

    def is_typeless_null(self):
        return self.v == None and self.t == Type(InterpreterBase.NULL_DEF)

    def __eq__(self, other):
        return self.t == other.t and self.v == other.v


# val is a string with the value we want to use to construct a Value object.
# e.g., '1234' 'null' 'true' '"foobar"'
def create_value(val):
    if val == InterpreterBase.TRUE_DEF:
        return Value(Type(InterpreterBase.BOOL_DEF), True)
    elif val == InterpreterBase.FALSE_DEF:
        return Value(Type(InterpreterBase.BOOL_DEF), False)
    elif val[0] == '"':
        return Value(Type(InterpreterBase.STRING_DEF), val.strip('"'))
    elif val.lstrip('-').isnumeric():
        return Value(Type(InterpreterBase.INT_DEF), int(val))
    elif val == InterpreterBase.NULL_DEF:
        return Value(Type(InterpreterBase.NULL_DEF), None)
    else:
        return None


# create a default value of the specified type; type_def is a Type object
def create_default_value(type_def):
    if type_def == Type(InterpreterBase.BOOL_DEF):
        return Value(Type(InterpreterBase.BOOL_DEF), False)
    elif type_def == Type(InterpreterBase.STRING_DEF):
        return Value(Type(InterpreterBase.STRING_DEF), "")
    elif type_def == Type(InterpreterBase.INT_DEF):
        return Value(Type(InterpreterBase.INT_DEF), 0)
    elif type_def == Type(
        InterpreterBase.NOTHING_DEF
    ):  # used for void return type on methods
        return Value(Type(InterpreterBase.NOTHING_DEF), None)
    else:
        return Value(
            type_def, None
        )  # the type is a class type, so we return null for default val, with proper class type


# Used to track user-defined types (for classes) as well as check for type compatibility between
# values of same/different types for assignment/comparison
class TypeManager:
    def __init__(self, interpreter: 'Interpreter'):
        self.map_typename_to_type = {}
        self.__setup_primitive_types()
        self.template_classes = {}
        self.interpreter = interpreter

    def add_template_class_type(self, statement):
        self.template_classes[statement[1]] = statement

    def create_template_class_type(self, declaration):
        if self.is_valid_type(declaration):
            return True
        template_class, *types = declaration.split(InterpreterBase.TYPE_CONCAT_CHAR)
        # [template_class, ...types] =
        for t in types:
            if not self.is_valid_type(t):
                self.interpreter.error(ErrorType.TYPE_ERROR, f'invalid type: {t}')

        # if template_class not in self.template_classes:
        # throw error

        original_statement = self.template_classes[template_class]
        # deepcopy statement with replaced strings
        copied_statement = copy.deepcopy(original_statement)
        if len(types) != len(copied_statement[2]):
            self.interpreter.error(ErrorType.TYPE_ERROR)

        def replace_all(s):
            if isinstance(s, list):
                if s[0] == InterpreterBase.TEMPLATE_CLASS_DEF:
                    for i in range(len(s[2])):
                        s[2][i] = replace_all(s[2][i])
                    for i in range(3, len(s)):
                        s[i] = replace_all(s[i])

                elif s[0] == InterpreterBase.FIELD_DEF or s[0] == InterpreterBase.NEW_DEF:
                    s[1] = replace_all(s[1])
                elif s[0] == InterpreterBase.METHOD_DEF:
                    s[1] = replace_all(s[1])
                    for i in range(len(s[3])):
                        s[3][i][0] = replace_all(s[3][i][0])
                    s[4] = replace_all(s[4])

                elif s[0] == InterpreterBase.LET_DEF:
                    for i in range(len(s[1])):
                        s[1][i][0] = replace_all(s[1][i][0])
                    for i in range(2, len(s)):
                        s[i] = replace_all(s[i])
                else:
                    for i in range(len(s)):
                        if isinstance(s[i], list):
                            s[i] = replace_all(s[i])
            elif isinstance(s, (StringWithLineNumber, str)):
                if s in original_statement[2]:
                    ind = original_statement[2].index(s)
                    s = StringWithLineNumber(types[ind], 0)
                elif InterpreterBase.TYPE_CONCAT_CHAR in s:
                    t_class, *type_args = s.split(InterpreterBase.TYPE_CONCAT_CHAR)
                    new_str = t_class + InterpreterBase.TYPE_CONCAT_CHAR + InterpreterBase.TYPE_CONCAT_CHAR.join(list(map(replace_all, type_args)))
                    s = StringWithLineNumber(new_str, 0)
            else:
                raise 'unexpected type in tclass statement'
            return s
        replace_all(copied_statement)
        print('\n',original_statement, '\n\n', copied_statement)

        self.add_class_type(declaration, None)

        self.interpreter.class_index[declaration] = ClassDef(copied_statement, self.interpreter)
        return

    # used to register a new class name (and its supertype name, if present as a valid type so it can be used
    # for type checking.
    # needs to be called the moment we parse the class name and superclass name to enable things like linked lists
    # and other self-referential structures
    def add_class_type(self, class_name, superclass_name):
        class_type = Type(class_name, superclass_name)
        self.map_typename_to_type[class_name] = class_type

    def is_valid_type(self, typename):
        return typename in self.map_typename_to_type

    # return Type object for specified typename string
    def get_type_info(self, typename):
        if not self.is_valid_type(typename):
            return None
        return self.map_typename_to_type[typename]

    # args are strings
    def is_a_subtype(self, suspected_supertype, suspected_subtype):
        if not self.is_valid_type(suspected_supertype) or not self.is_valid_type(
            suspected_subtype
        ):
            return False
        cur_type = suspected_subtype
        while True:
            if (
                suspected_supertype == cur_type
            ):  # passing a Student object to a Student parameter
                return True
            type_info = self.get_type_info(cur_type)
            if type_info.supertype_name is None:
                return False
            cur_type = (
                type_info.supertype_name  # check suspected supertype is in the inheritance chain
            )  # check the base class of the subtype next

    # typea and typeb are Type objects
    def check_type_compatibility(self, typea, typeb, for_assignment):
        # if either type is invalid (E.g., the user referenced a class name that doesn't exist) then
        # return false
        if not self.is_valid_type(typea.type_name) or not self.is_valid_type(
            typeb.type_name
        ):
            return False
        # if a is a supertype of b, then the types are compatible
        if self.is_a_subtype(
            typea.type_name, typeb.type_name
        ):  # animal = person or animal == person
            return True
        # if b is a supertype of a, and we're not doing assignment then the types are compatible
        if not for_assignment and self.is_a_subtype(
            typeb.type_name, typea.type_name
        ):  # person == animal
            return True
        # if the types are identical then they're compatible
        if typea == typeb:
            return True
        # if either is a primitive type, but the types aren't the same, they can't match
        if (
            typea.type_name in self.primitive_types
            or typeb.type_name in self.primitive_types
        ):
            return False
        # by the time we get here, the types must be class types and not primitives
        # check for one or both of the types to be the null type, in which the types are compatible
        # e.g., setting an object reference to null, or comparing two obj references
        if (
            typea.type_name == InterpreterBase.NULL_DEF
            or typeb.type_name == InterpreterBase.NULL_DEF
        ):
            return True
        # all other cases
        return False

    # add our primitive types to our map of valid types
    def __setup_primitive_types(self):
        self.primitive_types = {
            InterpreterBase.INT_DEF,
            InterpreterBase.STRING_DEF,
            InterpreterBase.BOOL_DEF,
        }
        self.map_typename_to_type[InterpreterBase.INT_DEF] = Type(
            InterpreterBase.INT_DEF
        )
        self.map_typename_to_type[InterpreterBase.STRING_DEF] = Type(
            InterpreterBase.STRING_DEF
        )
        self.map_typename_to_type[InterpreterBase.BOOL_DEF] = Type(
            InterpreterBase.BOOL_DEF
        )
        self.map_typename_to_type[InterpreterBase.NULL_DEF] = Type(
            InterpreterBase.NULL_DEF
        )
# pylint: disable=too-few-public-methods


"""
# v2
- inheritance
  (class foo inherits bar ...)
  dynamic dispatch
  polymorphism
- static typing
.  update formal params to use VariableDef instead of just strings
.  check parameter type compatability on calls
.  change syntax for method definitions to:
   (method method_name ((type1 param1) (type2 param2) ...) (statement))
.  change MethodDef class to store typename and Type.??? instead of just strings for formal params
.  create new let statement, which is just like begin except it has locals
   (let ((type1 param1 val1) (type2 param2 val2)) (statement1) (statement2))
.  update environment to scope variables by block
.  update code to ensure variables go out of scope at end of block
.  change class syntax for field definitions:
   (field type name init_value)
.  update FieldDef class to support types of fields
.  need to support class names for types
.  update variable assignments to ensure types are consistent
.  update parameter passing code to make sure actual and formal args are consistent types
   . must handle polymorphism (passing subtype to f(supertype))
.  update overload checking code to check not only by # of parameters but by types in inheritance
.  have return type for methods
.  update return code to check return type of returned value
.  add void return type for methods that don't return a value
.  update method completion code to return default value (0, False, "", null) if no returned value
.  add test cases for returning a subclass (pass) of the return type, and a superclass (fail)
.  test for duplicate formal param names and generate error
.  test for invalid param types and return types
.  propagate type to null during return and when assigned to variable so we can't compare
   a null pointer of type person to a null pointer of type robot
"""


class VariableDef:
    # var_type is a Type() and value is a Value()
    def __init__(self, var_type, var_name, value=None):
        self.type = var_type
        self.name = var_name
        self.value = value

    def set_value(self, value):
        self.value = value


# parses and holds the definition of a member method
# [method return_type method_name [[type1 param1] [type2 param2] ...] [statement]]
class MethodDef:
    def __init__(self, method_source):
        self.line_num = method_source[0].line_num  # used for errors
        self.method_name = method_source[2]
        if method_source[1] == InterpreterBase.VOID_DEF:
            self.return_type = Type(InterpreterBase.NOTHING_DEF)
        else:
            self.return_type = Type(method_source[1])
        self.formal_params = self.__parse_params(method_source[3])
        self.code = method_source[4]

    def get_method_name(self):
        return self.method_name

    def get_formal_params(self):
        return self.formal_params

    # returns a Type()
    def get_return_type(self):
        return self.return_type

    def get_code(self):
        return self.code

    # input params in the form of [[type1 param1] [type2 param2] ...]
    # output is a set of VariableDefs
    def __parse_params(self, params):
        formal_params = []
        for param in params:
            var_def = VariableDef(Type(param[0]), param[1])
            formal_params.append(var_def)
        return formal_params


# holds definition for a class, including a list of all the fields and their default values, all
# of the methods in the class, and the superclass information (if any)
# v2 class definition: [class classname [inherits baseclassname] [field1] [field2] ... [method1] [method2] ...]
# [] denotes optional syntax
class ClassDef:
    def __init__(self, class_source, interpreter: 'Interpreter'):
        self.interpreter = interpreter
        self.name = class_source[1]
        self.class_source = class_source
        fields_and_methods_start_index = (
            self.__check_for_inheritance_and_set_superclass_info(class_source)
        )
        self.__create_field_list(class_source[fields_and_methods_start_index:])
        self.__create_method_list(class_source[fields_and_methods_start_index:])

    # get the classname
    def get_name(self):
        return self.name

    # get a list of FieldDef objects for all fields in the class
    def get_fields(self):
        return self.fields

    # get a list of MethodDef objects for all methods in the class
    def get_methods(self):
        return self.methods

    # returns a ClassDef object
    def get_superclass(self):
        return self.super_class

    def __check_for_inheritance_and_set_superclass_info(self, class_source):
        if class_source[2] != InterpreterBase.INHERITS_DEF:
            self.super_class = None
            return 2  # fields and method definitions start after [class classname ...], jump to the correct place to continue parsing

        super_class_name = class_source[3]
        self.super_class = self.interpreter.get_class_def(
            super_class_name, class_source[0].line_num
        )
        return 4  # fields and method definitions start after [class classname inherits baseclassname ...]

    def __create_field_list(self, class_body):
        self.fields = []  # array of VariableDefs with default values set
        self.field_map = {}
        fields_defined_so_far = set()
        for member in class_body:
            # member format is [field typename varname default_value]
            if member[0] == InterpreterBase.FIELD_DEF:
                if member[2] in fields_defined_so_far:  # redefinition
                    self.interpreter.error(
                        ErrorType.NAME_ERROR,
                        "duplicate field " + member[2],
                        member[0].line_num,
                    )
                var_def = self.__create_variable_def_from_field(member)
                self.fields.append(var_def)
                self.field_map[member[2]] = var_def
                fields_defined_so_far.add(member[2])

    # field def: [field typename varname defvalue]
    # returns a VariableDef object that represents that field
    def __create_variable_def_from_field(self, field_def):
        var_def = VariableDef(
            Type(field_def[1]), field_def[2], create_value(field_def[3]) if len(field_def) >= 4 else create_default_value(Type(field_def[1]))
        )
        if InterpreterBase.TYPE_CONCAT_CHAR in field_def[1]:
            self.interpreter.type_manager.create_template_class_type(field_def[1])
        if not self.interpreter.check_type_compatibility(
            var_def.type, var_def.value.type(), True
        ):
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                "invalid type/type mismatch with field " + field_def[2],
                field_def[0].line_num,
            )
        return var_def

    def __create_method_list(self, class_body):
        self.methods = []
        self.method_map = {}
        methods_defined_so_far = set()
        for member in class_body:
            if member[0] == InterpreterBase.METHOD_DEF:
                method_def = MethodDef(member)
                if method_def.method_name in methods_defined_so_far:  # redefinition
                    self.interpreter.error(
                        ErrorType.NAME_ERROR,
                        "duplicate method " + method_def.method_name,
                        member[0].line_num,
                    )
                self.__check_method_names_and_types(method_def)
                self.methods.append(method_def)
                self.method_map[method_def.method_name] = method_def
                methods_defined_so_far.add(method_def.method_name)

    # for a given method, make sure that the parameter types are valid, return type is valid, and param names
    # are not duplicated
    def __check_method_names_and_types(self, method_def):
        if not self.interpreter.is_valid_type(
            method_def.return_type.type_name
        ) and method_def.return_type != Type(InterpreterBase.NOTHING_DEF):  # checks that return type isn't a defined type or void
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                "invalid return type for method " + method_def.method_name,
                method_def.line_num,
            )
        used_param_names = set()
        for param in method_def.formal_params:
            if param.name in used_param_names:
                self.interpreter.error(
                    ErrorType.NAME_ERROR,
                    "duplicate formal parameter " + param.name,
                    method_def.line_num,
                )
            if not self.interpreter.is_valid_type(param.type.type_name):
                self.interpreter.error(
                    ErrorType.TYPE_ERROR,
                    "invalid type for parameter " + param.name,
                    method_def.line_num,
                )


class ObjectDef:
    # statement execution results
    STATUS_PROCEED = 0
    STATUS_RETURN = 1

    # type constants
    INT_TYPE_CONST = Type(InterpreterBase.INT_DEF)
    STRING_TYPE_CONST = Type(InterpreterBase.STRING_DEF)
    BOOL_TYPE_CONST = Type(InterpreterBase.BOOL_DEF)

    # class_def is a ClassDef object
    def __init__(self, interpreter: 'Interpreter', class_def, anchor_object=None, trace_output=False):
        self.interpreter = interpreter  # objref to interpreter object. used to report errors, get input, produce output
        self.class_def = class_def

        if anchor_object is None:
            self.anchor_object = self
        else:
            self.anchor_object = anchor_object
        self.trace_output = trace_output
        self.__instantiate_fields()
        self.__map_method_names_to_method_definitions()
        self.__create_map_of_operations_to_lambdas()  # sets up maps to facilitate binary and unary operations, e.g., (+ 5 6)
        self.__init_superclass_if_any()  # construct default values for superclass fields all the way to the base class

    def __get_obj_with_method(self, start_obj, method_name, actual_params):
        cur_obj = start_obj
        while cur_obj is not None:
            if method_name not in cur_obj.methods:
                cur_obj = cur_obj.super_object
                continue
            method_def = cur_obj.methods[method_name]
            if len(actual_params) == len(
                method_def.formal_params
            ) and self.__compatible_param_types(
                actual_params, method_def.formal_params
            ):
                break

            cur_obj = cur_obj.super_object

        return cur_obj

    # actual_params is a list of Value objects; all parameters are passed by value
    # the caller passes in its line number so if there's an error (e.g., mismatched # of parameters or unknown
    # method name) we can generate an error at the source (where the call is initiated) for better context
    def call_method(self, method_name, actual_params, super_only, line_num_of_caller):
        # check to see if we have a method in this class or its base class(es) matching this signature
        if self.__get_obj_with_method(self, method_name, actual_params) is None:
            self.interpreter.error(
                ErrorType.NAME_ERROR,
                "unknown method " + method_name,
                line_num_of_caller,
            )

        # Yes, we have a method with the right name/parameters known to this class or its base classes...
        # So now find the proper version of the method in the most-derived class, which may be in a derived class
        # of this class!  Start from the anchor object (most derived part of the object) and search for the most
        # derived object part that has this method.
        if super_only:
            anchor = self
        else:
            anchor = self.anchor_object
        obj_to_call_on = self.__get_obj_with_method(anchor, method_name, actual_params)

        method_def = obj_to_call_on.methods[method_name]

        # handle the call in the object
        env = (
            EnvironmentManager()
        )  # maintains lexical environment for function; just params for now
        for formal, actual in zip(method_def.formal_params, actual_params):
            formal_copy = copy.copy(formal)  # VariableDef obj.
            formal_copy.set_value(actual)  # actual is a Value obj.
            if not env.create_new_symbol(formal_copy.name):
                self.interpreter.error(
                    ErrorType.NAME_ERROR,
                    "duplicate formal param name " + formal.name,
                    method_def.line_num,
                )
            env.set(formal_copy.name, formal_copy)
        # since each method has a single top-level statement, execute it.
        status, return_value = obj_to_call_on.__execute_statement(
            env, method_def.return_type, method_def.code
        )
        # if the method explicitly used the (return expression) statement to return a value, then return that
        # value back to the caller
        if status == ObjectDef.STATUS_RETURN and return_value is not None:
            return return_value
        # The method didn't explicitly return a value, so return the default return type for the method
        return create_default_value(method_def.get_return_type())

    # def get_me_as_value(self):
    #     return Value(Type(self.class_def.name), self)

    def get_me_as_value(self):
        anchor = self.anchor_object
        return Value(Type(anchor.class_def.name), anchor)

    # checks whether each formal parameter has a compatible type with the actual parameter
    def __compatible_param_types(self, actual_params, formal_params):
        for formal, actual in zip(formal_params, actual_params):
            if not self.interpreter.check_type_compatibility(
                formal.type, actual.type(), True
            ):
                return False
        return True

    # returns (status_code, return_value) where:
    # - status_code indicates whether the statement (or one of its sub-statements) executed a return command and thus
    #   the current method needs to terminate immediately, or whether the statement simply ran but didn't execute a
    #   return statement, and thus the next statement in the method should run normally
    # - return value is a value of type Value which is the returned value from the function
    def __execute_statement(self, env, return_type, code):
        if self.trace_output:
            print(f"{code[0].line_num}: {code}")
        tok = code[0]
        if tok == InterpreterBase.BEGIN_DEF:
            return self.__execute_begin(env, return_type, code)
        elif tok == InterpreterBase.SET_DEF:
            return self.__execute_set(env, code)
        elif tok == InterpreterBase.IF_DEF:
            return self.__execute_if(env, return_type, code)
        elif tok == InterpreterBase.CALL_DEF:
            return self.__execute_call(env, code)
        elif tok == InterpreterBase.WHILE_DEF:
            return self.__execute_while(env, return_type, code)
        elif tok == InterpreterBase.RETURN_DEF:
            return self.__execute_return(env, return_type, code)
        elif tok == InterpreterBase.INPUT_STRING_DEF:
            return self.__execute_input(env, code, True)
        elif tok == InterpreterBase.INPUT_INT_DEF:
            return self.__execute_input(env, code, False)
        elif tok == InterpreterBase.PRINT_DEF:
            return self.__execute_print(env, code)
        elif tok == InterpreterBase.LET_DEF:
            return self.__execute_let(env, return_type, code)
        else:
            # Report error via interpreter
            self.interpreter.error(
                ErrorType.SYNTAX_ERROR, "unknown statement " + tok, tok.line_num
            )

    # This method is used for both the begin and let statements
    # (begin (statement1) (statement2) ... (statementn))
    # (let ((type1 var1 defaultvalue1) ... (typen varn defaultvaluen)) (statement1) ... (statementn))
    def __execute_begin(self, env, return_type, code, has_vardef=False):
        if has_vardef:  # handles the let case
            code_start = 2
            env.block_nest()
            self.__add_locals_to_env(env, code[1], code[0].line_num)
        else:  # handles the begin case
            code_start = 1

        status = ObjectDef.STATUS_PROCEED
        return_value = None
        for statement in code[code_start:]:
            status, return_value = self.__execute_statement(env, return_type, statement)
            if status == ObjectDef.STATUS_RETURN:
                break
        # if we run through the entire block without a return, then just return proceed
        # we don't want the enclosing block to exit with a return
        if has_vardef:
            env.block_unnest()
        return status, return_value  # could be a valid return of a value or an error

    # add all local variables defined in a let to the environment
    def __add_locals_to_env(self, env, var_defs, line_number):
        for var_def in var_defs:
            # vardef in the form of (typename varname defvalue)
            var_type = Type(var_def[0])
            var_name = var_def[1]
            if InterpreterBase.TYPE_CONCAT_CHAR in var_def[0]:
                self.interpreter.type_manager.create_template_class_type(var_def[0])
            default_value = create_value(var_def[2]) if len(var_def) >= 3 else create_default_value(var_type)
            # make sure default value for each local is of a matching type
            self.__check_type_compatibility(
                var_type, default_value.type(), True, line_number
            )
            if not env.create_new_symbol(var_name):
                self.interpreter.error(
                    ErrorType.NAME_ERROR,
                    "duplicate local variable name " + var_name,
                    line_number,
                )
            var_def = VariableDef(var_type, var_name, default_value)
            env.set(var_name, var_def)

    # (let ((type1 var1 defval1) (type2 var2 defval2)) (statement1) (statement2) ...)
    # uses helper function __execute_begin to implement its functionality
    def __execute_let(self, env, return_type, code):
        return self.__execute_begin(env, return_type, code, True)

    # (call object_ref/me methodname param1 param2 param3)
    # where params are expressions, and expresion could be a value, or a (+ ...)
    # statement version of a method call; there's also an expression version of a method call below
    def __execute_call(self, env, code):
        return ObjectDef.STATUS_PROCEED, self.__execute_call_aux(
            env, code, code[0].line_num
        )

    # (set varname expression), where expression could be a value, or a (+ ...)
    def __execute_set(self, env, code):
        val = self.__evaluate_expression(env, code[2], code[0].line_num)
        self.__set_variable_aux(
            env, code[1], val, code[0].line_num
        )  # checks/reports type and name errors
        return ObjectDef.STATUS_PROCEED, None

    # (return expression) where expresion could be a value, or a (+ ...)
    def __execute_return(self, env, return_type, code):
        if len(code) == 1:
            # [return] with no return value; return default value for type
            return ObjectDef.STATUS_RETURN, None
        else:
            result = self.__evaluate_expression(env, code[1], code[0].line_num)
            # CAREY FIX
            if result.is_typeless_null():
                self.__check_type_compatibility(return_type, result.type(), True, code[0].line_num)
                result = Value(return_type, None)  # propagate return type to null ###
        self.__check_type_compatibility(
            return_type, result.type(), True, code[0].line_num
        )
        return ObjectDef.STATUS_RETURN, result

    # (print expression1 expression2 ...) where expresion could be a variable, value, or a (+ ...)
    def __execute_print(self, env, code):
        output = ""
        for expr in code[1:]:
            # TESTING NOTE: Will not test printing of object references
            term = self.__evaluate_expression(env, expr, code[0].line_num)
            val = term.value()
            typ = term.type()
            if typ == ObjectDef.BOOL_TYPE_CONST:
                if val == True:
                    val = "true"
                else:
                    val = "false"
            # document will never print out an obj ref
            output += str(val)
        self.interpreter.output(output)
        return ObjectDef.STATUS_PROCEED, None

    # (inputs target_variable) or (inputi target_variable) sets target_variable to input string/int
    def __execute_input(self, env, code, get_string):
        inp = self.interpreter.get_input()
        if get_string:
            val = Value(ObjectDef.STRING_TYPE_CONST, inp)
        else:
            val = Value(ObjectDef.INT_TYPE_CONST, int(inp))

        self.__set_variable_aux(env, code[1], val, code[0].line_num)
        return ObjectDef.STATUS_PROCEED, None

    # helper method used to set either parameter variables or member fields; parameters currently shadow
    # member fields
    def __set_variable_aux(self, env, var_name, value, line_num):
        # parameters shadows fields, locals shadow parameters (and outer-block locals)
        if self.__set_local_or_param(
            env, var_name, value, line_num
        ):  # may report a type error
            return
        if self.__set_field(var_name, value, line_num):  # may report a type error
            return
        self.interpreter.error(
            ErrorType.NAME_ERROR, "unknown field/variable " + var_name, line_num
        )

    # (if expression (statement) (statement) ) where expresion could be a boolean constant (e.g., true), member
    # variable without ()s, or a boolean expression in parens, like (> 5 a)
    def __execute_if(self, env, return_type, code):
        condition = self.__evaluate_expression(env, code[1], code[0].line_num)
        if condition.type() != ObjectDef.BOOL_TYPE_CONST:
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                "non-boolean if condition " + ' '.join(x for x in code[1]),
                code[0].line_num,
            )
        if condition.value():
            status, return_value = self.__execute_statement(
                env, return_type, code[2]
            )  # if condition was true
            return status, return_value
        elif len(code) == 4:
            status, return_value = self.__execute_statement(
                env, return_type, code[3]
            )  # if condition was false, do else
            return status, return_value
        else:
            return ObjectDef.STATUS_PROCEED, None

    # (while expression (statement) ) where expresion could be a boolean value, boolean member variable,
    # or a boolean expression in parens, like (> 5 a)
    def __execute_while(self, env, return_type, code):
        while True:
            condition = self.__evaluate_expression(env, code[1], code[0].line_num)
            if condition.type() != ObjectDef.BOOL_TYPE_CONST:
                self.interpreter.error(
                    ErrorType.TYPE_ERROR,
                    "non-boolean while condition " + ' '.join(x for x in code[1]),
                    code[0].line_num,
                )
            if not condition.value():  # condition is false, exit loop immediately
                return ObjectDef.STATUS_PROCEED, None
            # condition is true, run body of while loop
            status, return_value = self.__execute_statement(env, return_type, code[2])
            if status == ObjectDef.STATUS_RETURN:
                return (
                    status,
                    return_value,
                )  # could be a valid return of a value or an error

    # var_def is a VariableDef
    # this method checks to see if a variable holds a null value, and if so, changes the type of the null value
    # to the type of the variable, e.g.,
    def __propagate_type_to_null(self, var_def):
        if var_def.value.is_null():
            return Value(var_def.type, None)
        return var_def.value

    # given an expression, return a Value object with the expression's evaluated result
    # expressions could be: constants (true, 5, "blah"), variables (e.g., x), arithmetic/string/logical expressions
    # like (+ 5 6), (+ "abc" "def"), (> a 5), method calls (e.g., (call me foo)), or instantiations (e.g., new dog_class)
    def __evaluate_expression(self, env, expr, line_num_of_statement):
        if type(expr) is not list:
            # locals shadow member variables
            var_def = env.get(expr)
            if var_def is not None:
                return self.__propagate_type_to_null(var_def)
            elif expr in self.fields:
                return self.__propagate_type_to_null(
                    self.fields[expr]
                )  # return the Value object
            # need to check for variable name and get its value too
            value = create_value(expr)
            if value is not None:
                return value
            if expr == InterpreterBase.ME_DEF:
                return (
                    self.get_me_as_value()
                )  # create Value object for current object with right type
            self.interpreter.error(
                ErrorType.NAME_ERROR,
                "invalid field or parameter " + expr,
                line_num_of_statement,
            )

        operator = expr[0]
        if operator in self.binary_op_list:
            operand1 = self.__evaluate_expression(env, expr[1], line_num_of_statement)
            operand2 = self.__evaluate_expression(env, expr[2], line_num_of_statement)
            if (
                operand1.type() == operand2.type()
                and operand1.type() == ObjectDef.INT_TYPE_CONST
            ):
                if operator not in self.binary_ops[InterpreterBase.INT_DEF]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid operator applied to ints",
                        line_num_of_statement,
                    )
                return self.binary_ops[InterpreterBase.INT_DEF][operator](
                    operand1, operand2
                )
            if (
                operand1.type() == operand2.type()
                and operand1.type() == ObjectDef.STRING_TYPE_CONST
            ):
                if operator not in self.binary_ops[InterpreterBase.STRING_DEF]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid operator applied to strings",
                        line_num_of_statement,
                    )
                return self.binary_ops[InterpreterBase.STRING_DEF][operator](
                    operand1, operand2
                )
            if (
                operand1.type() == operand2.type()
                and operand1.type() == ObjectDef.BOOL_TYPE_CONST
            ):
                if operator not in self.binary_ops[InterpreterBase.BOOL_DEF]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid operator applied to bool",
                        line_num_of_statement,
                    )
                return self.binary_ops[InterpreterBase.BOOL_DEF][operator](
                    operand1, operand2
                )
            # handle object reference comparisons last
            if self.interpreter.check_type_compatibility(
                operand1.type(), operand2.type(), False
            ):
                return self.binary_ops[InterpreterBase.CLASS_DEF][operator](
                    operand1, operand2
                )
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                f"operator {operator} applied to two incompatible types",
                line_num_of_statement,
            )
        if operator in self.unary_op_list:
            operand = self.__evaluate_expression(env, expr[1], line_num_of_statement)
            if operand.type() == ObjectDef.BOOL_TYPE_CONST:
                if operator not in self.unary_ops[InterpreterBase.BOOL_DEF]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid unary operator applied to bool",
                        line_num_of_statement,
                    )
                return self.unary_ops[InterpreterBase.BOOL_DEF][operator](operand)

        # handle call expression: (call objref methodname p1 p2 p3)
        if operator == InterpreterBase.CALL_DEF:
            return self.__execute_call_aux(env, expr, line_num_of_statement)
        # handle new expression: (new classname)
        if operator == InterpreterBase.NEW_DEF:
            return self.__execute_new_aux(env, expr, line_num_of_statement)

    # (new classname)
    def __execute_new_aux(self, env, code, line_num_of_statement):
        class_name = code[1]
        obj = self.interpreter.instantiate(code[1], line_num_of_statement)
        return Value(Type(class_name), obj)

    # this method is a helper used by call statements and call expressions
    # (call object_ref/me methodname p1 p2 p3)
    def __execute_call_aux(self, env, code, line_num_of_statement):
        # determine which object we want to call the method on
        super_only = False
        obj_name = code[1]
        if obj_name == InterpreterBase.ME_DEF:
            obj = self
        elif obj_name == InterpreterBase.SUPER_DEF:
            if not self.super_object:
                self.interpreter.error(
                    ErrorType.TYPE_ERROR,
                    "invalid call to super object by class "
                    + self.class_def.get_name(),
                    line_num_of_statement,
                )
            obj = self.super_object
            super_only = True
        else:
            # return a Value() object which has a type and a value
            obj_val = self.__evaluate_expression(env, obj_name, line_num_of_statement)
            if obj_val.is_null():
                self.interpreter.error(
                    ErrorType.FAULT_ERROR, "null dereference", line_num_of_statement
                )
            obj = obj_val.value()
        # prepare the actual arguments for passing
        actual_args = []
        for expr in code[3:]:
            actual_args.append(
                self.__evaluate_expression(env, expr, line_num_of_statement)
            )
        return obj.call_method(code[2], actual_args, super_only, line_num_of_statement)

    def __map_method_names_to_method_definitions(self):
        self.methods = {}
        for method in self.class_def.get_methods():
            self.methods[method.method_name] = method

    def __instantiate_fields(self):
        self.fields = {}
        # get_fields() returns a set of VariableDefs
        for vardef in self.class_def.get_fields():
            self.fields[vardef.name] = copy.copy(vardef)

    def __set_field(self, field_name, value, line_num):
        if field_name not in self.fields:
            return False
        var_def = self.fields[field_name]
        self.__check_type_compatibility(var_def.type, value.type(), True, line_num)
        var_def.set_value(value)
        return True

    def __set_local_or_param(self, env, var_name, value, line_num):
        var_def = env.get(var_name)
        if var_def is None:
            return False
        self.__check_type_compatibility(var_def.type, value.type(), True, line_num)
        var_def.set_value(value)
        return True

    def __check_type_compatibility(
        self, lvalue_type, rvalue_type, for_assignment, line_num
    ):
        if not self.interpreter.check_type_compatibility(
            lvalue_type, rvalue_type, for_assignment
        ):
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                f"type mismatch {lvalue_type.type_name} and {rvalue_type.type_name}",
                line_num,
            )

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
        self.binary_ops = {}
        self.binary_ops[InterpreterBase.INT_DEF] = {
            "+": lambda a, b: Value(ObjectDef.INT_TYPE_CONST, a.value() + b.value()),
            "-": lambda a, b: Value(ObjectDef.INT_TYPE_CONST, a.value() - b.value()),
            "*": lambda a, b: Value(ObjectDef.INT_TYPE_CONST, a.value() * b.value()),
            "/": lambda a, b: Value(
                ObjectDef.INT_TYPE_CONST, a.value() // b.value()
            ),  # // for integer ops
            "%": lambda a, b: Value(ObjectDef.INT_TYPE_CONST, a.value() % b.value()),
            "==": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() == b.value()),
            "!=": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() != b.value()),
            ">": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() > b.value()),
            "<": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() < b.value()),
            ">=": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() >= b.value()),
            "<=": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() <= b.value()),
        }
        self.binary_ops[InterpreterBase.STRING_DEF] = {
            "+": lambda a, b: Value(ObjectDef.STRING_TYPE_CONST, a.value() + b.value()),
            "==": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() == b.value()),
            "!=": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() != b.value()),
            ">": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() > b.value()),
            "<": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() < b.value()),
            ">=": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() >= b.value()),
            "<=": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() <= b.value()),
        }
        self.binary_ops[InterpreterBase.BOOL_DEF] = {
            "&": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() and b.value()),
            "|": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() or b.value()),
            "==": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() == b.value()),
            "!=": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() != b.value()),
        }
        self.binary_ops[InterpreterBase.CLASS_DEF] = {
            "==": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() == b.value()),
            "!=": lambda a, b: Value(ObjectDef.BOOL_TYPE_CONST, a.value() != b.value()),
        }

        self.unary_ops = {}
        self.unary_ops[InterpreterBase.BOOL_DEF] = {
            "!": lambda a: Value(ObjectDef.BOOL_TYPE_CONST, not a.value()),
        }

    def __init_superclass_if_any(self):
        superclass_def = self.class_def.get_superclass()
        if superclass_def is None:
            self.super_object = None
            return

        self.super_object = ObjectDef(
            self.interpreter, superclass_def, self.anchor_object, self.trace_output
        )


# need to document that each class has at least one method guaranteed

# Main interpreter class


class Interpreter(InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self.trace_output = trace_output

    # run a program, provided in an array of strings, one string per line of source code
    # usese the provided BParser class found in parser.py to parse the program into lists
    def run(self, program):
        status, parsed_program = BParser.parse(program)
        if not status:
            super().error(
                ErrorType.SYNTAX_ERROR, f"Parse error on program: {parsed_program}"
            )
        self.__add_all_class_types_to_type_manager(parsed_program)
        self.__map_class_names_to_class_defs(parsed_program)

        # instantiate main class
        invalid_line_num_of_caller = None
        self.main_object = self.instantiate(
            InterpreterBase.MAIN_CLASS_DEF, invalid_line_num_of_caller
        )

        # call main function in main class; return value is ignored from main
        self.main_object.call_method(
            InterpreterBase.MAIN_FUNC_DEF, [], False, invalid_line_num_of_caller
        )

        # program terminates!

    # user passes in the line number of the statement that performed the new command so we can generate an error
    # if the user tries to new an class name that does not exist. This will report the line number of the statement
    # with the new command
    def instantiate(self, class_name, line_num_of_statement):
        if InterpreterBase.TYPE_CONCAT_CHAR in class_name:
            # template
            self.type_manager.create_template_class_type(class_name)

        if class_name not in self.class_index:
            super().error(
                ErrorType.TYPE_ERROR,
                f"No class named {class_name} found",
                line_num_of_statement,
            )
        class_def = self.class_index[class_name]
        obj = ObjectDef(
            self, class_def, None, self.trace_output
        )  # Create an object based on this class definition
        return obj

    # returns a ClassDef object
    def get_class_def(self, class_name, line_number_of_statement):
        if class_name not in self.class_index:
            super().error(
                ErrorType.TYPE_ERROR,
                f"No class named {class_name} found",
                line_number_of_statement,
            )
        return self.class_index[class_name]

    # returns a bool
    def is_valid_type(self, typename):
        return self.type_manager.is_valid_type(typename)

    # returns a bool
    def is_a_subtype(self, suspected_supertype, suspected_subtype):
        return self.type_manager.is_a_subtype(suspected_supertype, suspected_subtype)

    # typea and typeb are Type objects; returns true if the two type are compatible
    # for assignments typea is the type of the left-hand-side variable, and typeb is the type of the
    # right-hand-side variable, e.g., (set person_obj_ref (new teacher))
    def check_type_compatibility(self, typea, typeb, for_assignment=False):
        return self.type_manager.check_type_compatibility(typea, typeb, for_assignment)

    def __map_class_names_to_class_defs(self, program):
        self.class_index = {}
        for item in program:
            if item[0] == InterpreterBase.CLASS_DEF:
                if item[1] in self.class_index:
                    super().error(
                        ErrorType.TYPE_ERROR,
                        f"Duplicate class name {item[1]}",
                        item[0].line_num,
                    )
                self.class_index[item[1]] = ClassDef(item, self)
            # maybe todo: test for dupe tclasses

    # [class classname inherits superclassname [items]]
    def __add_all_class_types_to_type_manager(self, parsed_program):
        self.type_manager = TypeManager(self)
        for item in parsed_program:
            if item[0] == InterpreterBase.CLASS_DEF:
                class_name = item[1]
                superclass_name = None
                if item[2] == InterpreterBase.INHERITS_DEF:
                    superclass_name = item[3]
                self.type_manager.add_class_type(class_name, superclass_name)
            elif item[0] == InterpreterBase.TEMPLATE_CLASS_DEF:
                self.type_manager.add_template_class_type(item)
