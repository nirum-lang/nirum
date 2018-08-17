{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.Python.Deserializers
    ( compileDeserializer
    ) where

import Text.Blaze (Markup)
import Text.Heterocephalus (compileText)
import Text.InterpolatedString.Perl6 (qq)

import Nirum.Constructs.Identifier
import Nirum.Constructs.ModulePath
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.TypeExpression
import Nirum.Package.Metadata
import Nirum.Targets.Python.CodeGen
import Nirum.Targets.Python.Validators
import Nirum.TypeInstance.BoundModule

-- | It takes these arguments:
--
--     1. a 'BoundModule Python' for the context,
--     2. a 'TypeExpression' which determines how it interprets an input,
--     3. a Python expression of an input,
--     4. a Python expression to store an output, and
--     5. a Python expression of a function, takes two arguments, to store a
--        possible error.
--
-- It returns a Python code that deserializes an input.
--
-- A Python expressions of the third argument should be a target (lvalue)
-- expression, and the last argument should be evaluated as a Python callable.
--
-- A generated Python can be more than an expression; it can
-- consists of multiple statements.  Note that its indentation level is zero.
--
-- For example, if we have a deserializer named @d@ the following code:
--
-- @
--     d "inputs['x']" "outputs['x']" "error"
-- @
--
-- could return a string containing a Python code like the following:
--
-- @
--     if isinstance(inputs['x'], str):
--         try:
--             outputs['x'] = int(inputs['x'])
--         except ValueError:
--             errors('', 'Expected a numeric string.')
--     else:
--         error('', 'Expected a string.')
-- @
compileDeserializer
    :: BoundModule Python -- | The compilation context.
    -> TypeExpression -- | Determines how it interprets an input.
    -> Code -- | A Python expression of an input.
    -> Code -- | A Python expression to store an output.
    -> Code -- | A Python expression of a function to store a possible error.
    -> CodeGen Markup -- | A Python code that deserializes an input.
compileDeserializer mod' typeExpr vInput vOutput vError = do
    let intermOutput = mangleVar vOutput "interm"
    let intermError = mangleVar vError "interm"
    deserializer <- compileDeserializer'
        mod' typeExpr vInput [qq|$intermOutput['rv']|] intermError
    Validator pred' valueValidators' <-
        compileValidator mod' typeExpr [qq|$intermOutput['rv']|]
    return [compileText|
#{intermOutput} = {}
def #{intermError}(err_field, err_msg):
    #{intermError}.errored = True
    #{vError}(err_field, err_msg)
#{deserializer}
if not getattr(#{intermError}, 'errored', False) and #{intermOutput}:
    if not (#{pred'}):
        (#{vError})(
            '',
            'A deserialized value is unexpected type: {0!r}.'.format(
                #{intermOutput}['rv']
            )
        )
%{ forall ValueValidator predCode errorMsg <- valueValidators' }
    elif not (#{predCode}):
        (#{vError})('', #{stringLiteral errorMsg})
%{ endforall }
    (#{vOutput}) = #{intermOutput}['rv']
    |]

compileDeserializer'
    :: BoundModule Python -- | The compilation context.
    -> TypeExpression -- | Determines how it interprets an input.
    -> Code -- | A Python expression of an input.
    -> Code -- | A Python expression to store an output.
    -> Code -- | A Python expression of a function to store a possible error.
    -> CodeGen Markup -- | A Python code that deserializes an input.

compileDeserializer' mod' (OptionModifier typeExpr) vInput vOutput vError = do
    deserializer <- compileDeserializer mod' typeExpr vInput vOutput vError
    return [compileText|
if (#{vInput}) is None:
    (#{vOutput}) = None
else:
#{indent "    " deserializer}
    |]

compileDeserializer' mod' (SetModifier typeExpr) vInput vOutput vError = do
    let elInput = mangleVar vInput "element"
        elIndex = mangleVar vInput "index"
        elOutput = mangleVar vOutput "element"
        elError = mangleVar vError "element"
        elErrorFlag = mangleVar vError "errored"
    builtins <- importBuiltins
    baseString <- baseStringClass
    mAbc <- collectionsAbc
    deserializer <- compileDeserializer mod' typeExpr elInput elOutput elError
    return [compileText|
if (#{builtins}.isinstance(#{vInput}, #{mAbc}.Sequence) and
    not #{builtins}.isinstance(#{vInput}, #{baseString})):
    (#{vOutput}) = #{builtins}.set()
    #{elErrorFlag} = [False]
    def #{elError}(err_field, err_msg):
        #{elErrorFlag}[0] = True
        #{vError}(
            '[{0}]{1}'.format(#{elIndex}, err_field), err_msg
        )
    for #{elIndex}, #{elInput} in #{builtins}.enumerate(#{vInput}):
        #{elErrorFlag}[0] = False
#{indent "        " deserializer}
        if not #{elErrorFlag}[0]:
            (#{vOutput}).add(#{elOutput})
    (#{vOutput}) = #{builtins}.frozenset(#{vOutput})
else:
    (#{vError})('', 'Expected an array.')
    |]

compileDeserializer' mod' (ListModifier typeExpr) vInput vOutput vError = do
    let elInput = mangleVar vInput "elem"
        elIndex = mangleVar vInput "idx"
        elOutput = mangleVar vOutput "elem"
        elError = mangleVar vError "elem"
        elErrorFlag = mangleVar vError "flag"
    builtins <- importBuiltins
    mAbc <- collectionsAbc
    baseString <- baseStringClass
    insertThirdPartyImportsA [("nirum.datastructures", [("list_type", "List")])]
    deserializer <- compileDeserializer mod' typeExpr elInput elOutput elError
    return [compileText|
if (#{builtins}.isinstance(#{vInput}, #{mAbc}.Sequence) and
    not #{builtins}.isinstance(#{vInput}, #{baseString})):
    (#{vOutput}) = []
    #{elErrorFlag} = [False]
    def #{elError}(err_field, err_msg):
        #{elErrorFlag}[0] = True
        #{vError}(
            '[{0}]{1}'.format(#{elIndex}, err_field), err_msg
        )
    for #{elIndex}, #{elInput} in #{builtins}.enumerate(#{vInput}):
        #{elErrorFlag}[0] = False
#{indent "        " deserializer}
        if not #{elErrorFlag}[0]:
            (#{vOutput}).append(#{elOutput})
    (#{vOutput}) = list_type(#{vOutput})
else:
    (#{vError})(('', 'Expected an array.'))
    |]

compileDeserializer' mod' (MapModifier keyTypeExpr valueTypeExpr)
                     vInput vOutput vError = do
    let pairInput = mangleVar vInput "pair"
        pairIndex = mangleVar vInput "index"
        pairErrored = mangleVar vInput "errored"
        keyInput = mangleVar vInput "key"
        keyOutput = mangleVar vOutput "key"
        keyError = mangleVar vError "key"
        valueInput = mangleVar vInput "value"
        valueOutput = mangleVar vOutput "value"
        valueError = mangleVar vError "value"
    mAbc <- collectionsAbc
    builtins <- importBuiltins
    baseString <- baseStringClass
    insertThirdPartyImportsA [("nirum.datastructures", [("map_type", "Map")])]
    keyDeserializer <- compileDeserializer
        mod' keyTypeExpr keyInput keyOutput keyError
    valueDeserializer <- compileDeserializer
        mod' valueTypeExpr valueInput valueOutput valueError
    return [compileText|
if (#{builtins}.isinstance(#{vInput}, #{mAbc}.Sequence) and
    not #{builtins}.isinstance(#{vInput}, #{baseString})):
    (#{vOutput}) = []
    (#{keyError}) = lambda err_field, err_msg: #{vError}(
        '[{0}].key{1}'.format(#{pairIndex}, err_field),
        err_msg
    )
    (#{valueError}) = lambda err_field, err_msg: #{vError}(
        '[{0}].value{1}'.format(#{pairIndex}, err_field),
        err_msg
    )
    for #{pairIndex}, #{pairInput} in #{builtins}.enumerate(#{vInput}):
        (#{pairErrored}) = False
        if #{builtins}.isinstance(#{pairInput}, #{mAbc}.Mapping):
            try:
                (#{keyInput}) = #{pairInput}['key']
            except #{builtins}.KeyError:
                (#{vError})(
                    '[{0}].key'.format(#{pairIndex}), 'Expected to exist.'
                )
                (#{pairErrored}) = True
            else:
#{indent "                " keyDeserializer}
            try:
                (#{valueInput}) = #{pairInput}['value']
            except #{builtins}.KeyError:
                (#{vError})(
                    '[{0}].value'.format(#{pairIndex}), 'Expected to exist.'
                )
                (#{pairErrored}) = True
            else:
#{indent "                " valueDeserializer}
            if not #{pairErrored}:
                (#{vOutput}).append((#{keyOutput}, #{valueOutput}))
        else:
            (#{vError})(
                '[{0}]'.format(#{pairIndex}),
                'Expected an object which has the fields "key" and "value".'
            )
    (#{vOutput}) = map_type(#{vOutput})
else:
    (#{vError})('', 'Expected an array of objects.')
    |]

compileDeserializer' mod' (TypeIdentifier typeId) vInput vOutput vError = do
    builtins <- importBuiltins
    case lookupType typeId mod' of
        Missing -> return [compileText|raise #{builtins}.RuntimeError(
            "This must never happen; it is likely to be a Nirum compiler's bug."
        )|]
        Local (Alias t) -> compileDeserializer mod' t vInput vOutput vError
        Imported modulePath' _ (Alias t) ->
            case resolveBoundModule modulePath' (boundPackage mod') of
                Nothing -> return [compileText|raise #{builtins}.RuntimeError(
                    "This must never happen; "
                    "it is likely to be a Nirum compiler's bug."
                )|]
                Just foundMod ->
                    compileDeserializer foundMod t vInput vOutput vError
        Local PrimitiveType { primitiveTypeIdentifier = p } ->
            compilePrimitiveTypeDeserializer p vInput vOutput vError
        Imported _ _ PrimitiveType { primitiveTypeIdentifier = p } ->
            compilePrimitiveTypeDeserializer p vInput vOutput vError
        Local EnumType {} -> deserializerCall Nothing typeId
        Imported m srcTypeId EnumType {} -> deserializerCall (Just m) srcTypeId
        Local RecordType {} -> deserializerCall Nothing typeId
        Imported m srcTypId RecordType {} -> deserializerCall (Just m) srcTypId
        Local UnboxedType {} -> deserializerCall Nothing typeId
        Imported m srcTypId UnboxedType {} -> deserializerCall (Just m) srcTypId
        Local UnionType {} -> deserializerCall Nothing typeId
        Imported m srcTypeId UnionType {} -> deserializerCall (Just m) srcTypeId
  where
    target' :: Python
    target' = packageTarget $ boundPackage mod'
    deserializerCall :: Maybe ModulePath -> Identifier -> CodeGen Markup
    deserializerCall m sourceTypeId = do
        case m of
            Just mp -> insertThirdPartyImportsA
                [ ( toImportPath target' mp
                  , [(toClassName typeId, toClassName sourceTypeId)]
                  )
                ]
            Nothing -> return ()
        return [compileText|
#{vOutput} = #{toClassName typeId}.__nirum_deserialize__(
    #{vInput},
    on_error=(#{vError})
)
        |]

compilePrimitiveTypeDeserializer :: PrimitiveTypeIdentifier
                                 -> Code
                                 -> Code
                                 -> Code
                                 -> CodeGen Markup

compilePrimitiveTypeDeserializer Bigint vInput vOutput vError = do
    builtins <- importBuiltins
    baseInteger <- baseIntegerClass
    baseString <- baseStringClass
    return [compileText|
if #{builtins}.isinstance(#{vInput}, #{baseString}):
    try:
        #{vOutput} = #{builtins}.int(#{vInput})
    except #{builtins}.ValueError:
        #{vError}(
            '',
            'Expected a string of decimal digits, '
            'but the string consists of other than decimal digits.'
        )
elif #{builtins}.isinstance(#{vInput}, #{baseInteger}):
    #{vOutput} = #{vInput}
else:
    #{vError}(
        '',
        'Expected a string of decimal digits, '
        'but the given value is not a string.'
    )
    |]

compilePrimitiveTypeDeserializer Decimal vInput vOutput vError = do
    builtins <- importBuiltins
    decimal <- importStandardLibrary "decimal"
    baseString <- baseStringClass
    return [compileText|
if #{builtins}.isinstance(#{vInput}, #{baseString}):
    try:
        #{vOutput} = #{decimal}.Decimal(#{vInput})
    except #{builtins}.ValueError:
        #{vError}(
            '', 'Expected a numeric string, but the string is not numeric.'
        )
else:
    #{vError}(
        '', 'Expected a numeric string, but the given value is not a string.'
    )
    |]

compilePrimitiveTypeDeserializer Int32 vInput vOutput vError = do
    baseInteger <- baseIntegerClass
    builtins <- importBuiltins
    baseString <- baseStringClass
    return [compileText|
if #{builtins}.isinstance(#{vInput}, #{baseInteger}):
    #{vOutput} = #{vInput}
elif #{builtins}.isinstance(#{vInput}, #{builtins}.float):
    #{vOutput} = #{builtins}.int(#{vInput})
    if #{vOutput} != (#{vInput}):
        #{vError}(
            '',
            'Expected an integral number, the given number is not integral.'
        )
elif #{builtins}.isinstance(#{vInput}, #{baseString}):
    try:
        #{vOutput} = #{builtins}.int(#{vInput})
    except #{builtins}.ValueError:
        #{vError}(
            '', 'Expected an integral number or a string of decimal digits.'
        )
else:
    #{vError}(
        '',
        'Expected an integral number, but the given value is not a number.'
    )
    |]

compilePrimitiveTypeDeserializer Int64 vInput vOutput vError =
    compilePrimitiveTypeDeserializer Int32 vInput vOutput vError

compilePrimitiveTypeDeserializer Float32 vInput vOutput vError = do
    builtins <- importBuiltins
    baseString <- baseStringClass
    baseInteger <- baseIntegerClass
    return [compileText|
if #{builtins}.isinstance(#{vInput}, #{builtins}.float):
    #{vOutput} = #{vInput}
elif (#{builtins}.isinstance(#{vInput}, #{baseString}) or
      #{builtins}.isinstance(#{vInput}, #{baseInteger})):
    try:
        #{vOutput} = #{builtins}.int(#{vInput})
    except #{builtins}.ValueError:
        #{vError}(
            '',
            'Expected a floating-point number or its string representation.'
        )
else:
    #{vError}(
        '',
        'Expected a floating-point number, '
        'but the given value is not a number.'
    )
    |]

compilePrimitiveTypeDeserializer Float64 vInput vOutput vError =
    compilePrimitiveTypeDeserializer Float32 vInput vOutput vError

compilePrimitiveTypeDeserializer Text vInput vOutput vError = do
    builtins <- importBuiltins
    pyVer <- getPythonVersion
    case pyVer of
        Python3 -> return [compileText|
if #{builtins}.isinstance(#{vInput}, #{builtins}.str):
    #{vOutput} = #{vInput}
else:
    #{vError}('', 'Expected a string.')
        |]
        Python2 -> return [compileText|
if #{builtins}.isinstance(#{vInput}, #{builtins}.unicode):
    #{vOutput} = #{vInput}
elif #{builtins}.isinstance(#{vInput}, str):
    try:
        #{vOutput} = (#{vInput}).decode('utf-8')
    except #{builtins}.UnicodeDecodeError:
        try:
            #{vOutput} = unicode(#{vInput})
        except #{builtins}.UnicodeDecodeError:
            #{vError}('', 'Expected a Unicode string.')
else:
    #{vError}('', 'Expected a string.')
        |]

compilePrimitiveTypeDeserializer Binary vInput vOutput vError = do
    builtins <- importBuiltins
    base64 <- importStandardLibrary "base64"
    baseString <- baseStringClass
    return [compileText|
if #{builtins}.isinstance(#{vInput}, #{baseString}):
    try:
        #{vOutput} = #{base64}.b64decode(#{vInput})
    except #{builtins}.ValueError:
        #{vError}(
            '',
            'Expected a base64-encoded string, '
            'but the string is not properly base64-encoded.'
        )
else:
    #{vError}(
        '',
        'Expected a base64-encoded string, but the given value is not a string'
    )
    |]

compilePrimitiveTypeDeserializer Date vInput vOutput vError = do
    builtins <- importBuiltins
    datetime <- importStandardLibrary "datetime"
    re <- importStandardLibrary "re"
    let vMatch = mangleVar vInput "match"
    baseString <- baseStringClass
    wrapScope vInput vOutput vError $ \ in' _ err -> return [compileText|
if not #{builtins}.isinstance(#{in'}, #{baseString}):
    #{err}(
        '',
        'Expected a string of RFC 3339 date, '
        'but the given value is not a string.'
    )
#{vMatch} = #{re}.match(r'^(\d{4})-(\d\d)-(\d\d)$', #{in'})
if not #{vMatch}:
    #{err}(
        '',
        'Expected a string of RFC 3339 date, '
        'but the string is not a valid RFC 3339 date format.'
    )
try:
    return #{datetime}.date(
        #{builtins}.int(#{vMatch}.group(1)),
        #{builtins}.int(#{vMatch}.group(2)),
        #{builtins}.int(#{vMatch}.group(3)),
    )
except #{builtins}.ValueError:
    #{err}(
        '',
        'Expected a string of RFC 3339 date, but the date is invalid.'
    )
    |]

compilePrimitiveTypeDeserializer Datetime vInput vOutput vError = do
    builtins <- importBuiltins
    datetime <- importStandardLibrary "datetime"
    pyVer <- getPythonVersion
    case pyVer of
        Python3 -> do
            re <- importStandardLibrary "re"
            let vMatch = mangleVar vInput "match"
                vTz = mangleVar vInput "tz"
            wrapScope vInput vOutput vError $ \ in' _ err ->
                return [compileText|
if not #{builtins}.isinstance(#{in'}, #{builtins}.str):
    #{err}(
        '',
        'Expected a string of RFC 3339 date & time, '
        'but the given value is not a string.'
    )
#{vMatch} = #{re}.match(
    r'^(\d{4})-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)'
    r'(?:\.(\d*))?'
    r'(Z|([+-])(\d\d):?(\d\d))$',
    #{in'}
)
if not #{vMatch}:
    #{err}(
        '',
        'Expected a string of RFC 3339 date & time, '
        'but the string is not a valid RFC 3339 date & time format.'
    )
try:
    if #{vMatch}.group(8) == 'Z':
        #{vTz} = #{datetime}.timezone.utc
    else:
        #{vTz} = #{datetime}.timezone(
            #{datetime}.timedelta(
                hours=#{builtins}.int(#{vMatch}.group(10)),
                minutes=#{builtins}.int(#{vMatch}.group(11))
            )
        )
        if #{vMatch}.group(9) == '-':
            #{vTz} = -#{vTz}
    return #{datetime}.datetime(
        #{builtins}.int(#{vMatch}.group(1)),
        #{builtins}.int(#{vMatch}.group(2)),
        #{builtins}.int(#{vMatch}.group(3)),
        #{builtins}.int(#{vMatch}.group(4)),
        #{builtins}.int(#{vMatch}.group(5)),
        #{builtins}.int(#{vMatch}.group(6)),
        #{builtins}.int((#{vMatch}.group(7) or '0')[:6].zfill(6)),
        tzinfo=#{vTz}
    )
except #{builtins}.ValueError:
    #{err}(
        '',
        'Expected a string of RFC 3339 date & time, '
        'but the date & time is invalid.'
    )
                |]
        Python2 -> do
            addOptionalDependency (3, 0) "iso8601"
            insertThirdPartyImportsA
                [("iso8601", [("_parse_iso8601", "parse_date")])]
            insertThirdPartyImportsA
                [("iso8601.iso8601", [("_iso8601_parse_error", "ParseError")])]
            let vException = mangleVar vError "exc"
            wrapScope vInput vOutput vError $ \ in' _ err ->
                return [compileText|
if not #{builtins}.isinstance(#{in'}, #{builtins}.basestring):
    #{err}(
        '',
        'Expected a string of RFC 3339 date & time, '
        'but the given value is not a string.'
    )
try:
    return _parse_iso8601(#{in'})
except _iso8601_parse_error as #{vException}:
    if #{builtins}.str(#{vException}).startswith('Unable to parse'):
        #{err}(
            '',
            'Expected a string of RFC 3339 date & time, '
            'but the string is not a valid RFC 3339 date & time format.'
        )
    else:
        #{err}(
            '',
            'Expected a string of RFC 3339 date & time, '
            'but the date & time is invalid.'
        )
                |]

compilePrimitiveTypeDeserializer Bool vInput vOutput vError = do
    builtins <- importBuiltins
    return [compileText|
if #{builtins}.isinstance(#{vInput}, #{builtins}.bool):
    #{vOutput} = #{vInput}
else:
    #{vError}('', 'Expected a boolean value; true or false.')
    |]

compilePrimitiveTypeDeserializer Uuid vInput vOutput vError =
    wrapScope vInput vOutput vError $ \ in' _ err -> do
        builtins <- importBuiltins
        uuid <- importStandardLibrary "uuid"
        baseString <- baseStringClass
        return [compileText|
if not #{builtins}.isinstance(#{in'}, #{baseString}):
    #{err}(
        '', 'Expected a string of UUID, but the given value is not a string.'
    )
try:
    return #{uuid}.UUID(#{in'})
except #{builtins}.ValueError:
    #{err}(
        '',
        'Expected a string of UUID, '
        'but the string is not a valid hexadecimal UUID format.'
    )
        |]

compilePrimitiveTypeDeserializer Url vInput vOutput vError =
    compilePrimitiveTypeDeserializer Text vInput vOutput vError

-- | Wrap a Python deserializer code block into an isolated scope.
--
-- Without this function, a deserializer code cannot @return@ or @raise@,
-- because it is embeded in the middle of other Python code which shares
-- the flow.  It it controls its flow using @return@ or @raise@ the other
-- code that embeds it also can be terminated together.
--
-- That means a deserializer code should not be like:
--
-- @
--     if not condition_1:
--         #{vError}('', 'It should satisfy the condition_1.')
--     elif not condition_2:
--         #{vError}('', 'It should satisfy the condition_2.')
--     if mode:
--         try:
--             return result()
--         except ValueError:
--             #{vError}('', 'A value is invalid')
--     return result2()
-- @
--
-- but instead be:
--
-- @
--     if condition_1:
--         if not condition_2:
--             if mode:
--                 try:
--                     #{vOutput} = result()
--                 except ValueError:
--                     #{vError}('', 'A value is invalid')
--              else:
--                  #{vOutput} = result2()
--         else:
--             #{vError}('', 'It should satisfy the condition_2.')
--     else:
--         #{vError}('', 'It should satisfy the condition_1.')
-- @
--
-- which is too much indented and using assignig a result to the output
-- variable instead of just returning it.  It is fragile and bug-prone since
-- we can use @return@ or @raise@ by mistake.
--
-- The 'wrapScope' function offers an isolated flow, i.e., a function scope.
-- With this, returning a value is equivalent to assigning it to the output
-- variable, and raising an exception is calling an error function.
wrapScope :: Code -> Code -> Code
          -> (Code -> Code -> Code -> CodeGen Markup)
          -> CodeGen Markup
wrapScope vInput vOutput vError callback = do
    let f = mangleVar vInput "scoped_func"
    let exc = mangleVar vError "exc"
    let scopedInput = mangleVar vInput "scoped"
    let scopedOutput = mangleVar vOutput "scoped"
    let scopedError = mangleVar vError "scoped"
    builtins <- importBuiltins
    scopedCode <- callback scopedInput scopedOutput scopedError
    return [compileText|
def #{f}(#{scopedInput}):
    def #{scopedError}(err_field, err_msg):
        raise ValueError(err_field, err_msg)
    #{indent "    " scopedCode}
    return #{scopedOutput}
try:
    #{vOutput} = #{f}(#{vInput})
except #{builtins}.ValueError as #{exc}:
    if #{builtins}.len(#{exc}.args) == 2:
        #{vError}(*#{exc}.args)
    else:
        raise
|]
