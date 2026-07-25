-- This is praesidium's lexer, it transforms a string into a list of tokens.
{
module Core.Prlexer where
import Data.Char(toLower)
import Data.List(foldl')
}

%wrapper "posn"

-- MACROS
$digit = 0-9
$alphabet = [a-zA-Z]
$alpha_numeric = [$alphabet $digit]

-- IDENTIFIERS need to start with a letter or a underscore. It stops numbers with underscores being recognized as IDENTIFIERS.
@identifier = [$alphabet \_] [$alpha_numeric \_]*
@string = \" ([^\"\\] | \\.)* \"
@operator = "=>" | "->" | "==" | "!=" | "<=" | ">=" | ".." | "&&" | "\|\|" | "!" | "=" |
          "<" | ">" | "+" | "-" | "*" | "/" | "%" | "(" | ")" | "{" | "}" | "[" | "]" | ";"
          | "," | ":" | "\|"

tokens :-
  -- USELESS
  $white+               ;
  "//"                  ;

  -- LITERALS
  @string               {\p s -> STRING p (unescape(init(tail s)))}
  $digit+               {\p s -> INT p (read s)}
  $digit+ \. $digit+    {\p s -> DOUBLE p (read s)}
  0 [xX] [0-9a-fA-F]+   {\p s -> INT p (converthex(drop 2 s))}
  0 [bB] [01]+          {\p s -> INT p (convertbin(drop 2 s))}
  
  -- OPERATORS IDENTIFIERS
  @identifier           {\p s -> isid p s}
  @operator             {\p s -> isop p s}

  .                     {\p s -> ERROR p s}

{
-- UTILS, most are self explanatory so no comments for them
converthex :: String -> Integer
converthex = foldl' step 0
  where
    step v c | c >= '0' && c <= '9' = v * 16 + fromIntegral(fromEnum c - fromEnum '0')
             | c >= 'a' && c <= 'f' = v * 16 + fromIntegral(fromEnum c - fromEnum 'a' + 10)
             | c >= 'A' && c <= 'F' = v * 16 + fromIntegral(fromEnum c - fromEnum 'A' + 10)
             | otherwise            = v

convertbin :: String -> Integer
convertbin = foldl' step 0
  where
    step v '0' = v * 2
    step v '1' = v * 2 + 1
    step v _   = v

unescape :: String -> String
unescape('\\':c:r) = case c of
  'n'  -> '\n' : unescape r
  't'  -> '\t' : unescape r
  'r'  -> '\r' : unescape r
  '\\' -> '\\' : unescape r
  '\"' -> '\"' : unescape r
  '\'' -> '\'' : unescape r
  _    -> c    : unescape r
unescape(c:r) = c : unescape r
unescape [] = []

-- TOKEN DATA TYPE
data TOKEN
  = LET AlexPosn | IF AlexPosn | ELSE AlexPosn| OR AlexPosn 
  | NOR AlexPosn | XOR AlexPosn | XNOR AlexPosn | AND AlexPosn 
  | NAND AlexPosn | NOT AlexPosn | UNIT AlexPosn | TRUE AlexPosn | FALSE AlexPosn

  | STRING AlexPosn String
  | DOUBLE AlexPosn Double
  | INT AlexPosn Integer
  | UNDERLINE AlexPosn
  | IDENTIFIER AlexPosn String

  | FAT_ARROW AlexPosn | ARROW AlexPosn | EQUAL AlexPosn | NOT_EQUAL AlexPosn
  | LESS_EQUAL AlexPosn | GREAT_EQUAL AlexPosn | DDOT AlexPosn | AND_OP AlexPosn
  | OR_OP AlexPosn | NOT_OP AlexPosn

  | TYPE_IO AlexPosn | TYPE_STR AlexPosn | TYPE_NAT AlexPosn | TYPE_INT AlexPosn
  | TYPE_BOOL AlexPosn | TYPE_DBL AlexPosn | TYPE_LIST AlexPosn | TYPE_VECT AlexPosn

  | ASSIGN AlexPosn | LESS AlexPosn | GREATER AlexPosn | SUM AlexPosn
  | MINUS AlexPosn | MUL AlexPosn | DIV AlexPosn | MOD AlexPosn
  | LPAREN AlexPosn | RPAREN AlexPosn | LBRACE AlexPosn | RBRACE AlexPosn
  | LBRACKET AlexPosn | RBRACKET AlexPosn | SEMICOLON AlexPosn
  | COMMA AlexPosn | COLON AlexPosn | PIPE AlexPosn

  | ERROR AlexPosn String
  deriving (Eq)

-- NOTE: For future updates, adding one line here is adding a new keyword (IF YOU SPECIFY IN THE TOKEN DATA TYPE TOO).
isid :: AlexPosn -> String -> TOKEN
isid p s = case s of
  "let"     -> LET p
  "if"      -> IF p
  "else"    -> ELSE p
  "or"      -> OR p
  "nor"     -> NOR p
  "xor"     -> XOR p
  "xnor"    -> XNOR p
  "and"     -> AND p
  "nand"    -> NAND p
  "not"     -> NOT p
  "Unit"    -> UNIT p
  "true"    -> TRUE p
  "false"   -> FALSE p
  "IO"      -> TYPE_IO p
  "String"  -> TYPE_STR p
  "Nat"     -> TYPE_NAT p
  "Int"     -> TYPE_INT p
  "Bool"    -> TYPE_BOOL p
  "Double"  -> TYPE_DBL p
  "List"    -> TYPE_LIST p
  "Vect"    -> TYPE_VECT p
  "_"       -> UNDERLINE p
  _         -> IDENTIFIER p s

-- NOTE: The same thing than before but with operators, however rarely a new operator will be added.
isop :: AlexPosn -> String -> TOKEN
isop p s = case s of
  "=>"  -> FAT_ARROW p
  "->"  -> ARROW p
  "=="  -> EQUAL p
  "!="  -> NOT_EQUAL p
  "<="  -> LESS_EQUAL p
  ">="  -> GREAT_EQUAL p
  ".."  -> DDOT p
  "&&"  -> AND_OP p
  "||"  -> OR_OP p
  "!"   -> NOT_OP p
  "="   -> ASSIGN p
  "<"   -> LESS p
  ">"   -> GREATER p
  "+"   -> SUM p
  "-"   -> MINUS p
  "*"   -> MUL p
  "/"   -> DIV p
  "%"   -> MOD p
  "("   -> LPAREN p
  ")"   -> RPAREN p
  "{"   -> LBRACE p
  "}"   -> RBRACE p
  "["   -> LBRACKET p
  "]"   -> RBRACKET p
  ";"   -> SEMICOLON p
  ","   -> COMMA p
  ":"   -> COLON p
  "|"   -> PIPE p
  _     -> ERROR p ("Unexpected operator " ++ s)

-- This is just a helper func, rarely will change
format :: AlexPosn -> String -> String
format (AlexPn _ l c) name = "(" ++ show l ++ ":" ++ show c ++ ") " ++ name

instance Show TOKEN where
  show t = case t of
    LET p          -> format p "LET"
    IF p           -> format p "IF"
    ELSE p         -> format p "ELSE"
    OR p           -> format p "OR"
    NOR p          -> format p "NOR"
    XOR p          -> format p "XOR"
    XNOR p         -> format p "XNOR"
    AND p          -> format p "AND"
    NAND p         -> format p "NAND"
    NOT p          -> format p "NOT"
    UNIT p         -> format p "UNIT"
    TRUE p         -> format p "TRUE"
    FALSE p        -> format p "FALSE"

    STRING p s     -> format p ("STRING " ++ show s)
    DOUBLE p d     -> format p ("DOUBLE " ++ show d)
    INT p i        -> format p ("INT " ++ show i)
    UNDERLINE p    -> format p "UNDERLINE"
    IDENTIFIER p s -> format p ("IDENTIFIER " ++ show s)

    FAT_ARROW p    -> format p "FAT_ARROW"
    ARROW p        -> format p "ARROW"
    EQUAL p        -> format p "EQUAL"
    NOT_EQUAL p    -> format p "NOT_EQUAL"
    LESS_EQUAL p   -> format p "LESS_EQUAL"
    GREAT_EQUAL p  -> format p "GREAT_EQUAL"
    DDOT p         -> format p "DDOT"
    AND_OP p       -> format p "AND_OP"
    OR_OP p        -> format p "OR_OP"
    NOT_OP p       -> format p "NOT_OP"

    TYPE_IO p      -> format p "TYPE_IO"
    TYPE_STR p     -> format p "TYPE_STR"
    TYPE_NAT p     -> format p "TYPE_NAT"
    TYPE_INT p     -> format p "TYPE_INT"
    TYPE_BOOL p    -> format p "TYPE_BOOL"
    TYPE_DBL p     -> format p "TYPE_DBL"
    TYPE_LIST p    -> format p "TYPE_LIST"
    TYPE_VECT p    -> format p "TYPE_VECT"

    ASSIGN p       -> format p "ASSIGN"
    LESS p         -> format p "LESS"
    GREATER p      -> format p "GREATER"
    SUM p          -> format p "SUM"
    MINUS p        -> format p "MINUS"
    MUL p          -> format p "MUL"
    DIV p          -> format p "DIV"
    MOD p          -> format p "MOD"
    LPAREN p       -> format p "LPAREN"
    RPAREN p       -> format p "RPAREN"
    LBRACE p       -> format p "LBRACE"
    RBRACE p       -> format p "RBRACE"
    LBRACKET p     -> format p "LBRACKET"
    RBRACKET p     -> format p "RBRACKET"
    SEMICOLON p    -> format p "SEMICOLON"
    COMMA p        -> format p "COMMA"
    COLON p        -> format p "COLON"
    PIPE p         -> format p "PIPE"

    ERROR p s      -> format p ("ERROR " ++ s)

main :: IO ()
main = do 
  s <- getContents
  print(alexScanTokens s)
}
