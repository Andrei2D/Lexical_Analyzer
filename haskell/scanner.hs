import System.IO

-- -----------------------------------------------

isIn :: Eq a => a -> [a] -> Bool
isIn _ [] = False
isIn elm lst
    | elm == hd = True
    | otherwise = isIn elm tl
    where
        hd = head lst
        tl = tail lst

isLetter :: Char -> Bool
isLetter chr = chr `isIn` (['a'..'z'] ++ ['A'..'Z'])

isDigit :: Char -> Bool
isDigit chr = chr `isIn` ['0'..'9']

isOperator :: Char -> Bool
isOperator chr = chr `isIn` "+-*/%^=><&|!,.#"

isSeparator :: Char -> Bool
isSeparator chr = chr `isIn` "{}[]()"

-- -----------------------------------------------

data ClsType = Identif | LitIntr | LitFlot |
    LitCrc | LitStr | Operator | Separator | Comment |
    TabSpace | NewLine | Space | Delim
    deriving (Show, Eq)

data Interm = None | FrontSlh | PlusInterm | EqualInterm | 
    ComLoop | ComEnd | LineComm |
    FltExp | FltExpPow | NumbRepr | BinRepr | HexRepr |
    ChrInsInterm | ChrEndInterm | ChrQuotInterm | 
    StrgInterm | StrgQuotInterm
    deriving (Show, Eq)

data State a = F ClsType | I Interm | OutState | Forbidd
    deriving (Show, Eq)

data Token = T ClsType String | ErrTok String

instance Show Token where
    show (ErrTok st)  = "<ErrToken, " ++ st ++ ">"
    show (T c s) = "<" ++ show c ++ " " ++ show s  ++ ">"


-- ------------------------------------------------

stateIsFinal :: State a -> Bool
stateIsFinal (F _) = True
stateIsFinal _ = False

lambdaTrans :: State a -> State a
lambdaTrans (I FrontSlh) = (F Operator)
lambdaTrans (I EqualInterm) = (F Operator)
lambdaTrans (I PlusInterm) = (F Operator)
lambdaTrans (I FltExpPow) = (F LitFlot)
lambdaTrans (I NumbRepr) = (F LitIntr)
lambdaTrans (I BinRepr) = (F LitIntr)
lambdaTrans (I HexRepr) = (F LitIntr)
lambdaTrans st = st

tranzFun :: State a -> Char -> State a
tranzFun (I None) '0' = (I NumbRepr)
tranzFun (I None) chr
    | isLetter chr = (F Identif)
    | isDigit chr = (F LitIntr)
    | isSeparator chr = (F Separator)

-- Particular operators
tranzFun (I None) ' ' = (F Space)
tranzFun (I None) '\t' = (F TabSpace)
tranzFun (I None) '\n' = (F NewLine)
tranzFun (I None) '/' = (I FrontSlh)
tranzFun (I None) '+' = (I PlusInterm)
tranzFun (I None) '-' = (I EqualInterm)
tranzFun (I None) '*' = (I EqualInterm)
tranzFun (I None) '>' = (I EqualInterm)
tranzFun (I None) '<' = (I EqualInterm)
tranzFun (I None) '=' = (I EqualInterm)
tranzFun (I None) ';' = (F Delim)
tranzFun (I None) '\'' = (I ChrInsInterm)
tranzFun (I None) '"' = (I StrgInterm)

tranzFun (I None) chr 
    | isOperator chr = (F Operator)

tranzFun (F Identif) chr
    | isDigit chr = (F Identif)
    | isLetter chr = (F Identif)
    | chr == '_' = (F Identif)

-- Integral literal related
tranzFun (F LitIntr) chr
    | isDigit chr = (F LitIntr)
    | chr == '.' = (F LitFlot)
    | chr == 'e' = (I FltExp)
    | isLetter chr = Forbidd

tranzFun (I NumbRepr) chr
    | chr == 'b' = (I BinRepr)
    | chr == 'x' = (I HexRepr)
    | isDigit chr = (F LitIntr)

tranzFun (I BinRepr) chr
    | chr == '0' = (I BinRepr)
    | chr == '1' = (I BinRepr)
    | isDigit chr = Forbidd
    | isLetter chr = Forbidd
tranzFun (I HexRepr) chr
    | isDigit chr = (I HexRepr)
    | chr `isIn` ['a'..'f'] =  (I HexRepr)
    | chr `isIn` ['A'..'F'] =  (I HexRepr)
    | isLetter chr = Forbidd

-- Float literal related
tranzFun (F LitFlot) chr
    | isDigit chr = (F LitFlot)
    | chr == 'e' = (I FltExp)
    | isLetter chr = Forbidd

tranzFun (I FltExp) chr
    | chr == '+' = (I FltExpPow)
    | chr == '-' = (I FltExpPow)
    | isDigit chr = (I FltExpPow)
    | otherwise = Forbidd
tranzFun (I FltExpPow) chr
    | isDigit chr = (F LitFlot)

-- Intermediary states for operators
tranzFun (I EqualInterm) chr
    | chr == '=' = (F Operator)
tranzFun (I FrontSlh) chr
    | chr == '=' = (F Operator)
    | chr == '*' = (I ComLoop)
    | chr == '/' = (I LineComm)
tranzFun (I PlusInterm) chr
    | chr == '=' = (F Operator)
    | chr == '+' = (F Operator)

-- Char and strings
tranzFun (I ChrInsInterm) chr
    | chr == '\\' = (I ChrQuotInterm)
    | otherwise = (I ChrEndInterm)
tranzFun (I ChrQuotInterm) chr
    | chr == '\'' = (I ChrEndInterm)
tranzFun (I ChrEndInterm) chr
    | chr == '\'' = (F LitCrc)
tranzFun (I StrgInterm) chr
    | chr == '"' = (F LitStr)
    | chr == '\n' = (I StrgInterm)
    | chr == '\\' = (I StrgQuotInterm)
    | otherwise = (I StrgInterm)
tranzFun (I StrgQuotInterm) chr
    | chr == '"' = (I StrgInterm)

-- Comment cases
tranzFun (I ComLoop) chr
    | chr == '*' = (I ComEnd)
    | otherwise = (I ComLoop)
tranzFun (I ComEnd) chr
    | chr == '*' = (I ComEnd) 
    | chr == '/' = (F Comment) 
    | otherwise = (I ComLoop)
tranzFun (I LineComm) chr
    | chr == '\n' = (F Comment) 
    | otherwise = (I LineComm)

tranzFun _ _ = OutState

-- @TODO: Comments; 

-- CurrState -> CurrBuild -> BigString -> IntermTokens -> FinalTokens
scanner :: State a -> String -> String -> [Token] -> [Token]
scanner st word [] tok
    | stateIsFinal st = finTokList
    | stateIsFinal trzSt = trzTokList
    | otherwise = tok ++ (ErrTok word):[]
    where
        F fSt = st
        newToken = (T fSt word)
        finTokList = tok ++ newToken:[]   -- end 1st case
        trzSt = lambdaTrans st
        F finTrSt = trzSt
        trzToken = (T finTrSt word)
        trzTokList = tok ++ trzToken:[]    -- end 2ndcase

scanner st word code tok
    | nxtState == Forbidd = errTokens
    | nxtState /= OutState = scanner nxtState newWord restCode tok 
    | stateIsFinal st = scanner (I None) "" code finTokList
    | stateIsFinal trzSt = scanner (I None) "" code trzTokList
    | otherwise = errTokens
    where
        chr = head code
        nxtState = tranzFun st chr
        newWord = word ++ chr:[]
        restCode = tail code               -- end 1st case
        F fSt = st
        newToken = (T fSt word)
        finTokList = tok ++ newToken:[]    -- end 2nd case
        trzSt = lambdaTrans st
        F finTrSt = trzSt
        trzToken = (T finTrSt word)
        trzTokList = tok ++ trzToken:[]    -- end 3rd case
        errTokens = tok ++ (ErrTok (show st ++ newWord)):[]


-- -----------------------------------------------

lexicalAnalisys :: String -> [Token]
lexicalAnalisys code = scanner (I None) "" code []

tokensPrint :: [Token] -> IO ()
tokensPrint [] = print "_________"
tokensPrint tok = do
    print hd
    tokensPrint tl
    where 
        hd = head tok
        tl = tail tok

lxTest :: IO ()
lxTest = do
    handle <- openFile "in.txt" ReadMode
    input <- hGetContents handle
    let output = lexicalAnalisys input
    tokensPrint output

action :: String -> String
action input = unlines . map show . lexicalAnalisys $ input

main :: IO ()
main = do
    handle <- openFile "in.txt" ReadMode
    input <- hGetContents handle
    let output = action input
    writeFile "out.txt" output




-- -------------------------------------------

{-
Its starts with the definition of DFA. 
Recall that we defined a DFA as a 5-tuple M = (Q, Σ, δ, s, F) 
    where Q, Σ are finite sets, δ : Q × Σ → Q, s ∈ Q and F ⊆ Q.
-}