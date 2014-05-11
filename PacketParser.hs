{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module PacketParser (
  CType(..),
  Field(..),
  Packet(..),
  packetName,
  isPrim,
  wsGeneric,
  parsePackets,
  parseIncludes
  ) where

import Constants (reservedWords)
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (member)
import Text.ParserCombinators.Parsec

{-

packet <name> {
  <C_type> <field_name>;
  ...
}

-}

-- | Represents a C++ type.
data CType =
    CString
  | Prim String
  | User String
  | Pair CType CType
  | Vector CType
  | List CType
  | Map CType CType

-- | Represents a named field in a packet.
data Field = Field CType String

-- | Represents a parsed packet.
data Packet = Packet String [Field] deriving Show

instance Show CType where
  show CString = "std::string"
  show (Prim s) = s
  show (User s) = s
  show (Pair t1 t2) = "std::pair<" ++ show t1 ++ ", " ++ show t2 ++
                      wsGeneric t2 ++ ">"
  show (Vector t) = "std::vector<" ++ show t ++ wsGeneric t ++ ">"
  show (List t) = "std::list<" ++ show t ++ wsGeneric t ++ ">"
  show (Map t1 t2) = "std::map<" ++ show t1 ++ ", " ++ show t2 ++
                     wsGeneric t2 ++ ">"

instance Show Field where
  show (Field t n) = show t ++ ' ' : n
  
-- | Gets the user-defined packet name.
packetName :: Packet -> String
packetName (Packet n _) = n

-- | Gets whether the given type is a C++ primitive. (String is not.)
isPrim :: CType -> Bool
isPrim (Prim _) = True
isPrim _ = False

-- | Gets the whitespace needed to avoid a parsing error on a generic type,
--   e.g. to write "vector<vector<int> >" instead of "vector<vector<int>>".
wsGeneric :: CType -> String
wsGeneric (Pair _ _) = " "
wsGeneric (Vector _) = " "
wsGeneric (List _) = " "
wsGeneric (Map _ _) = " "
wsGeneric _ = ""

-- | Parses and ignores something.
skip :: Parser a -> Parser ()
skip = liftM (const ())

-- | Parses and ignores at least one space.
spaces1 :: Parser ()
spaces1 = skip (many1 space)

-- | A map from strings to types of 1-parameter template types.
template1PMap :: Map String (CType -> CType)
template1PMap = M.fromList [("list", List), ("vector", Vector)]

-- | Parses a 1-parameter template type with the given name @c@.
template1P :: String -> Parser CType
template1P c = do
  string c
  spaces
  char '<'
  spaces
  t <- typeP
  spaces
  char '>'
  case M.lookup c template1PMap of
    Nothing -> fail ("Type " ++ c ++ " is not a 1-parameter template type.")
    Just typeCtor -> return (typeCtor t)

-- | A map from strings to types of 2-parameter template types.
template2PMap :: Map String (CType -> CType -> CType)
template2PMap = M.fromList [("map", Map), ("pair", Pair)]

-- | Parses a 2-parameter template type with the given name @c@.
template2P :: String -> Parser CType
template2P c = do
  string c
  spaces
  char '<'
  spaces
  t1 <- typeP
  spaces
  char ','
  spaces
  t2 <- typeP
  spaces
  char '>'
  case M.lookup c template2PMap of
    Nothing -> fail ("Type " ++ c ++ " is not a 2-parameter template type.")
    Just typeCtor -> return (typeCtor t1 t2)

-- | Parses a numeric type string, possibly unsigned.
numTypeP :: Bool -> Parser String
numTypeP unsigned =
  string "char" <|> string "int" <|> short <|>
  case unsigned of
    True -> long True
    False -> string "bool" <|> string "float" <|>
             string "double" <|> long False
  where
    -- | Parses an additional "int" (useful for "short int" and "long int").
    intOpt s = (try (spaces1 >> string "int") <|> return "") >> return s
    -- | Parses "short" or "short int".
    short = do
      string "short"
      intOpt "short"
    -- | Parses either "long", "long long", or "long double".
    long unsigned = do
      string "long"
      (case unsigned of
        True -> fail "Doubles may not be unsigned"
        False -> try (spaces1 >> string "double" >> return "long double")
        ) <|>
        try (spaces1 >> string "long" >> intOpt "long long") <|>
        intOpt "long"

-- | Parses an unsigned type.
unsignedP :: Parser CType
unsignedP = do
  string "unsigned"
  spaces1
  t <- numTypeP True <?> "Illegal use of unsigned"
  return (Prim ("unsigned " ++ t))

-- | Valid primitive typedefs.
validTypedefs :: [Parser String]
validTypedefs = map string ["size_t", "time_t"]

-- | Parses a typedef'd primitive, such as "time_t" or "size_t".
typedefP :: Parser CType
typedefP = liftM Prim (msum validTypedefs)

-- | Parses any primitive type.
primitiveP :: Parser CType
primitiveP = try typedefP <|> unsignedP <|> liftM Prim (numTypeP False)

-- | Parses a user-given field name.
nameP :: Parser String
nameP = do
  first <- letter <|> char '_'
  rest <- many (alphaNum <|> char '_')
  name <- return (first : rest)
  case member name reservedWords of
    True -> fail ("Cannot name field reserved word: " ++ name)
    False -> return name

-- | Parses any C++ type.
typeP :: Parser CType
typeP = try (template1P "vector") <|> try (template1P "list") <|>
        try (template2P "map") <|> try (template2P "pair") <|>
        try primitiveP <|> try stringP <|> userP
  where
    -- | Parses a String type.
    stringP = string "string" >> return CString
    -- | The default is a user-defined (unknown) type.
    userP = liftM User nameP

-- | Parses a comment (// ...) on a field.
commentP :: Parser (Maybe a)
commentP = do
  string "//"
  many (noneOf "\n")
  spaces
  return Nothing
  
fieldP :: Parser (Maybe Field)
fieldP = commentP <|> do
  ctype <- typeP
  spaces1
  fname <- nameP
  spaces
  char ';'
  spaces
  -- An underscore is appended for C++ field-name style.
  return (Just (Field ctype (fname ++ "_")))

-- | Parses a top-level (/* ... */) comment on a packet.
commentPacketP :: Parser (Maybe Packet)
commentPacketP = string "/*" >> findClose >> return Nothing
  where
    -- | Parses a comment until */ is found.
    findClose = do
      skipMany (noneOf "*")
      char '*'
      skip (char '/') <|> findClose
      spaces

-- | Parses a whole packet.
packetP :: Parser (Maybe Packet)
packetP = commentPacketP <|> do
  string "packet"
  spaces1
  name <- nameP
  spaces
  char '{'
  spaces
  fields <- liftM catMaybes (many fieldP)
  char '}'
  spaces
  return (Just (Packet name fields))

-- | Parses a whole packet file, possibly with multiple packets.
packetFileP :: Parser [Packet]
packetFileP = do
  spaces
  packets <- liftM catMaybes (many packetP)
  eof
  return packets
  
-- | Parses a .packet file and returns a list of packets, or an error
parsePackets :: FilePath -> IO (Either ParseError [Packet])
parsePackets = parseFromFile packetFileP

_packet :: String -> IO ()
_packet file = do
  res <- parseFromFile packetFileP file
  case res of
    Left error -> print error
    Right packets -> print packets

-- | Parses the name of a header file.
--   TODO: Validation?
headerP :: Parser FilePath
headerP = many1 (alphaNum <|> char '_' <|> char '.' <|> char '-')

-- | Parses an include statement of the form "type:header".
includeP :: Parser (Maybe (String, FilePath))
includeP = do
  ctype <- nameP
  spaces
  char ':'
  spaces
  hname <- headerP
  spaces
  char ';'
  spaces
  return $ Just (ctype, hname)

-- | Parses a list of inclusion statements.
includesP :: Parser (Map String FilePath)
includesP = liftM (M.fromList . catMaybes) $ many (commentP <|> includeP)
    
-- | Parses an inclusions file
parseIncludes :: FilePath -> IO (Either ParseError (Map String FilePath))
parseIncludes = parseFromFile includesP