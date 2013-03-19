{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module PacketParser (
  CType(..),
  Field(..),
  Packet(..),
  packetName,
  isPrim,
  wsTemp,
  parsePackets
  ) where

import Control.Monad
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec

{-

packet <name> {
  <C_type> <field_name>;
  ...
}

-}

data CType =
    CString
  | Prim String
  | User String
  | Vector CType
  | Map CType CType

data Field = Field CType String
data Packet = Packet String [Field] deriving Show

instance Show CType where
  show CString = "std::string"
  show (Prim s) = s
  show (User s) = s
  show (Vector t) = "std::vector<" ++ show t ++ wsTemp t ++ ">"
  show (Map t1 t2) = "std::map<" ++ show t1 ++ ", " ++ show t2 ++
                     wsTemp t2 ++ ">"

instance Show Field where
  show (Field t n) = show t ++ ' ' : n
  
packetName :: Packet -> String
packetName (Packet n _) = n

isPrim :: CType -> Bool
isPrim (Prim _) = True
isPrim _ = False

wsTemp :: CType -> String
wsTemp (Vector _) = " "
wsTemp (Map _ _) = " "
wsTemp _ = ""

skip :: Parser a -> Parser ()
skip = liftM (const ())

spaces1 :: Parser ()
spaces1 = skip (many1 space)

vectorP :: Parser CType
vectorP = do
  string "vector"
  spaces
  char '<'
  spaces
  t <- typeP
  spaces
  char '>'
  return (Vector t)
  
mapP :: Parser CType
mapP = do
  string "map"
  spaces
  char '<'
  spaces
  k <- typeP
  spaces
  char ','
  spaces
  v <- typeP
  spaces
  char '>'
  return (Map k v)
  
numTypeP :: Bool -> Parser String
numTypeP unsigned =
  string "char" <|> string "int" <|> short <|>
  case unsigned of
    True -> long True
    False -> string "bool" <|> string "float" <|>
             string "double" <|> long False
  where
    intOpt s = (try (spaces1 >> string "int") <|> return "") >> return s
    short = do
      string "short"
      intOpt "short"
    long b = do
      string "long"
      (case b of
        True -> fail "Doubles may not be unsigned"
        False -> try (spaces1 >> string "double" >> return "long double")
        ) <|>
        try (spaces1 >> string "long" >> intOpt "long long") <|>
        intOpt "long"
  
unsignedP :: Parser CType
unsignedP = do
  string "unsigned"
  spaces1
  t <- numTypeP True <?> "Illegal use of unsigned"
  return (Prim ("unsigned " ++ t))
  
primitiveP :: Parser CType
primitiveP = unsignedP <|> liftM Prim (numTypeP False)

nameP :: Parser String
nameP = do
  first <- letter <|> char '_'
  rest <- many (alphaNum <|> char '_')
  return (first : rest)

typeP :: Parser CType
typeP = try vectorP <|> try mapP <|> try primitiveP <|> try stringP <|> userP
  where
    stringP = string "string" >> return CString
    userP = liftM User nameP
  
commentP :: Parser (Maybe Field)
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
  return (Just (Field ctype (fname ++ "_")))

commentPacketP :: Parser (Maybe Packet)
commentPacketP = string "/*" >> findClose >> return Nothing
  where
    findClose = do
      skipMany (noneOf "*")
      char '*'
      skip (char '/') <|> findClose
      spaces

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