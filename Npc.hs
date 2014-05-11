{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Writer hiding (All)
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.FilePath.Posix

import PacketParser

type CppWriter = Writer String ()

{---------- Utility Functions ----------}

tellLn :: MonadWriter String m => String -> m ()
tellLn w = do
  tell w
  tell "\n"
  
lnBreak :: MonadWriter String m => m ()
lnBreak = tell "\n"

-- | Generates the comment given the file name
autoComment :: String -> String
autoComment f = "/* *** " ++ f ++ ": Auto-generated using npc *** */\n"

-- | Capitalizes a word
capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

-- | Determines what files need to be included for a type
has :: Map String FilePath -> CType -> Set String
has _ CString = S.singleton "<string>"
has m (Pair t1 t2) = S.insert "<utility>" (S.union (has m t1) (has m t2))
has m (Vector t) = S.insert "<vector>" (has m t)
has m (List t) = S.insert "<list>" (has m t)
has m (Map k v) = S.insert "<map>" (S.union (has m k) (has m v))
has m (User t) = S.singleton ('\"' : m M.! t ++ "\"")
has _ (Prim "time_t") = S.singleton "<ctime>"
has _ (Prim "size_t") = S.singleton "<cstdlib>"
has _ (Prim _) = S.empty

-- | Determines what files need to be included for a set of packets
includes :: Map String FilePath -> [Packet] -> Set String
includes m = foldl (\inc (Packet _ fs) -> S.union inc $
                      (foldl (\inc2 (Field t _) ->
                               S.union inc2 (has m t)) S.empty fs)) S.empty

-- | Takes the name of a class and converts it to CONSTANT_CASE
constantCase :: String -> String
constantCase "" = ""
constantCase (c:cs) = toUpper c : constantCase' cs
  where
    constantCase' "" = ""
    constantCase' (c:cs)
      | isUpper c = '_' : c : constantCase' cs
      | otherwise = toUpper c : constantCase' cs

-- | Gets the type string for a const-correct type
ccType :: CType -> String
ccType (Prim t) = t ++ " "
ccType t = "const " ++ show t ++ " &"

{---------- Class Definitions (.h) ----------}

-- | Writes the header inclusions
writeIncludes :: Map String FilePath -> [Packet] -> CppWriter
writeIncludes m ps =
  forM_ (S.toList (includes m ps)) (\f -> tellLn ("#include " ++ f))

-- | Writes a field's getter, optionally making it const
getter :: Field -> Bool -> CppWriter
getter (Field t n) const = do
  tell "  "
  tell (if const then ccType t else show t ++ " &")
  tell (init n)
  tell "() "
  when const (tell "const ")
  tellLn $ "{ return " ++ n ++ "; }"
                     
-- | Write's a field's setter
setter :: Field -> CppWriter
setter (Field t n) = do
  tell "  void set"
  tell $ init (capitalize n)
  tell $ '(' : ccType t
  tell "value) { "
  tell n
  tellLn " = value; }"

-- | Writes the accessor methods. Primitives do not need non-const getters
writeAccessors :: [Field] -> CppWriter
writeAccessors fds = do
  forM_ fds (\fd@(Field t _)  -> do
                when (not (isPrim t)) (getter fd False)
                getter fd True
                setter fd
                lnBreak
            )
    
-- | Makes the initialization list constructor
makeCtor :: String -> [Field] -> CppWriter
makeCtor n fds = do
  tell n
  tell "("
  mapM_ tell args
  tell ") : Packet("
  tell $ constantCase n
  tell "), "
  mapM_ tell initList
  tellLn " { }"
  where
    args = intersperse ", " (map argify fds)
    argify (Field t n) = ccType t ++ n
    initList = intersperse ", " (map initF fds)
    initF (Field _ f) = f ++ '(' : f ++ ")"

-- | Makes the default and initialization list constructors
writeCtors :: Packet -> CppWriter
writeCtors (Packet n fds) = do
  tellLn ("  " ++ n ++ "() : Packet(" ++ constantCase n ++ ") { }")
  when (not (null fds)) $ tell "  " >> makeCtor n fds
  lnBreak

-- | Writes the class that appears in the .h file
writeClass :: Packet -> CppWriter
writeClass p@(Packet n fds) = do
  tellLn ("class " ++ n ++ " : public Packet {")
  tellLn "private:"
  forM_ fds (\fd -> do
                tell "  "
                tell (show fd)
                tellLn ";")
  lnBreak
  tellLn "public:"
  writeCtors p
  writeAccessors fds
  tellLn "  unsigned int SerializedSize() const;"
  tellLn "  void Serialize(ByteBuffer &buffer) const;"
  tellLn "  void Deserialize(ByteBuffer &buffer);"
  tellLn "};\n"
  
-- | Writes the header file for a group of packets
writePacketHeader :: String -> [Packet] -> Map String FilePath -> CppWriter
writePacketHeader hdr ps incs = do
  tellLn $ autoComment (hdr ++ ".h")
  tellLn ("#ifndef " ++ guard)
  tellLn ("#define " ++ guard)
  lnBreak
  writeIncludes incs ps
  tellLn "#include \"bytebuffer.h\""
  tellLn "#include \"packet.h\"\n"
  mapM_ writeClass ps
  tell ("#endif // " ++ guard)
  where
    guard = "__" ++ constantCase hdr ++ "_HEADER__"

-- | The constant Packet class
packetClass :: [String]
packetClass = [
  "class Packet {",
  "protected:",
  "  PacketType type;",
  "  unsigned long seqNum;\n",
  "public:",
  "  Packet(PacketType t) : type(t) { }",
  "  virtual ~Packet() { }\n",
  "public:",
  "  PacketType GetPacketType() const { return type; }",
  "  std::string TypeString() const;",
  "  unsigned long SeqNum() const { return seqNum; }",
  "  void SetSeqNum(unsigned long s) { seqNum = s; }\n",
  "  virtual unsigned int SerializedSize() const {",
  "    return sizeof(PacketType) + sizeof(unsigned long);",
  "  }",
  "  virtual void Serialize(ByteBuffer &b) const {", 
  "    b.SerializeInt(static_cast<int>(type));",
  "    b.SerializeLong(seqNum);",
  "  }",
  "  virtual void Deserialize(ByteBuffer &b) { seqNum = b.DeserializeLong(); }\n",
  "  // Returned packet must be deleted",
  "  static Packet *DeserializePacket(ByteBuffer &b);",
  "};\n"
  ]

-- | Write a deserialize case for a packet
writePacketCase :: Bool -> Packet -> CppWriter
writePacketCase b (Packet n _) = do
  if b then tell "  if (type == " else tell "  else if (type == "
  tell (constantCase n)
  tellLn ") {"
  tellLn ("    " ++ n ++ " *packet = new " ++ n ++ "();")
  tellLn "    packet->type = type;"
  tellLn "    packet->Deserialize(b);"
  tellLn "    return packet;"
  tellLn "  }"
              
-- | Writes the packet.cpp file
writePacketImpl :: [String] -> [Packet] -> CppWriter
writePacketImpl fnames ps = do
  tellLn (autoComment "packet.cpp")
  tellLn "#include <string>"
  tellLn "#include \"packet.h\""
  tellLn "#include \"bytebuffer.h\""
  forM_ fnames (\fn -> tellLn ("#include \"" ++ fn ++ ".h\""))
  tellLn "\nstd::string Packet::TypeString() const {"
  tellLn "  switch(type) {"
  forM_ ps (\(Packet n _) -> do
               tellLn ("  case " ++ constantCase n ++ ":")
               tellLn ("    return \"" ++ constantCase n ++ "\";"))
  tellLn "  default:"
  tellLn "    return \"<invalid>\";"
  tellLn "  }"
  tellLn "}\n"
  tellLn "Packet *Packet::DeserializePacket(ByteBuffer &b) {"
  tellLn "  PacketType type = static_cast<PacketType>(b.DeserializeInt());"
  case ps of
    [] -> return ()
    (p:ps) -> do
      writePacketCase True p
      forM_ ps (writePacketCase False)
  tellLn "  return NULL;"
  tellLn "}"
              
-- | Writes the main header file, including the Packet class and enum of
--   packet types
writeMainHeader :: [Packet] -> CppWriter
writeMainHeader ps = do
  tellLn (autoComment "packet.h")
  tellLn "#ifndef __PACKET_HEADER__"
  tellLn "#define __PACKET_HEADER__\n"
  tellLn "#include <string>"
  tellLn "#include \"bytebuffer.h\"\n"
  tellLn "enum PacketType {"
  forM_ ps (\p -> do
               tell "  "
               tell $ constantCase (packetName p)
               tellLn ",")
  tellLn "};\n"
  mapM_ tellLn packetClass
  tell "#endif // __PACKET_HEADER__"
  
{---------- Serialization Methods (.cpp) ----------}
  
-- | The code to find the size of a serialized pair
serSizePair :: CType -> CType -> String -> String -> String
serSizePair (Prim t1) (Prim t2) _ ws =
  ws ++ "__size += sizeof(" ++ t1 ++ ") + sizeof(" ++ t2 ++ ");"
serSizePair (Prim t1) t2 n ws =
  ws ++ "__size += sizeof(" ++ t1 ++ ");\n" ++
  ws ++ ccType t2 ++ tmp2 ++ " = " ++ n ++ ".second;\n" ++
  serializedSize (Field t2 tmp2) ws
  where tmp2 = "tmp2_" ++ n
serSizePair t1 (Prim t2) n ws =
  ws ++ ccType t1 ++ tmp1 ++ " = " ++ n ++ ".first;\n" ++
  serializedSize (Field t1 tmp1) ws ++ "\n" ++
  ws ++ "__size += sizeof(" ++ t2 ++ ");"
  where tmp1 = "tmp1_" ++ n
serSizePair t1 t2 n ws =
  ws ++ ccType t1 ++ tmp1 ++ " = " ++ n ++ ".first;\n" ++
  serializedSize (Field t1 tmp1) ws ++ "\n" ++
  ws ++ ccType t2 ++ tmp2 ++ " = " ++ n ++ ".second;\n" ++
  serializedSize (Field t2 tmp2) ws
  where
    tmp1 = "tmp1_" ++ n
    tmp2 = "tmp2_" ++ n
      
-- | The code to find the size of a serialized vector or list
serSizeList :: String -> CType -> String -> String -> String
serSizeList _ (Prim t) n ws =
  ws ++ "__size += sizeof(unsigned int);\n" ++
  ws ++ "__size += " ++ n ++ ".size() * sizeof(" ++ t ++ ");"
serSizeList c t n ws =
  ws ++ "__size += sizeof(unsigned int);\n" ++
  ws ++ "for (std::" ++ c ++ "<" ++ show t ++ wsGeneric t ++
  ">::const_iterator " ++ it ++ " = " ++ n ++ ".begin(); " ++ it ++ " != " ++
  n ++ ".end(); " ++ "++" ++ it ++ ") {\n" ++
  ws2 ++ ccType t ++ tmp ++ " = *" ++ it ++ ";\n" ++
  serializedSize (Field t tmp) ws2 ++ "\n" ++ ws ++ "}"
  where
    ws2 = ' ' : ' ' : ws
    it = n ++ "it"
    tmp = "tmp_" ++ n
    
-- | The code to find the size of a serialized map
serSizeMap :: CType -> CType -> String -> String -> String
serSizeMap (Prim k) (Prim v) n ws =
  ws ++ "__size += sizeof(unsigned int);\n" ++
  ws ++ "__size += " ++ n ++ ".size() * sizeof(" ++ k ++ ");\n" ++
  ws ++ "__size += " ++ n ++ ".size() * sizeof(" ++ v ++ ");"
serSizeMap k v n ws =
  ws ++ "__size += sizeof(unsigned int);\n" ++
  sizeT k ++ sizeT v ++
  ws ++ "for (std::map<" ++ show k ++ ", " ++ show v ++ wsGeneric v ++
  ">::const_iterator " ++ it ++ " = " ++ n ++ ".begin(); " ++
  it ++ " != " ++ n ++ ".end(); ++" ++ it ++ ") {\n" ++
  (if isPrim k then "" else
  ws2 ++ ccType k ++ tmpk ++ " = " ++ it ++ "->first;\n" ++
  serializedSize (Field k tmpk) ws2 ++ "\n") ++
  (if isPrim v then "" else
  ws2 ++ ccType v ++ tmpv ++ " = " ++ it ++ "->second;\n" ++
  serializedSize (Field v tmpv) ws2 ++ "\n") ++ ws ++"}"
  where
    ws2 = ' ' : ' ' : ws
    it = n ++ "it"
    tmpk = "tmpk_" ++ n
    tmpv = "tmpv_" ++ n
    sizeT (Prim t) = ws ++ "__size += " ++ n ++ ".size() * sizeof(" ++
                     t ++ ");\n"
    sizeT _ = ""

-- | Gets the name of the de/serialization method
serMethod :: Bool -> String -> String
serMethod d s = "b." ++ m ++ concat (map capitalize (words t))
  where
    t = if isPrefixOf "unsigned " s then drop 9 s else s
    m = if d then "Deserialize" else "Serialize"

-- | The code to serialize a pair
serPair :: CType -> CType -> String -> String -> String
serPair t1 t2 n ws =
  ws ++ ccType t1 ++ tmp1 ++ " = " ++ n ++ ".first;\n" ++
  ws ++ ccType t2 ++ tmp2 ++ " = " ++ n ++ ".second;\n" ++
  serialize (Field t1 tmp1) ws ++ "\n" ++
  serialize (Field t2 tmp2) ws
  where
    tmp1 = "tmp1_" ++ n
    tmp2 = "tmp2_" ++ n

-- | The code to serialize a vector or list
serList :: String -> CType -> String -> String -> String
serList c t n ws =
  ws ++ "b.SerializeInt(" ++ n ++ ".size());\n" ++
  ws ++ "for (std::" ++ c ++ "<" ++ show t ++ wsGeneric t ++
  ">::const_iterator " ++ it ++ " = " ++ n ++ ".begin(); " ++ it ++ " != " ++
  n ++ ".end(); ++" ++ it ++ ") {\n" ++ ws2 ++ ccType t ++ tmp ++ " = *" ++
  it ++ ";\n" ++ serialize (Field t tmp) ws2 ++ "\n" ++ ws ++ "}"
  where
    ws2 = ' ' : ' ' : ws
    it = n ++ "it"
    tmp = "tmp_" ++ n
  
-- | The code to serialize a map
serMap :: CType -> CType -> String -> String -> String
serMap k v n ws =
  ws ++ "b.SerializeInt(" ++ n ++ ".size());\n" ++
  ws ++ "for (std::map<" ++ show k ++ ", " ++ show v ++ wsGeneric v ++
  ">::const_iterator " ++ it ++ " = " ++ n ++ ".begin(); " ++
  it ++ " != " ++ n ++ ".end(); ++" ++ it ++ ") {\n" ++
  ws2 ++ ccType k ++ tmpk ++ " = " ++ it ++ "->first;\n" ++
  ws2 ++ ccType v ++ tmpv ++ " = " ++ it ++ "->second;\n" ++
  serialize (Field k tmpk) ws2 ++ "\n" ++
  serialize (Field v tmpv) ws2 ++ "\n" ++ ws ++"}"
  where
    ws2 = ' ' : ' ' : ws
    it = n ++ "it"
    tmpk = "tmpk_" ++ n
    tmpv = "tmpv_" ++ n
    
-- | The code to deserialize a pair
deserPair :: CType -> CType -> String -> String -> String
deserPair t1 t2 n ws =
  ws ++ show t1 ++ ' ' : tmp1 ++ ";\n" ++
  deserialize (Field t1 tmp1) ws ++ "\n" ++
  ws ++ show t2 ++ ' ' : tmp2 ++ ";\n" ++
  deserialize (Field t2 tmp2) ws ++ "\n" ++
  ws ++ n ++ ".first = " ++ tmp1 ++ ";\n" ++
  ws ++ n ++ ".second = " ++ tmp2 ++ ";"
  where
    tmp1 = "tmp1_" ++ n
    tmp2 = "tmp2_" ++ n
        
-- | The code to deserialize a vector or list
deserList :: CType -> String -> String -> String
deserList t n ws =
  ws ++ "unsigned int " ++ size ++ " = b.DeserializeInt();\n" ++
  ws ++ "for (unsigned int " ++ i ++ " = 0; " ++ i ++ " < " ++
  size ++ "; ++" ++ i ++ ") {\n" ++
  ws2 ++ show t ++ ' ' : tmp ++ ";\n" ++
  deserialize (Field t tmp) ws2 ++ "\n" ++
  ws2 ++ n ++ ".push_back(" ++ tmp ++ ");\n" ++ ws ++ "}"
  where
    ws2 = ' ' : ' ' : ws
    size = "size_" ++ n
    i = n ++ "i"
    tmp = "tmp_" ++ n
    
-- | The code to deserialize a map
deserMap :: CType -> CType -> String -> String -> String
deserMap k v n ws =
  ws ++ "unsigned int " ++ size ++ " = b.DeserializeInt();\n" ++
  ws ++ "for (unsigned int " ++ i ++ " = 0; " ++ i ++ " < " ++
  size ++ "; ++" ++ i ++ ") {\n" ++
  ws2 ++ show k ++ ' ' : tmpk ++ ";\n" ++
  ws2 ++ show v ++ ' ' : tmpv ++ ";\n" ++
  deserialize (Field k tmpk) ws2 ++ "\n" ++
  deserialize (Field v tmpv) ws2 ++ "\n" ++
  ws2 ++ n ++ '[' : tmpk ++ "] = " ++ tmpv ++ ";\n" ++ ws ++ "}"
  where
    ws2 = ' ' : ' ' : ws
    size = "size_" ++ n
    i = n ++ "i"
    tmpk = "tmpk_" ++ n
    tmpv = "tmpv_" ++ n

-- | The code to get the size of a serialized field
serializedSize :: Field -> String -> String
serializedSize (Field CString n) ws = ws ++ "__size += " ++ n ++ ".size() + 1;"
serializedSize (Field (User _) n) ws = ws ++ "__size += " ++ n ++
                                       ".SerializedSize();"
serializedSize (Field (Prim t) _) ws = ws ++ "__size += sizeof(" ++ t ++ ");"
serializedSize (Field (Pair t1 t2) n) ws = serSizePair t1 t2 n ws
serializedSize (Field (Vector t) n) ws = serSizeList "vector" t n ws
serializedSize (Field (List t) n) ws = serSizeList "list" t n ws
serializedSize (Field (Map k v) n) ws = serSizeMap k v n ws

-- | The code to serialize a field
serialize :: Field -> String -> String
serialize (Field CString n) ws = ws ++ serMethod False "string" ++
                                  '(' : n ++ ");"
serialize (Field (User _) n) ws = ws ++ n ++ ".Serialize(b);"
serialize (Field (Prim t) n) ws = ws ++ serMethod False t ++
                                   '(' : n ++ ");"
serialize (Field (Pair t1 t2) n) ws = serPair t1 t2 n ws
serialize (Field (Vector t) n) ws = serList "vector" t n ws
serialize (Field (List t) n) ws = serList "list" t n ws
serialize (Field (Map k v) n) ws = serMap k v n ws

-- | The code to deserialize a field
deserialize :: Field -> String -> String
deserialize (Field CString n) ws = ws ++ n ++ " = " ++
                                    serMethod True "string" ++ "();"
deserialize (Field (User _) n) ws = ws ++ n ++ ".Deserialize(b);"
deserialize (Field (Prim t) n) ws = ws ++ n ++ " = " ++
                                     serMethod True t ++ "();"
deserialize (Field (Pair t1 t2) n) ws = deserPair t1 t2 n ws
deserialize (Field (Vector t) n) ws = deserList t n ws
deserialize (Field (List t) n) ws = deserList t n ws
deserialize (Field (Map k v) n) ws = deserMap k v n ws

-- | The code to get the size of a serialized packet
writeSerializedSize :: Packet -> CppWriter
writeSerializedSize (Packet n fds) = do
  tellLn ("unsigned int " ++ n ++ "::SerializedSize() const {")
  tellLn "  unsigned int __size = Packet::SerializedSize();"
  forM_ fds (\fd -> tellLn (serializedSize fd "  "))
  tellLn "  return __size;"
  tellLn "}\n"

-- | The code to serialize a packet
writeSerialize :: Packet -> CppWriter
writeSerialize (Packet n fds) = do
  tellLn ("void " ++ n ++ "::Serialize(ByteBuffer &b) const {")
  tellLn "  Packet::Serialize(b);"
  forM_ fds (\fd -> tellLn (serialize fd "  "))
  tellLn "}\n"
  
-- | The code to deserialize a packet
writeDeserialize :: Packet -> CppWriter
writeDeserialize (Packet n fds) = do
  tellLn ("void " ++ n ++ "::Deserialize(ByteBuffer &b) {")
  tellLn "  Packet::Deserialize(b);"
  forM_ fds (\fd -> tellLn (deserialize fd "  "))
  tellLn "}\n"

-- | The code to write the .cpp file for a group of packets
writeCppFile :: String -> [Packet] -> Map String FilePath -> CppWriter
writeCppFile name ps incs = do
  tellLn $ autoComment (name ++ ".cpp")
  writeIncludes incs ps
  tellLn "#include \"bytebuffer.h\""
  tellLn "#include \"packet.h\""
  tellLn ("#include \"" ++ name ++ ".h\"\n")
  forM_ ps (\p -> do
               writeSerializedSize p
               writeSerialize p
               writeDeserialize p
           )
    
{---------- Main IO ----------}
  
-- | Prints usage instructions
printUsage :: IO ()
printUsage = do
  putStrLn "Nick's Packet Compiler"
  putStrLn "Auto-generates C++ code for packet classes given fields. There are" 
  putStrLn "two possible invocations:\n"
  putStrLn "npc [--inc=FILE] packet1 [packet2] ..."
  putStrLn "npc [--inc=FILE] --all\n"
  putStrLn "The first invocation will create .h and .cpp files for each packet" 
  putStrLn "file specified."
  putStrLn "The second invocation is equivalent to invoking 'npc' with"
  putStrLn "every .packet file in the current directory."
  putStrLn "The inc option is used to specify the file of inclusions for"
  putStrLn "user-defined classes."
  
data Flag =
    All
  | Includes (Maybe String)
  deriving (Eq, Show)
           
getInc' :: Flag -> Maybe String
getInc' All = Nothing
getInc' (Includes s) = s

getInc :: [Flag] -> String
getInc flgs = case mapMaybe getInc' flgs of
  []    -> ""
  (f:_) -> f
    
options :: [OptDescr Flag]
options = [
  Option ['a'] ["all"] (NoArg All) "compile all packets in directory",
  Option ['i'] ["inc"] (OptArg Includes "FILE") "mappings of classes to header files"
  ]
          
-- | Gets the files whose extension is .packet
getFiles :: IO [FilePath]
getFiles = do
  cd <- getCurrentDirectory
  files <- getDirectoryContents cd
  return (filter (\f -> takeExtension f == ".packet") files)

-- | Writes all the packet files
npc' :: [FilePath] -> [[Packet]] -> Map String FilePath -> IO ()
npc' packetFiles packets incs = do
  forM_ (zip packetFiles packets)
        (\(name,ps) -> do
            base <- return (dropExtension name)
            writeFile (base ++ ".h") $
              execWriter (writePacketHeader base ps incs)
            putStrLn ("Created " ++ base ++ ".h")
            writeFile (base ++ ".cpp") $ execWriter (writeCppFile base ps incs)
            putStrLn ("Created " ++ base ++ ".cpp")
        )

-- | Generates all the files
npc :: [FilePath] -> [Flag] -> IO ()
npc packetFiles flgs = do
  results <- sequence (map parsePackets packetFiles)
  case sequence results of
    Left err -> print err
    -- packets :: [[Packet]]
    Right packets -> do
      writeFile "packet.h" $ execWriter (writeMainHeader (concat packets))
      putStrLn "Created packet.h"
      writeFile "packet.cpp" $
        execWriter (writePacketImpl (map dropExtension packetFiles)
                    (concat packets))
      putStrLn "Created packet.cpp"
      -- Parse the inclusions file
      incFile <- return (getInc flgs)
      incs' <- if null incFile then (return (Right M.empty))
               else parseIncludes incFile
      case incs' of
        Left err -> print err
        Right incs -> npc' packetFiles packets incs
      

-- | Reads the input and generates the packet files
main :: IO ()
main = do
  argv <- getArgs
  when (null argv) (printUsage >> exitSuccess)
  case getOpt Permute options argv of
    (flgs,fs,[]) -> do
      packetFiles <- if elem All flgs then getFiles else return fs
      when (null packetFiles) (printUsage >> exitFailure)
      npc packetFiles flgs
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo info options))
  where info = "Usage: npc [OPTION...] files..."
      