{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Writer
import Data.Char
import Data.List
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

-- | Pairwise ors 3-tuples
or3 :: (Bool, Bool, Bool) -> (Bool, Bool, Bool) -> (Bool, Bool, Bool)
or3 (a1,a2,a3) (b1,b2,b3) = (a1 || b1, a2 || b2, a3 || b3)

-- | Sees whether a type has any (maps, strings, vectors)
has :: CType -> (Bool, Bool, Bool)
has CString = (False, True, False)
has (Vector t) = or3 (False, False, True) (has t)
has (Map k v) = or3 (True, False, False) (or3 (has k) (has v))
has _ = (False, False, False)
    
-- | Sees whether a group of packets needs to include (map, string, vector)
includes :: [Packet] -> (Bool, Bool, Bool)
includes = foldl (\inc (Packet _ fs) -> or3 inc $
                   (foldl (\inc2 (Field t _) ->
                            or3 inc2 (has t)) (False, False, False) fs))
           (False, False, False)

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

-- | Writes a field's getter, optionally making it const
getter :: Field -> Bool -> CppWriter
getter (Field t n) const = do
  tell "  "
  tell (ccType t)
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
  tell "  " >> makeCtor n fds
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
writePacketHeader :: String -> [Packet] -> CppWriter
writePacketHeader hdr ps = do
  tellLn $ autoComment (hdr ++ ".h")
  tellLn ("#ifndef " ++ guard)
  tellLn ("#define " ++ guard)
  lnBreak
  when incMap (tellLn "#include <map>")
  when incStr (tellLn "#include <string>")
  when incVec (tellLn "#include <vector>")
  tellLn "#include \"bytebuffer.h\""
  tellLn "#include \"packet.h\"\n"
  mapM_ writeClass ps
  tell ("#endif // " ++ guard)
  where
    guard = "__" ++ constantCase hdr ++ "_HEADER__"
    (incMap, incStr, incVec) = includes ps

-- | The constant Packet class
packetClass :: [String]
packetClass = [
  "class Packet {",
  "protected:",
  "  PacketType type;\n",
  "public:",
  "  Packet(PacketType t) : type(t) { }",
  "  PacketType GetPacketType() const { return type; }",
  "  virtual unsigned int SerializedSize() const { return sizeof(PacketType); }",
  "  virtual void Serialize(ByteBuffer &b) const { b.SerializeInt(static_cast<int>(type)); }",
  "  virtual void Deserialize(ByteBuffer &b) { type = static_cast<PacketType>(b.DeserializeInt()); }",
  "};\n"
  ]
              
-- | Writes the main header file, including the Packet class and enum of
--   packet types
writeMainHeader :: [Packet] -> CppWriter
writeMainHeader ps = do
  tellLn (autoComment "packet.h")
  tellLn "#ifndef __PACKET_HEADER__"
  tellLn "#define __PACKET_HEADER__\n"
  tellLn "enum PacketType {"
  forM_ ps (\p -> do
               tell "  "
               tell $ constantCase (packetName p)
               tellLn ",")
  tellLn "};\n"
  mapM_ tellLn packetClass
  tell "#endif // __PACKET_HEADER__"
  
{---------- Serialization Methods (.cpp) ----------}
  
{-
-- | The code to find the size of a serialized vector
serSizeVector' :: CType -> String -> String -> CppWriter
serSizeVector' (Prim t) n ws = do
  tell ws
  tellLn "__size += sizeof(unsigned int);"
  tell ws
  tell $ "__size += " ++ n ++ ".size() * sizeof(" ++ t ++ ");"
serSizeVector' t n ws = do
  tell ws
  tellLn "__size += sizeof(unsigned int);"
  tell ws
  tell "for (std::vector<"
-}
      
-- | The code to find the size of a serialized vector
serSizeVector :: CType -> String -> String -> String
serSizeVector (Prim t) n ws =
  ws ++ "__size += sizeof(unsigned int);\n" ++
  ws ++ "__size += " ++ n ++ ".size() * sizeof(" ++ t ++ ");"
serSizeVector t n ws =
  ws ++ "__size += sizeof(unsigned int);\n" ++
  ws ++ "for (std::vector<" ++ show t ++ wsTemp t ++ ">::const_iterator " ++
  it ++ " = " ++ n ++ ".begin(); " ++ it ++ " != " ++ n ++ ".end(); " ++
  "++" ++ it ++ ") {\n" ++
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
  ws ++ "for (std::map<" ++ show k ++ ", " ++ show v ++ wsTemp v ++
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

-- | The code to serialize a vector
serVector :: CType -> String -> String -> String
serVector t n ws =
  ws ++ "b.SerializeInt(" ++ n ++ ".size());\n" ++
  ws ++ "for (std::vector<" ++ show t ++ wsTemp t ++ ">::const_iterator " ++
  it ++ " = " ++ n ++ ".begin(); " ++ it ++ " != " ++ n ++
  ".end(); ++" ++ it ++ ") {\n" ++
  ws2 ++ ccType t ++ tmp ++ " = *" ++ it ++ ";\n" ++
  serialize (Field t tmp) ws2 ++ "\n" ++ ws ++ "}"
  where
    ws2 = ' ' : ' ' : ws
    it = n ++ "it"
    tmp = "tmp_" ++ n
  
-- | The code to serialize a map
serMap :: CType -> CType -> String -> String -> String
serMap k v n ws =
  ws ++ "b.SerializeInt(" ++ n ++ ".size());\n" ++
  ws ++ "for (std::map<" ++ show k ++ ", " ++ show v ++ wsTemp v ++
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
        
-- | The code to deserialize a vector
deserVector :: CType -> String -> String -> String
deserVector t n ws =
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
serializedSize (Field (Vector t) n) ws = serSizeVector t n ws
serializedSize (Field (Map k v) n) ws = serSizeMap k v n ws

-- | The code to serialize a field
serialize :: Field -> String -> String
serialize (Field CString n) ws = ws ++ serMethod False "string" ++
                                  '(' : n ++ ");"
serialize (Field (User _) n) ws = ws ++ n ++ ".Serialize(b);"
serialize (Field (Prim t) n) ws = ws ++ serMethod False t ++
                                   '(' : n ++ ");"
serialize (Field (Vector t) n) ws = serVector t n ws
serialize (Field (Map k v) n) ws = serMap k v n ws

-- | The code to deserialize a field
deserialize :: Field -> String -> String
deserialize (Field CString n) ws = ws ++ n ++ " = " ++
                                    serMethod True "string" ++ "();"
deserialize (Field (User _) n) ws = ws ++ n ++ ".Deserialize(b);"
deserialize (Field (Prim t) n) ws = ws ++ n ++ " = " ++
                                     serMethod True t ++ "();"
deserialize (Field (Vector t) n) ws = deserVector t n ws
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
writeCppFile :: String -> [Packet] -> CppWriter
writeCppFile name ps = do
  tellLn $ autoComment (name ++ ".cpp")
  when incMap (tellLn "#include <map>")
  when incStr (tellLn "#include <string>")
  when incVec (tellLn "#include <vector>")
  tellLn "#include \"bytebuffer.h\""
  tellLn "#include \"packet.h\""
  tellLn ("#include \"" ++ name ++ ".h\"\n")
  forM_ ps (\p -> do
               writeSerializedSize p
               writeSerialize p
               writeDeserialize p
           )
  where (incMap, incStr, incVec) = includes ps
    
{---------- Main IO ----------}
    
-- | Gets the files whose extension is .packet
getFiles :: IO [FilePath]
getFiles = do
  cd <- getCurrentDirectory
  files <- getDirectoryContents cd
  return (filter (\f -> takeExtension f == ".packet") files)
  
-- | Prints usage instructions
printUsage :: IO ()
printUsage = do
  putStrLn "Nick's Packet Compiler"
  putStrLn "Auto-generates C++ code for packet classes given fields. There are" 
  putStrLn "two possible invocations:\n"
  putStrLn "npc [--main] [packet1] [packet2] ..."
  putStrLn "npc --all\n"
  putStrLn "The first invocation will create .h and .cpp files for each packet" 
  putStrLn "file specified. Providing the --main flag will also create or"
  putStrLn "refresh packet.h with the updated enum of packet types provided.\n"
  putStrLn "The second invocation is equivalent to invoking 'npc --main' with"
  putStrLn "every .packet file in the current directory."
  
-- | Processes the flags
processArgs :: [String] -> IO (Maybe (Bool, [FilePath]))
processArgs args
  | elem "--all" args = do
    when (length args /= 1) (printUsage >> exitFailure)
    return Nothing
  | otherwise = return (Just (elem "--main" args, delete "--main" args))

-- | Generates all the file
npc :: [FilePath] -> Bool -> IO ()
npc packetFiles mainH = do
  results <- sequence (map parsePackets packetFiles)
  case sequence results of
    Left err -> print err
    -- packets :: [[Packet]]
    Right packets -> do
      when mainH $ do
        writeFile "packet.h" $ execWriter (writeMainHeader (concat packets))
        putStrLn "Created packet.h"
      forM_ (zip packetFiles packets)
        (\(name,ps) -> do
            base <- return (dropExtension name)
            writeFile (base ++ ".h") $ execWriter (writePacketHeader base ps)
            putStrLn ("Created " ++ base ++ ".h")
            writeFile (base ++ ".cpp") $ execWriter (writeCppFile base ps)
            putStrLn ("Created " ++ base ++ ".cpp")
        )

-- | Reads the input and generates the packet files
main :: IO ()
main = do
  args <- getArgs
  when (null args) (printUsage >> exitSuccess)
  argRes <- processArgs args
  case argRes of
    Nothing -> do
      packetFiles <- getFiles
      npc packetFiles True
    Just (mainH, packetFiles) -> npc packetFiles mainH
      