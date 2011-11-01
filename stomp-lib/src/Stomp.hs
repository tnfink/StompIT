-- Bekannte Fallen/Probleme/TODOs
-- * localhost wird bei IP-6 Unterstuetzung nicht aufgeloest, 
--   für Details mit Loesung s. http://www.mail-archive.com/haskell-cafe@haskell.org/msg76549.html
-- * "Quoting" von Sonderzeichen fehlt noch (s. "Value encoding")
-- * Es koennen mehrere Header mit gleichem Namen empfangen werden. Nur der erste ist zu uebergeben. 
-- * Fehlermeldung ist in dem header "message" 

-- Aktueller Testlauf
--   handle <- stompConnectToServer "127.0.0.1" "61613"
--   stompConnectToMessaging handle "localhost" "guest" "guest"
--   stompSendTextMessage handle "/queue/pingQueue" "Hola mundo!"


module Stomp where

-- import Data.Bits
import Network.Socket
-- import Network.BSD
import Data.Either
import Data.List
-- import Data.Maybe
import System.IO
import Test.HUnit

--
-- Interne Konstanten
-----------------------------------------------------------------

newline :: String
newline = "\n"

nullTermination :: String
nullTermination = "\0"

connectCommand :: String
connectCommand = "CONNECT"

connectedCommand :: String
connectedCommand = "CONNECTED"


--
-- Datentypen
-----------------------------------------------------------------

data StompHandle = 
  StompHandle { getHandle :: Handle } 
        

data StompFrame = 
    ConnectFrame FrameContent 
  | ConnectedFrame FrameContent
  | ErrorFrame FrameContent
  | SendFrame FrameContent
  deriving (Show, Eq)      

data FrameContent =
 FrameContent { getHeaders :: [(String,String)]
              , getBody    :: String
              }
 deriving (Show, Eq) 

-- 
-- create frames
-------------------------------------------------------------------------------

type FrameHeaders = [(String, String)]

--
-- initiate connection
-- 

createInitialConnect :: String -> StompFrame
createInitialConnect host = 
  ConnectFrame ( FrameContent 
                 [("accept-version", "1.1"), ("host",host)]
                 ""
               ) 
        
createInitialConnectWithLogin :: String -> String -> String -> StompFrame
createInitialConnectWithLogin host login passcode =
  let ConnectFrame ( FrameContent headers body) = 
        createInitialConnect host
  in  ConnectFrame ( FrameContent (("login", login) : ("passcode", passcode) : headers)
                                  body
                   )     

createSendMessageFrame :: String -> String -> StompFrame
createSendMessageFrame destination message =
  SendFrame ( FrameContent [("destination", destination), ("content-type","text/plain"), ("content-length", show $length message)]
                           message
            )

--
-- Encoding
-------------------------------------------------------------------------------

--
-- encoding of frames
--

testEncodeConnect :: Test
testEncodeConnect = TestCase
  (assertEqual "Connect to host"
               "\
\CONNECT\n\
\accept-version:1.1\n\
\host:testhost\n\               
\\n\
\\0"
               (encodeFrame (createInitialConnect "testhost"))
  )
  
encodeFrame :: StompFrame -> String


encodeFrame (ConnectFrame frameContent) =
  "CONNECT\n" ++ (encodeFrameContent frameContent)
  
encodeFrame (ConnectedFrame frameContent) =
  "CONNECTED\n" ++ (encodeFrameContent frameContent)

encodeFrame (SendFrame frameContent) =
  "SEND\n" ++ (encodeFrameContent frameContent)

encodeFrame (ErrorFrame frameContent) =
  "ERROR\n" ++ (encodeFrameContent frameContent)


--
-- Encoding of the frame content
-- 

testSimpleFrame :: Test
testSimpleFrame = TestCase
  (assertEqual "FrameContent  h1->v1 EinzeilenBody"
               ( "h1:v1" ++newline ++ newline 
                 ++ "EinzeilenBody" ++ nullTermination)
               (encodeFrameContent (FrameContent [("h1","v1")] "EinzeilenBody") )
  )  
               

encodeFrameContent :: FrameContent -> String
encodeFrameContent (FrameContent headers body) =
  encodeHeaders headers ++ newline           
  ++ body ++ nullTermination

--
-- Enoding of the headers
-- 

testSimpleHeader :: Test
testSimpleHeader = TestCase
  (assertEqual "encodeHeaders [(h1,v1),(h2,v2)]"
               ("h1:v1\nh2:v2\n")
               (encodeHeaders [("h1","v1"),("h2","v2")])
  )        

testEmptyHeader :: Test
testEmptyHeader = TestCase
  (assertEqual "encodeHeaders []"
               ""
               (encodeHeaders [])
  )        


encodeHeaders :: FrameHeaders -> String
encodeHeaders headers =
  foldl (\ string (name,value) -> string ++ name ++ ":" ++ value ++ newline )
        "" headers


--
-- Decoding
----------------------------------------------------------------------------

type ErrorMessage = String 
type FrameBody = String
type FrameCommand = String
type SafeStompFrame = Either ErrorMessage StompFrame

connectedTestMessage :: String
connectedTestMessage = "\
\CONNECTED\n\
\version:1.1\n\
\session:someUniqueSessionId\n\
\server:serverName/1.0.0*Kommentar\n\               
\\n\
\\0"

testDecodeConnectedMessage :: Test
testDecodeConnectedMessage = TestCase 
 (assertEqual "testDecodeConnectedMessage"
              (Right (ConnectedFrame (FrameContent [("version","1.1")
                                                    ,("session","someUniqueSessionId")
                                                    ,("server","serverName/1.0.0*Kommentar")]
                                                    ""
              )      )               )
              (decodeFrame connectedTestMessage)
 )

----------------------------------------------------------------------------

decodeFrame :: String -> (Either ErrorMessage StompFrame)
decodeFrame frameString =
  let (eitherCommand, contentString) = decodeFrameCommand frameString
      (eitherHeaders, bodyString)  = decodeFrameHeaders contentString
      eitherBody = decodeFrameBody bodyString
   in combineFrame eitherCommand eitherHeaders eitherBody


combineFrame :: (Either ErrorMessage FrameCommand) -> (Either ErrorMessage FrameHeaders) -> (Either ErrorMessage FrameBody) 
  -> (Either ErrorMessage StompFrame)

combineFrame (Left errorMessage) _ _ = Left errorMessage
combineFrame _ (Left errorMessage) _ = Left errorMessage
combineFrame _ _ (Left errorMessage) = Left errorMessage

combineFrame (Right command) (Right headers) (Right body) =
 let content = FrameContent headers body
 in 
    case command of
      "CONNECTED" -> Right $ConnectedFrame content
      "ERROR"     -> Right $ErrorFrame content
      _           -> Left "not yet implemented" 
    
    

----------------------------------------------------------------------------

testDecodeFrameCommand :: Test
testDecodeFrameCommand = TestCase
 (assertEqual "testDecodeFrameCommand"
               (Right "SomeCommand" , "remainder\nremainder\n\0")
               (decodeFrameCommand "SomeCommand\nremainder\nremainder\n\0")
 )


decodeFrameCommand :: String -> (Either ErrorMessage FrameCommand, String)
decodeFrameCommand frameString = 
  let maybeIndex = elemIndex '\n' frameString
  in ( case maybeIndex of
         Just index -> let (command, remainderWithNL) = splitAt index frameString
                        in (Right command, tail remainderWithNL)
         Nothing    -> (Left "No command found on the message", "")
     )

----------------------------------------------------------------------------

testDecodeFrameHeaders :: Test
testDecodeFrameHeaders = TestCase
 (assertEqual "testDecodeFrameHeaders"
              (Right [("h1","v1"),("h2","v2")], "remainder")
              (decodeFrameHeaders "h1:v1\nh2:v2\n\nremainder")
  )           

decodeFrameHeaders :: String -> (Either ErrorMessage FrameHeaders, String)
decodeFrameHeaders contentString =
  let  headersString :: String
       remainder :: String
       (headersString, remainder) = splitHeadersFromBody "" contentString
   in if (null remainder)
      then (Left "The headers are not terminated.", "")
      else let headerLines = lines headersString
               splitHeaders :: [Either ErrorMessage (String,String)]
               splitHeaders = map splitHeaderLine headerLines
               (errorMessages,headers) = partitionEithers splitHeaders
            in if (null errorMessages)
               then (Right (foldl (\ hs h -> hs ++ [h]) [] headers), remainder)
               else (Left  (foldl (\ es e -> es ++ "\n" ++ e) "" errorMessages), "")
               


testSplitHeadersFromBody :: Test
testSplitHeadersFromBody = TestCase
 $assertEqual "testSplitHeadersFromBody"
              ("h1:v1\nh2:v2\n","remainder")
              (splitHeadersFromBody "" "h1:v1\nh2:v2\n\nremainder")  

splitHeadersFromBody :: String -> String -> (String,String)
splitHeadersFromBody headers ('\n' : '\n' : remainder ) = (headers ++ "\n", remainder) 
splitHeadersFromBody headers (x : xs) = splitHeadersFromBody (headers ++ [x]) xs 
splitHeadersFromBody headers [] = (headers, [])

testSplitHeaderLine :: Test
testSplitHeaderLine = TestCase
 $assertEqual "testSplitHeaderLine"
              (Right ("h1","v1"))
              (splitHeaderLine "h1:v1")

splitHeaderLine :: String -> Either ErrorMessage (String,String)
splitHeaderLine headerLine =
  let maybeIndex = elemIndex ':' headerLine
   in case maybeIndex of
       Nothing    -> Left ("No colon found in header line: "++headerLine)
       Just index -> let (name, valueWithColon) = splitAt index headerLine
                         nameEmpty = null name
                         valueEmpty = (length valueWithColon) <= 1 
                      in if nameEmpty
                         then Left ("No name in header: " ++ headerLine)
                         else if valueEmpty
                         then Left ("No value in header: " ++ headerLine)
                         else Right (name, tail valueWithColon) 

----------------------------------------------------------------------------

decodeFrameBody :: String -> Either ErrorMessage FrameBody 
decodeFrameBody bodyString =
  if (null bodyString )
     || ( (last bodyString) /= '\0')
  then Left "The body is missing the \\0 termination."
  else Right (init bodyString) 



-- 
-- Basic Communication
----------------------------------------------------------------------------

-- stConnectToServer
stompConnectToServer :: HostName -> String -> IO StompHandle
stompConnectToServer hostname port =
        do -- Look up the hostname and port.  Either raises an exception
           -- or returns a nonempty list.  First element in that list
           -- is supposed to be the best option.
           addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
           let serveraddr = head addrinfos
           
           -- Establish a socket for communication
           sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        
           -- Mark the socket for keep-alive handling since it may be idle
           -- for long periods of time
           setSocketOption sock KeepAlive 1
        
           -- Connect to server
           connect sock (addrAddress serveraddr)
        
           -- Make a Handle out of it for convenience
           h <- socketToHandle sock ReadWriteMode
        
           -- We're going to set buffering to BlockBuffering and then
           -- explicitly call hFlush after each message, below, so that
           -- messages get logged immediately
           hSetBuffering h (BlockBuffering Nothing)
               
           -- Save off the socket, program name, and server address in a handle
           return $ StompHandle h 
      
               
-- send a message
stompSendFrame :: StompHandle -> StompFrame -> IO ()        
stompSendFrame  stompHandle stompFrame =
        do hPutStrLn networkHandle encodedMessage
        
           -- Make sure that we send data immediately
           hFlush networkHandle
        where
           networkHandle = getHandle stompHandle 
           encodedMessage = encodeFrame stompFrame
           
           
-- 
-- receives a message and decode it
--            
stompReceiveFrame :: StompHandle -> IO (Either ErrorMessage StompFrame)
stompReceiveFrame stompHandle =
        do 
           messageData <- readUntilNull networkHandle
           return $decodeFrame messageData
        where
           networkHandle = getHandle stompHandle           
           
printNextFrameFromServer :: StompHandle -> IO ()
printNextFrameFromServer stompHandle =
        do 
           printUntilNull networkHandle
        where
           networkHandle = getHandle stompHandle
           
printUntilNull :: Handle -> IO ()
printUntilNull handle =
        do
           isEndOfFile <- hIsEOF handle
           if isEndOfFile
             then return ()
             else do char <- hGetChar handle
                     putChar char
                     if (char /= '\0') then printUntilNull handle
                                       else putStr "\nNULL" 
readUntilNull :: Handle -> IO String
readUntilNull handle =
  do
    char <- hGetChar handle
    if (char == '\0' ) 
      then return "\0"
      else do remainder <- readUntilNull handle
              return (char : remainder)
    
   
   
-- 
-- STOMP interaction protocol
----------------------------------------------------------------------------
           
           
-- connect to Stomp
----------------------------------------------------------------------------

stompConnectToMessaging :: StompHandle -> String -> String -> String -> IO (Maybe ErrorMessage)
stompConnectToMessaging handle host login passcode =
  do 
    stompSendFrame handle connectMessage
    answer <- stompReceiveFrame handle
    case answer of
      Left errorMessage -> 
        return (Just errorMessage)
      Right (ConnectedFrame _) -> 
        return Nothing
      Right (ErrorFrame content) ->
        return (Just (getBody content))
      _ ->
        (return (Just "Unknown frame received after sending the CONNECTED frame."))   
  where 
    connectMessage = createInitialConnectWithLogin host login passcode
   
     
-- send text message
----------------------------------------------------------------------------
     
stompSendTextMessage :: StompHandle -> String -> String -> IO (Maybe ErrorMessage)
stompSendTextMessage handle destination message =
  do
    stompSendFrame handle sendFrame
    return Nothing
    -- what now, only in case of an error a message is sent 
    -- better to use a state chart for it;
    -- no! => server is sending a receipt
  where
    sendFrame = createSendMessageFrame destination message
     
           
---------------------------------------------------------------------------------------           
           
-- alle Tests
tests :: Test
tests = TestList [ testSimpleHeader, testEmptyHeader, testSimpleFrame, testEncodeConnect, testDecodeConnectedMessage
                 , testDecodeFrameCommand, testDecodeFrameHeaders, testSplitHeadersFromBody, testSplitHeaderLine]         

-- Main-Funktion, um alle Tests auszuführen
-- Das scheint da etwas besseres in Cabal zu geben, schaut aber nach Work-In-Progress aus. Mal auf die nächste Plattform-Version warten...

main :: IO Counts  
main = runTestTT tests

