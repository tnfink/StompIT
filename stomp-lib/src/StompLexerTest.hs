module StompLexerTest where

import StompLexer
import Test.HUnit

scanMessage :: String -> Either String [Token]
 scanMessage messageString =
   runAlex messageString  alexMonadScan
      

connectedTestMessage :: String
connectedTestMessage =
"\
\CONNECTED\n\
\version:1.1\n\
\session:someUniqueSessionId\n\
\server:serverName/1.0.0*Kommentar\n\               
\\n\
\\0"

testConnectedMessage :: Test
testConnectedMessage = TestCase 
 (assertEqual "testConnectedMessage"
              [Connected]
              (scanMessage testConnectedMessage)
 )

