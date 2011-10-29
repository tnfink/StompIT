{-# LANGUAGE CPP,MagicHash #-}
{-# LINE 1 "src/StompLexer.x" #-}

module StompLexer where

import Data.Either

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

{-# LINE 18 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  String)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,c,[]) = Nothing
alexGetChar (p,_,(c:s))  = let p' = alexMove p c in p' `seq`
                                Just (c, (p', c, s))


{-# LINE 51 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad


data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_scd :: !Int        -- the current startcode



    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Alex a -> Either String a
runAlex input (Alex f) 
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input,       
                        alex_chr = '\n',



                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of 
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_inp=inp} -> 
        Right (s, (pos,c,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_inp=inp} of
                  s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError message = Alex $ \s -> Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError inp' -> alexError "lexical error"
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan
    AlexToken inp' len action -> do
        alexSetInput inp'
        action inp len

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip input len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code input len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
-- andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

-- token :: (String -> Int -> token) -> AlexAction token
token t input len = return (t input len)



-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)

{-# LINE 251 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper

{-# LINE 273 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version

{-# LINE 297 "templates/wrappers.hs" #-}

{-# LINE 322 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.

{-# LINE 339 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version

{-# LINE 354 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

alex_base :: AlexAddr
alex_base = AlexA# "\xc0\xff\xff\xff\x00\x00\x00\x00\xc2\xff\xff\xff\xb9\xff\xff\xff\xc3\xff\xff\xff\x00\x00\x00\x00\xc4\xff\xff\xff\xc8\xff\xff\xff\xc6\xff\xff\xff\xba\xff\xff\xff\xcd\xff\xff\xff\xb8\xff\xff\xff\xcf\xff\xff\xff\x00\x00\x00\x00\xd1\xff\xff\xff\xd2\xff\xff\xff\xd0\xff\xff\xff\xc9\xff\xff\xff\xd7\xff\xff\xff\xca\xff\xff\xff\xda\xff\xff\xff\xcb\xff\xff\xff\xcc\xff\xff\xff\xd3\xff\xff\xff\x00\x00\x00\x00\xd4\xff\xff\xff\xd5\xff\xff\xff\xdc\xff\xff\xff\xdf\xff\xff\xff\x00\x00\x00\x00\xd6\xff\xff\xff\xdd\xff\xff\xff\xd8\xff\xff\xff\xdb\xff\xff\xff\xde\xff\xff\xff\x00\x00\x00\x00\xe0\xff\xff\xff\xd9\xff\xff\xff\xe1\xff\xff\xff\xec\xff\xff\xff\x00\x00\x00\x00\xe6\xff\xff\xff\x00\x00\x00\x00\xe7\xff\xff\xff\xe4\xff\xff\xff\xeb\xff\xff\xff\x00\x00\x00\x00\xe2\xff\xff\xff\xf0\xff\xff\xff\xf2\xff\xff\xff\xea\xff\xff\xff\xed\xff\xff\xff\xee\xff\xff\xff\xf6\xff\xff\xff\xe9\xff\xff\xff\xf1\xff\xff\xff\xf9\xff\xff\xff\xef\xff\xff\xff\xfc\xff\xff\xff\xfb\xff\xff\xff\xf3\xff\xff\xff\x00\x00\x00\x00\xe5\xff\xff\xff\xf5\xff\xff\xff\xf7\xff\xff\xff\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\x02\x00\x00\x00\x06\x00\x00\x00\xf8\xff\xff\xff\xfa\xff\xff\xff\x03\x00\x00\x00\x00\x00\x00\x00\xfd\xff\xff\xff\xfe\xff\xff\xff\x07\x00\x00\x00\x05\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x08\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00"#

alex_table :: AlexAddr
alex_table = AlexA# "\x00\x00\x27\x00\x1c\x00\x22\x00\x37\x00\x55\x00\x01\x00\x02\x00\x03\x00\x05\x00\x06\x00\x0a\x00\x08\x00\x49\x00\x2d\x00\x07\x00\x09\x00\x0b\x00\x50\x00\x04\x00\x0e\x00\x17\x00\x0d\x00\x40\x00\x0c\x00\x0f\x00\x11\x00\x10\x00\x13\x00\x12\x00\x19\x00\x15\x00\x14\x00\x16\x00\x18\x00\x1a\x00\x1b\x00\x1f\x00\x1e\x00\x2b\x00\x20\x00\x3c\x00\x1d\x00\x24\x00\x2c\x00\x21\x00\x26\x00\x29\x00\x25\x00\x28\x00\x2a\x00\x2f\x00\x23\x00\x3d\x00\x2e\x00\x30\x00\x31\x00\x34\x00\x36\x00\x32\x00\x35\x00\x33\x00\x42\x00\x39\x00\x3a\x00\x3b\x00\x3e\x00\x38\x00\x43\x00\x41\x00\x3f\x00\x45\x00\x48\x00\x44\x00\x4d\x00\x46\x00\x4e\x00\x47\x00\x4b\x00\x4f\x00\x4c\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x51\x00\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x00\x54\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

alex_check :: AlexAddr
alex_check = AlexA# "\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x44\x00\x4e\x00\x45\x00\x45\x00\x42\x00\x53\x00\x52\x00\x4d\x00\x4e\x00\x49\x00\x43\x00\x42\x00\x52\x00\x53\x00\x42\x00\x55\x00\x45\x00\x54\x00\x55\x00\x49\x00\x43\x00\x52\x00\x42\x00\x53\x00\x49\x00\x53\x00\x55\x00\x4e\x00\x4e\x00\x47\x00\x45\x00\x4d\x00\x49\x00\x43\x00\x4d\x00\x4e\x00\x54\x00\x52\x00\x41\x00\x4f\x00\x42\x00\x43\x00\x4f\x00\x4b\x00\x4b\x00\x43\x00\x54\x00\x50\x00\x54\x00\x45\x00\x4e\x00\x43\x00\x49\x00\x4e\x00\x53\x00\x4f\x00\x45\x00\x43\x00\x45\x00\x4e\x00\x4d\x00\x54\x00\x45\x00\x44\x00\x4f\x00\x41\x00\x45\x00\x47\x00\x45\x00\x53\x00\x43\x00\x53\x00\x50\x00\x45\x00\x49\x00\x54\x00\xff\xff\xff\xff\xff\xff\xff\xff\x52\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x52\x00\x52\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_deflt :: AlexAddr
alex_deflt = AlexA# "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_accept = listArray (0::Int,85) [[],[(AlexAcc (alex_action_0))],[],[],[],[(AlexAcc (alex_action_1))],[],[],[],[],[],[],[],[(AlexAcc (alex_action_2))],[],[],[],[],[],[],[],[],[],[],[(AlexAcc (alex_action_3))],[],[],[],[],[(AlexAcc (alex_action_4))],[],[],[],[],[],[(AlexAcc (alex_action_5))],[],[],[],[],[(AlexAcc (alex_action_6))],[],[(AlexAcc (alex_action_7))],[],[],[],[(AlexAcc (alex_action_8))],[],[],[],[],[],[],[],[],[],[(AlexAcc (alex_action_9))],[],[],[],[],[(AlexAcc (alex_action_10))],[],[],[],[(AlexAcc (alex_action_11))],[],[(AlexAcc (alex_action_12))],[],[],[],[],[],[],[(AlexAcc (alex_action_13))],[],[],[],[],[],[],[(AlexAcc (alex_action_14))],[],[],[],[]]
{-# LINE 36 "src/StompLexer.x" #-}

-- Each right-hand side has type :: String -> Token

-- The token type:
data Token =
      Send        
    | Subscribe
    | Unsubscribe
    | Begin
    | Commit
    | Abort
    | Ack
    | Nack
    | Disconnect
    | Connect
    | Stomp
    | Connected
    | Message
    | Receipt
    | Error
    | HeaderName String
    | HeaderValue String
    | Body String
    | EndOfMessage
    deriving (Eq,Show)

alexEOF :: Alex Token   
alexEOF =  return EndOfMessage  
   
    
 
alex_action_0 =  token (\s l -> Send) 
alex_action_1 =  token (\s l -> Subscribe) 
alex_action_2 =  token (\s l -> Unsubscribe) 
alex_action_3 =  token (\s l -> Begin) 
alex_action_4 =  token (\s l -> Commit) 
alex_action_5 =  token (\s l -> Abort) 
alex_action_6 =  token (\s l -> Ack) 
alex_action_7 =  token (\s l -> Nack) 
alex_action_8 =  token (\s l -> Disconnect) 
alex_action_9 =  token (\s l -> Connect) 
alex_action_10 =  token (\s l -> Stomp) 
alex_action_11 =  token (\s l -> Connected) 
alex_action_12 =  token (\s l -> Message) 
alex_action_13 =  token (\s l -> Receipt) 
alex_action_14 =  token (\s l -> Error) 
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 37 "templates/GenericTemplate.hs" #-}

{-# LINE 47 "templates/GenericTemplate.hs" #-}


data AlexAddr = AlexA# Addr#

#if __GLASGOW_HASKELL__ < 503
uncheckedShiftL# = shiftL#
#endif

{-# INLINE alexIndexInt16OffAddr #-}
alexIndexInt16OffAddr (AlexA# arr) off =
#ifdef WORDS_BIGENDIAN
  narrow16Int# i
  where
        i    = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
        high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
        low  = int2Word# (ord# (indexCharOffAddr# arr off'))
        off' = off *# 2#
#else
  indexInt16OffAddr# arr off
#endif





{-# INLINE alexIndexInt32OffAddr #-}
alexIndexInt32OffAddr (AlexA# arr) off = 
#ifdef WORDS_BIGENDIAN
  narrow32Int# i
  where
   i    = word2Int# ((b3 `uncheckedShiftL#` 24#) `or#`
		     (b2 `uncheckedShiftL#` 16#) `or#`
		     (b1 `uncheckedShiftL#` 8#) `or#` b0)
   b3   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 3#)))
   b2   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 2#)))
   b1   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
   b0   = int2Word# (ord# (indexCharOffAddr# arr off'))
   off' = off *# 4#
#else
  indexInt32OffAddr# arr off
#endif





#if __GLASGOW_HASKELL__ < 503
quickIndex arr i = arr ! i
#else
-- GHC >= 503, unsafeAt is available from Data.Array.Base.
quickIndex = unsafeAt
#endif




-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (I# (sc))
  = alexScanUser undefined input (I# (sc))

alexScanUser user input (I# (sc))
  = case alex_scan_tkn user input 0# input sc AlexNone of
	(AlexNone, input') ->
		case alexGetChar input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input'

	(AlexLastSkip input'' len, _) ->



		AlexSkip input'' len

	(AlexLastAcc k input''' len, _) ->



		AlexToken input''' len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
	new_acc = check_accs (alex_accept `quickIndex` (I# (s)))
  in
  new_acc `seq`
  case alexGetChar input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		(base) = alexIndexInt32OffAddr alex_base s
		((I# (ord_c))) = ord c
		(offset) = (base +# ord_c)
		(check)  = alexIndexInt16OffAddr alex_check offset
		
		(new_s) = if (offset >=# 0#) && (check ==# ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
	case new_s of 
	    -1# -> (new_acc, input)
		-- on an error, we want to keep the input *before* the
		-- character that failed, not after.
    	    _ -> alex_scan_tkn user orig_input (len +# 1#) 
			new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (I# (len))
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (I# (len))
	check_accs (AlexAccPred a predx : rest)
	   | predx user orig_input (I# (len)) input
	   = AlexLastAcc a input (I# (len))
	check_accs (AlexAccSkipPred predx : rest)
	   | predx user orig_input (I# (len)) input
	   = AlexLastSkip input (I# (len))
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (I# (sc)) user _ _ input = 
     case alex_scan_tkn user input 0# input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (I# (i)) = i
