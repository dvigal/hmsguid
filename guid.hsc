{-# LANGUAGE ForeignFunctionInterface #-}

module Guid (GUID, guid) where

import Control.Monad (forM_)

import System.Win32.Types
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc (malloc)
import Foreign.Marshal.Array (peekArray)

#include "Rpc.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data GUID = GUID 
	{ 	dataOne 	:: !DWORD
	 ,	dataTwo 	:: !WORD
	 ,	dataThree 	:: !WORD
	 , 	dataFour 	:: ![BYTE]
	} deriving (Show)

instance Storable GUID where
	alignment _ = #{alignment GUID}
	sizeOf _ = (#size GUID)
	peek ptr = do
		d1  <- (#peek GUID, Data1) ptr
		d2  <- (#peek GUID, Data2) ptr
		d3  <- (#peek GUID, Data3) ptr
		let off = (#offset GUID, Data4)
		d4 <- peekArray 8 (ptr `plusPtr` off)
		return GUID { dataOne = d1, dataTwo = d2, dataThree = d3, dataFour = d4 }
	poke ptr (GUID d1 d2 d3 d4) = do
		(#poke GUID, Data1) ptr d1 
		(#poke GUID, Data2) ptr d2
		(#poke GUID, Data3) ptr d3
		let ptrOff = ptr `plusPtr` (#offset GUID, Data4)
		forM_ (zip [0..7] d4) (\(idx,val) -> pokeElemOff ptrOff idx val)

#include "Objbase.h"

foreign import stdcall unsafe "CoCreateGuid"
	c_CoCreateGuid :: Ptr GUID -> IO (CULong)

guid :: IO (Maybe GUID)
guid = do
	ptr <- malloc::IO (Ptr GUID)
	r <- c_CoCreateGuid ptr
	let s_ok = 0
	if r == s_ok
		then peek ptr >>= (\g -> return (Just g))
		else return Nothing	


