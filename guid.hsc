{-# LANGUAGE ForeignFunctionInterface #-}

module Guid (GUID, emptyGuid, generateGuid, createGuid, createGuidPtr) where

import System.Win32.Types
import System.Win32.Mem (zeroMemory)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc (malloc)
import Foreign.Marshal.Array (peekArray, pokeArray)

#include "Rpc.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data GUID = GUID 
	{ 	dataOne 	:: !DWORD
	 ,	dataTwo 	:: !WORD
	 ,	dataThree 	:: !WORD
	 , 	dataFour 	:: [BYTE]
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
		pokeArray ptrOff d4
		return ()

emptyGuid :: IO (GUID)
emptyGuid = do
	ptr <- malloc::IO (Ptr GUID)
	let szt = (fromIntegral #size GUID)::DWORD
	zeroMemory ptr szt
	peek ptr

#include "Objbase.h"

foreign import stdcall unsafe "CoCreateGuid"
	c_CoCreateGuid :: Ptr GUID -> IO (CULong)

generateGuid :: IO (Maybe GUID) 
generateGuid = do
	ptr <- malloc::IO (Ptr GUID)
	r <- c_CoCreateGuid ptr
	let s_ok = 0
	if r == s_ok
		then peek ptr >>= (\g -> return (Just g))
		else return Nothing	

createGuid :: DWORD -> WORD -> WORD -> [BYTE] -> GUID
createGuid d1 d2 d3 d4 | length d4 > 8 = error "Not valid length of list. Must be less than or equals to 8"
					   | otherwise   = GUID d1 d2 d3 d4

createGuidPtr :: GUID -> IO (Ptr GUID)
createGuidPtr guid = do
	ptr <- malloc::IO (Ptr GUID)
	poke ptr guid >> return ptr		    