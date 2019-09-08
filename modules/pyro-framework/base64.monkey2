' Copyright (c) 2011 Steve Revill and Shane Woolcock.
' Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
' The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Namespace pyro.framework.base64

Using pyro.framework.strings

#rem monkeydoc The Base64 class.

Pyro Base64 Encoder / Decoder.

Based on the original version created by Steve Revill and Shane Woolcock.

Provides Base64 encoding/decoding functions.

Base64 is a useful method for converting non-printable characters and other binary data to printable text.

Due to the way base64 works, the encoded string will generally be approximately 33% larger.
#end
Class Base64

	#rem monkeydoc Converts an array of Ints into a base64 encoded string.
	
	Enabling lineWrap will wrap the string every lineWrapWidth characters ( defaults to 80 ).
	
	Enabling padOutput will pad the ending of the output with equals characters ( = ) to next 4-byte boundary.
	
	#end
	Function Encode:String( src:Int[],padOutput:Bool=False,lineWrap:Bool=False,lineWrapWidth:Int=80 )

		If src.Length=0 Return ""

		Local buffer:=New DataBuffer( Int( src.Length*4.0/3.0+10 ) )
		Local addr:=0
		Local s1:=0,s2:=0,s3:=0,a:=0,b:=0,c:=0,d:=0,i:=0
		Local charsAdded:=0
	
		Repeat
			' Get 3 source bytes:
			s1=src[i]
			If i+1<src.Length s2=src[i+1] Else s2=0
			If i+2<src.Length s3=src[i+2] Else s3=0
	
			' Make 4 target bytes:
			a=s1 Shr 2
			b=( ( s1&3 ) Shl 4 ) | ( s2 Shr 4 )
			c=( ( s2&15 ) Shl 2 ) | ( s3 Shr 6 )
			d=s3&63
	
			' Set target bytes 3 and 4 if the source is not divisible by 3:
			If i+1>=src.Length c=64
			If i+2>=src.Length d=64
	
			' Append target bytes,adding a line wrap if we must:
			If lineWrap And lineWrapWidth>0 And charsAdded Mod lineWrapWidth=0 And charsAdded>1
				buffer.PokeByte( addr,"~n"[0] ) ; addr+=1
			Endif

			buffer.PokeByte( addr,_characters[a] ) ; addr+=1 ; charsAdded+=1

			If lineWrap And lineWrapWidth>0 And charsAdded Mod lineWrapWidth=0
				buffer.PokeByte( addr,"~n"[0] ) ; addr+=1
			Endif

			buffer.PokeByte( addr,_characters[b] ) ; addr+=1 ; charsAdded+=1

			If c<64 Or padOutput
				If lineWrap And lineWrapWidth>0 And charsAdded Mod lineWrapWidth=0
					buffer.PokeByte( addr,"~n"[0] ) ; addr+=1
				Endif
				buffer.PokeByte( addr,_characters[c] ) ; addr+=1 ; charsAdded+=1
			Endif

			If d<64 Or padOutput
				If lineWrap And lineWrapWidth>0 And charsAdded Mod lineWrapWidth=0
					buffer.PokeByte( addr,"~n"[0] ) ; addr+=1
				End
				buffer.PokeByte( addr,_characters[d] ) ; addr+=1 ; charsAdded+=1
			Endif
	
			' Next 3 bytes!
			i+=3
		Until i>=src.Length

		Return _PeekString( buffer,0,addr )

	End
	
	#rem monkeydoc Converts a ( usually readable ) String into a base64 encoded string.
	
	Enabling lineWrap will wrap the string every lineWrapWidth characters ( defaults to 80 ).
	
	Enabling padOutput will pad the ending of the output with equals characters ( = ) to next 4-byte boundary.
	
	#end
	Function Encode:String( src:String,padOutput:Bool=False,lineWrap:Bool=False,lineWrapWidth:Int=80 )
		Return Encode( ToChars( src ),padOutput,lineWrap,lineWrapWidth )
	End
	
	#rem monkeydoc Converts a base64 encoded string into another ( usually readable ) string.
	#end
	Function Decode:String( src:String )
		Return FromChars( DecodeBytes( src ) )
	End
	
	#rem monkeydoc Converts a base64 encoded string into an array of integers.
	#end
	Function DecodeBytes:Int[]( src:String )

		_Init()

		Local a:=0,b:=0,c:=0,d:=0,i:=0,j:=0
		Local src2:=New Int[src.Length]
		Local padding:=0

		' find out how many base64 characters:
		Local srclen:=0

		For i=0 Until src.Length
			If _base64[src[i]]>=0
				src2[srclen]=src[i]
				srclen+=1
				' check if it's a padding character and increment the count:
				If _base64[src[i]]=64 padding+=1
			Endif
		Next
		
		' die if there are no base64 chars:
		If srclen=0 Return Null
	
		' get the target length and create the array:
		Local len:=3*( srclen/4 )

		If srclen Mod 4=0
			len-=padding
		Else If padding=0
			If srclen Mod 4>=2 len+=1
			If srclen Mod 4=3 len+=1
		Endif

		Local rv:=New Int[len]

		i=0
		j=0
		
		Repeat
			a=_base64[src2[i]]
			If i+1>srclen Exit										' This shouldn't happen with base64,so something's wrong!
			b=_base64[src2[i+1]]
			If i+2<srclen c=_base64[src2[i+2]] Else c=64
			If i+3<srclen d=_base64[src2[i+3]] Else d=64
			rv[j]=( a Shl 2 ) | ( b Shr 4 )
			If j+1<len rv[j+1]=( ( b&15 ) Shl 4 ) | ( c Shr 2 )
			If j+2<len rv[j+2]=( ( c&3 ) Shl 6 ) | d
			i+=4
			j+=3
		Until i>=srclen
		
		Return rv

	End

	Private
	
	Function _Init()
	
		If _base64.Length=0
	
			_base64=New Int[256]
	
			For Local i:=0 Until _base64.Length
				_base64[i]=-1
			Next
	
			For Local i:=0 Until _characters.Length
				_base64[_characters[i]]=i
			Next
	
		Endif
	
	End
	
	'Deprecated!
	Function _PeekBytes:Int[]( dataBuffer:DataBuffer,address:Int,count:Int=$1fffffff )
		If address+count>dataBuffer.Length count=dataBuffer.Length-address
		Local bytes:=New Int[count]
		_PeekBytes( dataBuffer,address,bytes,0,count )
		Return bytes
	End
		
	Function _PeekBytes( dataBuffer:DataBuffer,address:Int,bytes:Int[],offset:Int=0,count:Int=$1fffffff )
		If address+count>dataBuffer.Length count=dataBuffer.Length-address
		If offset+count>bytes.Length count=bytes.Length-offset
		For Local i:=0 Until count
			bytes[offset+i]=dataBuffer.PeekByte( address+i )
		Next
	End
	
	Function _PeekString:String( dataBuffer:DataBuffer,address:Int,encoding:String="utf8" )
		Return _PeekString( dataBuffer,address,dataBuffer.Length-address,encoding )
	End
	
	Function _PeekString:String( dataBuffer:DataBuffer,address:Int,count:Int,encoding:String="utf8" )
	
		Select encoding
		Case "utf8"
			Local p:=_PeekBytes( dataBuffer,address,count )
			Local i:=0,e:=p.Length,err:=False
			Local q:=New Int[e],j:=0
			While i<e
				Local c:=p[i] & $ff
				i+=1
				If c & $80
					If (c & $e0)=$c0
						If i>=e Or (p[i] & $c0)<>$80
							err=True
							Exit
						Endif
						c=(c & $1f) Shl 6 | (p[i] & $3f)
						i+=1
					Else If (c & $f0)=$e0
						If i+1>=e Or (p[i] & $c0)<>$80 Or (p[i+1] & $c0)<>$80
							err=True
							Exit
						Endif
						c=(c & $0f) Shl 12 | (p[i] & $3f) Shl 6 | (p[i+1] & $3f)
						i+=2
					Else
						err=True
						Exit
					Endif
				Endif
				q[j]=c
				j+=1
			Wend
			If err
				'UTF8 encoding error! 
				Return FromChars( p )
			Endif
			If j<e q=q.Slice( 0,j )
			Return FromChars( q )
		Case "ascii"
			Local p:=_PeekBytes( dataBuffer,address,count )
			For Local i:=0 Until p.Length
				p[i]&=$ff
			Next
			Return FromChars( p )
		End
		
	''	Error( "Invalid string encoding:"+encoding )
		Print "Invalid string encoding:"+encoding
		
		Return ""
		
	End
	
	Const _characters:="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

	Global _base64:Int[]

End

