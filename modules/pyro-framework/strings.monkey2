Namespace pyro.framework.strings

#rem monkeydoc Creates and returns a string containing the character codes in the specified array.
#end
Function FromChars:String( chars:Int[] )

	Local out:=""

	For Local i:=0 Until chars.Length
		out+=String.FromChar( chars[i] )
	Next

	Return out

End

#rem monkeydoc Returns the string in alphabetical order.
#end
Function SortString:String( str:String,splitter:String="~n" )

	Local data:=str.Trim().Split( splitter )
	If data.Length<=1 Return str

	Local c:=0
	Local d:=""

	While c<data.Length

		For Local i:=0 Until data.Length-1

			If data[i]>data[i+1]
				d=data[i]
				data[i]=data[i+1]
				data[i+1]=d
			Endif

		Next
		
		c+=1
	
	Wend

	Local out:=""

	For Local i:=0 Until data.Length
		If out out+=splitter
		out+=data[i]
	Next

	Return out

End

#rem monkeydoc @hidden
#end
Function Split:String[]( str:String,splitter:String="," )

	Local mode:=0

	Local out:String[]

	For Local i:=0 Until str.Length

		If out=Null out=ResizeArray( out,out.Length+1 )

		If str.Mid( i,1 )=String.FromChar(34)
			mode=Not mode
			i+=1
		Endif

		If mode=0 And str.Mid( i,1 )=splitter
			out=ResizeArray( out,out.Length+1 )
			i+=1
		Endif

		out[out.Length-1]+=str.Mid( i,1 )

	Next
	
	Return out
	
End

#rem monkeydoc Converts string to an array of character codes.
#end
Function ToChars:Int[]( str:String )

	Local out:=New Int[ str.Length ]

	For Local i:=0 Until str.Length
		out[i]=str[i]
	Next

	Return out

End
