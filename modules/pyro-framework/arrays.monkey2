Namespace pyro.framework.arrays

#rem monkeydoc @hidden
#end
Function ExpandArray<T>:T[,]( arr:T[,],w:Int,h:Int )

	If w<=arr.GetSize( 0 ) And h<=arr.GetSize( 1 ) Return arr

	Local maxw:=Max( arr.GetSize( 0 ),w )
	Local maxh:=Max( arr.GetSize( 1 ),h )

	Local minw:=Min( arr.GetSize( 0 ),maxw )
	Local minh:=Min( arr.GetSize( 1 ),maxh )

	Local out:=New T[maxw,maxh]

	For Local x:=0 Until minw
		For Local y:=0 Until minh
			out[x,y]=arr[x,y]
		Next
	Next

	Return out

End

#rem monkeydoc Loads array data from path.

The data file must contain comma delimited data.
#end
Function LoadIntArray:Int[]( path:String )

	Local str:=LoadString( path )

	If str="" Return Null
	
	Return ToInt( str.Split( "," ) )
	
End

#rem monkeydoc @hidden
#end
Function ResizeArray<T>:T[]( arr:T[],len:Int )
	If arr.Length=len Return arr
	Local out:=New T[len]
	arr.CopyTo( out,0,0,Min( len,arr.Length ) )
	Return out
End

#rem monkeydoc @hidden
#end
Function ResizeArray<T>:T[,]( arr:T[,],w:Int,h:Int )

	If arr.GetSize( 0 )=w And arr.GetSize( 1 )=h Return arr

	Local out:=New T[w,h]

	Local minw:=Min( arr.GetSize( 0 ),w )
	Local minh:=Min( arr.GetSize( 1 ),h )

	For Local x:=0 Until minw
		For Local y:=0 Until minh
			out[x,y]=arr[x,y]
		Next
	Next

	Return out

End

Function ShuffleArray:String[]( data:String[] )

	Local out:=New String[data.Length]

	For Local i:=0 Until data.Length

		Local p:=Int( Rnd( data.Length ) )

		While out[p]<>""
			p=Int( Rnd( data.Length ) )
		Wend

		out[p]=data[i]

	Next

	Return out

End

#rem monkeydoc Splits the string in 2, using the separator string to delimit each piece.
#end
Function SplitInTwo:String[]( data:String,separator:String )

	Local d:=New String[]( "","" )

	Local p:=data.Find( separator )
	If p=-1 Return d

	d[0]=data.Left( p )
	If d[0]="" Return d

	Local l:=data.Length-d[0].Length-1
	If l<=0 Return d

	d[1]=data.Mid( p+1,l )

	Return d

End

#rem monkeydoc Converts a string array to int array.
#end
Function ToInt:Int[]( data:String[] )

	Local out:=New Int[data.Length]

	For Local i:=0 Until data.Length
		out[i]=Int( data[i] )
	Next

	Return out

End

#rem monkeydoc Converts a string array to float array.
#end
Function ToFloat:Float[]( data:String[] )

	Local out:=New Float[data.Length]

	For Local i:=0 Until data.Length
		out[i]=Float( data[i] )
	Next

	Return out

End

Function ToString:String( data:Int[] )
	Local out:=""
	For Local i:=0 Until data.Length
		If out<>"" out+=","
		out+=data[i]
	Next
	Return out
End

Function ToString:String( data:Float[] )
	Local out:=""
	For Local i:=0 Until data.Length
		If out<>"" out+=","
		out+=data[i]
	Next
	Return out
End