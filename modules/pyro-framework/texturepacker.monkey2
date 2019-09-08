Namespace pyro.framework.texturepacker

#rem monkeydoc The TexturePacker class.
#end
Class TexturePacker

	Function GrabImage:Image( sourceImage:Image,data:String,path:String )
	
		For Local l:=Eachin data.Split( "~n" )
			If l.StartsWith( path )
				Local i:=l.Split( ":" )
				If i.Length=5 Return New Image( sourceImage,New Recti( Int( i[1] ),Int( i[2] ),Int( i[1] )+Int( i[3] ),Int( i[2] )+Int( i[4] ) ) )
			Endif
		Next
		Return Null
	
	End

	Function GrabImages:Image[]( sourceImage:Image,data:String,image:Image[]=Null )

		For Local l:=Eachin data.Split( "~n" )
			Local i:=l.Split( ":" )
			If i.Length=5
				Local index:=image.Length
				image=ResizeArray( image,index+1 )
				image[index]=New Image( sourceImage,New Recti( Int( i[1] ),Int( i[2] ),Int( i[1] )+Int( i[3] ),Int( i[2] )+Int( i[4] ) ) )
			Endif
		Next
		Return image

	End
	
End