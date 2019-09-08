Namespace pyro.framework.bitmapfont

#rem monkeydoc @hidden
#end
Class BitmapFont

	#rem monkeydoc The height.
	#end
	Field Height:=0.0
	Field Space:=0

	Method GetAdvance:Int( index:Int )
		Return _advance[index]
	End Method

	Method GetChar:Image( index:Int )
		Return _char[index]
	End Method

	Method GetOffsetX:int( index:Int )
		Return _offsetX[index]
	End Method

	Method GetOffsetY:int( index:Int )
		Return _offsetY[index]
	End Method

	#rem monkeydoc Draws the text.
	#end
	Method Draw( canvas:Canvas,text:String,location:Vec2f,scale:Vec2f=New Vec2f( 1,1 ),handle:Vec2f=New Vec2f( .5,.5 ) )
		Draw( canvas,text,location.X,location.Y,scale.X,scale.Y,handle.X,handle.Y )
	End
	
	#rem monkeydoc Draws the text.
	#end
	Method Draw( canvas:Canvas,text:String,x:Float=0,y:Float=0,scaleX:Float=1,scaleY:Float=1,handleX:Float=.5,handleY:Float=.5 )

		x-=GetWidth( text,scaleX )*handleX
		y-=GetHeight( text,scaleY )*handleY

		For Local i:=0 Until text.Length

			If _char[ text[i] ]
				canvas.DrawImage( _char[ text[i] ],x+_offsetX[ text[i] ]*scaleX,y+_offsetY[ text[i] ]*scaleY,0,scaleX,scaleY )
			Endif

			x+=( _advance[ text[i] ]+Space )*scaleX

		Next

	End

	#rem monkeydoc Returns the text height.
	#end
	Method GetHeight:Float( text:String,scale:Float=1 )
		Return Height*scale
	End

	#rem monkeydoc Returns the text width.
	#end
	Method GetWidth:Float( text:String,scale:Float=1 )

		Local width:=0
		For Local i:=0 Until text.Length
			width+=( _advance[text[i]]+Space )
		Next

		Return width*scale

	End

	#rem monkeydoc Loads char from path.
	#end
	Method LoadChar( path:String,char:Int )

		_char[char]=Image.Load( SmartPath( path ) )

		_advance[char]=_char[char].Width

		If Height<_char[char].Height Height=_char[char].Height

	End

	#rem monkeydoc Set char from image.
	#end
	Method SetChar( image:Image,char:Int )

		_char[char]=image

		_advance[char]=_char[char].Width

		If Height<_char[char].Height Height=_char[char].Height

	End

	Private

	Field _advance:=New Int[256]
	Field _char:=New Image[256]
	Field _offsetX:=New Int[256]
	Field _offsetY:=New Int[256]

End