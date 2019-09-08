Namespace pyro.framework.drawdata

Using pyro.framework.contentmanager

#rem monkeydoc The DrawSet class.
#end
Class DrawSet

	Field Location:=New Vec2f()

	Property X:Float()
		Return Location.x
	Setter( x:Float )
		Location.x=x
	End

	Property Y:Float()
		Return Location.y
	Setter( y:Float )
		Location.y=y
	End

	Method InitDraw( canvas:Canvas )
		canvas.Translate( Location.x,Location.y )
	End

End

#rem monkeydoc The TextSet class.
#end
Class TextSet Extends DrawSet

	Field Font:=NullFont()
	Field Text:=""

	Method New()
	End

	Property Height:Float()
		If _height<>0 Return _height
''		If Text="" Return 0
		If Font=Null Return 0
		Return Font.Height
	Setter( height:Float )
		_height=height
	End

	Property Width:Float()
		If _width<>0 Return _width
		If Text="" Return 0
		If Font=Null Return 0
		Return Font.TextWidth( Text )
	Setter( width:Float )
		_width=width
	End

	Method DrawData:TextData( renderState:Int,frame:Int=0 )

		_drawData=ExpandArray( _drawData,renderState+1,frame+1 )

		If _drawData[renderState,frame]=Null _drawData[renderState,frame]=New TextData

		Return _drawData[renderState,frame]

	End

	Private

	Field _drawData:=New TextData[1,1]
	Field _height:=0.0
	Field _width:=0.0

End

#rem monkeydoc The ImageSet class.
#end
Class ImageSet Extends DrawSet

	Field PatchData:Int[]

	Method New()
	End

	Property Frames:int()
		Return _drawData.GetSize( 1 )
	End

	Property Height:Float()
		If _height<>0 Return _height
		Local height:=0.0
		For Local renderState:=0 Until _drawData.GetSize( 0 )
			For Local frame:=0 Until _drawData.GetSize( 1 )
				If _drawData[renderState,frame]=Null Continue
				If _drawData[renderState,frame].Image=Null Continue
				If _drawData[renderState,frame].Image.Height>height height=_drawData[renderState,frame].Image.Height''*_drawData[renderState,frame]._scale.Y
			Next
		Next
		Return height
	Setter ( height:Float )
		_height=height
	End

	Property Width:Float()
		If _width<>0 Return _width
		Local width:=0.0
		For Local renderState:=0 Until _drawData.GetSize( 0 )
			For Local frame:=0 Until _drawData.GetSize( 1 )
				If _drawData[renderState,frame]=Null Continue
				If _drawData[renderState,frame].Image=Null Continue
				If _drawData[renderState,frame].Image.Width>width width=_drawData[renderState,frame].Image.Width''*_drawData[renderState,frame]._scale.X
			Next
		Next
		Return width
	Setter ( width:Float )
		_width=width
	End

	Method DrawData:ImageData( renderState:Int,frame:Int=0 )

		_drawData=ExpandArray( _drawData,renderState+1,frame+1 )

		If _drawData[renderState,frame]=Null _drawData[renderState,frame]=New ImageData

		Return _drawData[renderState,frame]

	End

	Private

	Field _drawData:=New ImageData[1,1]
	Field _height:=0.0
	Field _width:=0.0

End

#rem monkeydoc The DrawData class.
#end
Class DrawData

	Field BlendMode:=mojo.graphics.BlendMode.Alpha
	Field Color:=New Color( 1,1,1,1 )
	Field Location:=New Vec2f()
	Field Scale:=New Vec2f( 1,1 )

	Property Alpha:Float()
		Return Color.a
	Setter( a:Float )
		Color.a=a
	End

	Property ScaleX:Float()
		Return Scale.x
	Setter( scaleX:Float )
		Scale.x=scaleX
	End

	Property ScaleY:Float()
		Return Scale.y
	Setter( scaleY:Float )
		Scale.y=scaleY
	End

	Property X:Float()
		Return Location.x
	Setter( x:Float )
		Location.x=x
	End

	Property Y:Float()
		Return Location.y
	Setter( y:Float )
		Location.y=y
	End

	Method InitDraw( canvas:Canvas,x:Float,y:Float,width:Float,height:Float,rotation:Float=0,handleX:Float=.5,handleY:Float=.5,brightness:Color=New Color( 1,1,1,1) )

		canvas.Translate( x,y )

		canvas.Translate( Location.x,Location.y )

		canvas.Scale( Scale.x,Scale.y )

		canvas.Translate( -width*handleX,-height*handleY )

		canvas.Translate( width*handleX,height*handleY )
''		canvas.Rotate( rotation*RotationMode )
		canvas.Rotate( rotation )
		canvas.Translate( -width*handleX,-height*handleY )

		canvas.BlendMode=BlendMode
		canvas.Color=Color*brightness

	End

End

#rem monkeydoc The TextData class.
#end
Class TextData Extends DrawData
End

#rem monkeydoc The ImageData class.
#end
Class ImageData Extends DrawData

	Field Image:Image

End