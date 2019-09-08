Namespace pyro.framework.gfx

Using pyro.framework.math

#rem monkeydoc @hidden
#end
Class Cursor

	Property CursorPosition:Int()
		Return _cursorPosition
	Setter( cursorPosition:Int )
		_cursorPosition=cursorPosition
	End

	Property CursorWidth:Int()
		Return _cursorWidth
	Setter( cursorWidth:Int )
		_cursorWidth=cursorWidth
	End

	Property Selection:Int[]()
		If _selection[1]>_selection[2] Return New Int[]( _selection[2],_selection[1] )
		Return New Int[]( _selection[1],_selection[2] )
	End

	Property TextSelected:Bool()
		Return _selection[0]
	End

	Method DeselectText()
		_selection[0]=0
	End

	Method DrawSelection( canvas:Canvas,text:String,x:Float,y:Float )

		If TextSelected=False Return

		Local resetX:=x

		For Local i:=0 Until text.Length
	
			Local char:=String.FromChar( text[i] )
			Select char
				Case _lineBreak
					x=resetX
					y+=canvas.Font.Height
				Default
				If i>=Selection[0] And i<Selection[1]
					canvas.DrawRect( x,y,canvas.Font.TextWidth( char ),canvas.Font.Height )
				Endif
				x+=canvas.Font.TextWidth( char )
			End Select

		Next
	
	End

	Method DrawText( canvas:Canvas,text:String,x:Float,y:Float,showCursor:Bool=True,textColor:Color=New Color( 1,0,0,1 ),cursorColor:Color=New Color( 1,1,0,1 ) )

		Local cursorX:=0

		Local cursorWidth:=_cursorWidth
		
		If cursorWidth<1 cursorWidth=canvas.Font.TextWidth( String.FromChar( text[_cursorPosition] ) )

		If cursorWidth<1 cursorWidth=canvas.Font.TextWidth( " " )

		For Local i:=0 Until _cursorPosition
			cursorX+=canvas.Font.TextWidth( String.FromChar( text[i] ) )
		Next

		If _showCursor=True And showCursor=True
			canvas.Color=cursorColor
			canvas.DrawRect( x+cursorX,y,cursorWidth,canvas.Font.Height )
		Endif

		canvas.Color=textColor
		canvas.DrawText( text,x,y )

		If Millisecs()>_cursorTime
			_cursorTime=Millisecs()+500
			_showCursor=Not _showCursor
		Endif

	End

	method DrawText2( canvas:Canvas,text:String,x:Float,y:Float,textColor:Color=New Color( 1,1,0,1 ),cursorColor:Color=New Color( 1,1,1,1 ),selectionColor:Color=New Color( 1,0,0,1 ) )

		canvas.Color=selectionColor
		DrawSelection( canvas,text,x,y )
		
		Local cursor:=GetCursorLocation( canvas.Font,text )
		canvas.Color=cursorColor
		canvas.DrawRect( x+cursor.x,y+cursor.y,_cursorWidth,canvas.Font.Height )
	
		canvas.Color=textColor
		DrawTextField( canvas,text,x,y )

	End

	Method DrawTextField( canvas:Canvas,text:String,x:Float,y:Float )

		Local resetX:=x

		For Local i:=0 Until text.Length
	
			Local char:=String.FromChar( text[i] )
			Select char
				Case _lineBreak
					x=resetX
					y+=canvas.Font.Height
				Default
				canvas.DrawText( char,x,y )
				x+=canvas.Font.TextWidth( char )
			End Select

		Next
	
	End

	Method GetCursorLocation:Vec2i( font:Font,text:String )

		Local x:=0
		Local y:=0

		For Local i:=0 Until _cursorPosition

			x+=font.TextWidth( String.FromChar( text[i] ) )

			Local char:=String.FromChar( text[i] )
			Select char
				Case _lineBreak
					x=0
					y+=font.Height
			End Select

		Next
		
		Return New Vec2i( x,y )

	End

	Method KeyEvent:String( event:KeyEvent,text:String )

		If ( ( event.Type=EventType.KeyChar ) And _AllowedChars( event ) )

			text=text.Slice( 0,_cursorPosition )+event.Text+text.Slice( _cursorPosition )

			_cursorPosition+=1

			Return text

		Endif

		Return SpecialKeys( event,text )

	End

	Method Left:String( str:String,length:Int )
	
''		If length>str.Length length=str.Length
	
		Return str.Slice( 0,length )
	
	End	

	Method SetCursorLocation( font:Font,text:String,x:Int,y:Int )

		Local h:=0
		Local v:=0
		Local cursorPosition:=0

		Local lines:=text.Split( _lineBreak )

		For v=0 Until ( y/font.Height )-1
			cursorPosition+=lines[v].Length+1
		Next

		Local line:=lines[v]
		For Local c:=0 Until line.Length
			h+=font.TextWidth( String.FromChar( line[c] ) )
			If h>x Exit
			cursorPosition+=1
		Next

		_cursorPosition=cursorPosition

	End

	Method SpecialKeys:String( event:KeyEvent,text:String )

		Local lineCursorPosition:=_cursorPosition-Left( text,_cursorPosition ).FindLast( _lineBreak ) - 1

		Local lineStart:=Left( text,_cursorPosition ).FindLast( _lineBreak ) + 1
		Local lineEnd:=text.Find( _lineBreak,lineStart )
		Local lineLength:=lineEnd-lineStart

		Local lineAboveStart:=Left( text,lineStart - 1 ).FindLast( _lineBreak ) + 1
		Local lineAboveLength:=lineStart - 1 - lineAboveStart
		Local lineAboveEnd:=lineAboveStart+lineAboveLength

		Local lineBelowStart:=text.Find( _lineBreak,_cursorPosition ) + 1
		Local lineBelowEnd:=text.Find( _lineBreak,lineBelowStart )
		Local lineBelowLength:=lineBelowEnd-lineBelowStart

''		Print lineAboveLength
''		Print lineCursorPosition
''		Print lineStart
''		Print lineAboveStart
''		Print lineLength
''		Print lineEnd
''		Print lineCursorPosition
''		Print lineBelowStart
''		Print lineBelowDif
''		Print lineBelowStart
''		Print _cursorPosition

		If TextSelected=True And Not event.Modifiers=Modifier.LeftShift And Not event.Type=EventType.KeyDown And ( event.Key=Key.Left Or event.Key=Key.Right Or event.Key=Key.Up Or event.Key=Key.Down )
			DeselectText()
		Endif

		If event.Type=EventType.KeyDown And event.Key=Key.F1
			_selection=New Int[]( 1,0,1 )
		Endif

		If ( event.Type=EventType.KeyDown Or event.Type=EventType.KeyRepeat ) And event.Key=Key.Left

			If event.Modifiers=Modifier.LeftShift And TextSelected=False
				_selection[0]=1
				_selection[1]=_cursorPosition
			Endif

			_cursorPosition-=1
			If _cursorPosition<0 _cursorPosition=0

			If event.Modifiers=Modifier.LeftShift _selection[2]=_cursorPosition

		Endif

		If ( event.Type=EventType.KeyDown Or event.Type=EventType.KeyRepeat ) And event.Key=Key.Right

			If event.Modifiers=Modifier.LeftShift And TextSelected=False
				_selection[0]=1
				_selection[1]=_cursorPosition
			Endif

			_cursorPosition+=1
			If _cursorPosition>text.Length _cursorPosition=text.Length

			If event.Modifiers=Modifier.LeftShift _selection[2]=_cursorPosition

		Endif

		If ( event.Type=EventType.KeyDown Or event.Type=EventType.KeyRepeat ) And event.Key=Key.Backspace

			If _cursorPosition>0	'And text.Length > 0

				text=text.Slice( 0,_cursorPosition-1 )+text.Slice( _cursorPosition )
				_cursorPosition-=1

			Endif

		Endif
		
		If ( event.Type=EventType.KeyDown Or event.Type=EventType.KeyRepeat ) And event.Key=Key.Home
			_cursorPosition=lineStart
		Endif
		
		If ( event.Type=EventType.KeyDown Or event.Type=EventType.KeyRepeat ) And event.Key=Key.KeyEnd
			_cursorPosition=lineEnd
		Endif
		
		If ( event.Type=EventType.KeyDown Or event.Type=EventType.KeyRepeat ) And event.Key=Key.KeyDelete
			text=text.Slice( 0,_cursorPosition )+text.Slice( _cursorPosition+1 )
		Endif

		If _lineBreak="" Return text

		If ( event.Type=EventType.KeyDown Or event.Type=EventType.KeyRepeat ) And event.Key=Key.Enter
			text=text.Slice( 0,_cursorPosition )+_lineBreak+text.Slice( _cursorPosition )
			_cursorPosition+=1
		Endif

		If ( event.Type=EventType.KeyDown Or event.Type=EventType.KeyRepeat ) And event.Key=Key.Up

			If event.Modifiers=Modifier.LeftShift And TextSelected=False
				_selection[0]=1
				_selection[1]=_cursorPosition
			Endif

			If lineCursorPosition>lineAboveLength
				_cursorPosition=lineAboveEnd
			Else
				_cursorPosition=lineAboveStart+lineCursorPosition
			Endif

			If event.Modifiers=Modifier.LeftShift _selection[2]=_cursorPosition

		Endif

		If ( event.Type=EventType.KeyDown Or event.Type=EventType.KeyRepeat ) And event.Key=Key.Down

			If event.Modifiers=Modifier.LeftShift And TextSelected=False
				_selection[0]=1
				_selection[1]=_cursorPosition
			Endif

			If lineCursorPosition>lineBelowLength
				_cursorPosition=lineBelowEnd
			Else
				_cursorPosition=lineBelowStart+lineCursorPosition
			Endif

			If event.Modifiers=Modifier.LeftShift _selection[2]=_cursorPosition

		Endif

		Return text

	End

	Private

	Method _AllowedChars:Bool( event:KeyEvent )
		If event.Key>=32 And event.Key<=126 Return True
		Return False
	End

	Method _SwapSelection()
		If TextSelected=False Return
		If _selection[1]<=_selection[2] Return
		Local s:=_selection[1]
		_selection[1]=_selection[2]
		_selection[2]=s
	End

	Field _cursorTime:=0
	Field _cursorWidth:=1
	Field _lineBreak:=""'"~n"
	Field _showCursor:=True
	Field _cursorPosition:=0
	Field _selection:=New Int[3]

End

#rem monkeydoc @hidden
#end
Function CreateImageMask:Image( pixmap:Pixmap,color:Color=Color.White,textureFlags:TextureFlags=TextureFlags.FilterMipmap )
	
	For Local x:=0 Until pixmap.Width
		For Local y:=0 Until pixmap.Height
			Local pixel:UInt=pixmap.GetPixelARGB( x,y )
			If pixel&$ff000000 pixmap.SetPixel( x,y,color )
		Next
	Next
	
	Return New Image( pixmap,textureFlags )

End

#rem monkeydoc @hidden
#end
Function CreateImageMask:Image[]( pixmap:Pixmap[],color:Color=Color.White,textureFlags:TextureFlags=TextureFlags.FilterMipmap )
	Local image:=New Image[pixmap.Length]
	For Local i:=0 Until pixmap.Length
		image[i]=CreateImageMask( pixmap[i],color,textureFlags )
	Next
	Return image
End

#rem monkeydoc @hidden
#end
Function CreatePatchData:Int[]( image:Image,patchData:Int )
	Return New Int[]( 0,0,patchData,patchData,patchData,0,image.Width-patchData*2,patchData,image.Width-patchData,0,patchData,patchData,0,patchData,patchData,image.Height-patchData*2,patchData,patchData,image.Width-patchData*2,image.Height-patchData*2,image.Width-patchData,patchData,patchData,image.Height-patchData*2,0,image.Height-patchData,patchData,patchData,patchData,image.Height-patchData,image.Width-patchData*2,patchData,image.Width-patchData,image.Height-patchData,patchData,patchData )
End

#rem monkeydoc @hidden
#end
Function CreateTiledImage:Image( tile:Image,width:Int,height:Int )

	Local image:=New Image( width,height )

	Local canvas:=New Canvas( image )

	Local tileWidth:=Int( tile.Width )
	Local tileHeight:=Int( tile.Height )

	canvas.Translate( tileWidth*tile.Handle.x,tileHeight*tile.Handle.y )

	For Local h:=0 Until width/tileWidth
		For Local v:=0 Until height/tileHeight
			canvas.DrawImage( tile,h*tileWidth,v*tileHeight )
		Next
	Next
	
	canvas.Flush()
	
	Return image

End

#rem monkeydoc @hidden
#end
Function Draw9Patch( canvas:Canvas,image:Image,patchData:Int[],x:Float,y:Float,width:Float,height:Float,rotation:Float=0,scaleX:Float=1,scaleY:Float=1,handleX:Float=.5,handleY:Float=.5 )

	If image=Null Return

	If patchData.Length=1 patchData=CreatePatchData( image,patchData[0] )

	If patchData.Length=36

		Local handle:=image.Handle
	
		image.Handle=New Vec2f( handleX,handleY )
	
		Local w:Int=width-( patchData[0*4+2]+patchData[1*4+2]+patchData[2*4+2] )
		Local h:Int=height-( patchData[0*4+3]+patchData[3*4+3]+patchData[6*4+3] )
	
		Local x1:Float=-width*image.Handle.X
		Local y1:Float=-height*image.Handle.Y
	
		Local x2:Int=x1+patchData[0*4+2]
		Local y2:Int=y1
	
		Local x3:Int=x2+( patchData[1*4+2]+w )
		Local y3:Int=y2
	
		Local x4:Int=x1
		Local y4:Int=y1+patchData[0*4+3]
	
		Local x5:Int=x4+patchData[3*4+2]
		Local y5:Int=y4
	
		Local x6:Int=x5+( patchData[4*4+2]+w )
		Local y6:Int=y5
	
		Local x7:Int=x4
		Local y7:Int=y4+( patchData[3*4+3]+h )
	
		Local x8:Int=x7+patchData[6*4+2]
		Local y8:Int=y7
	
		Local x9:Int=x8+( patchData[7*4+2]+w )
		Local y9:Int=y8
	
		canvas.PushMatrix()
	
		canvas.Translate( x,y )
		canvas.Rotate( rotation )
		canvas.Scale( scaleX,scaleY )
	
		canvas.DrawRect( x1,y1,patchData[0*4+2],patchData[0*4+3],image,patchData[0*4+0],patchData[0*4+1],patchData[0*4+2],patchData[0*4+3] )
		canvas.DrawRect( x2,y2,patchData[1*4+2]+w,patchData[1*4+3],image,patchData[1*4+0],patchData[1*4+1],patchData[1*4+2],patchData[1*4+3] )
		canvas.DrawRect( x3,y3,patchData[2*4+2],patchData[2*4+3],image,patchData[2*4+0],patchData[2*4+1],patchData[2*4+2],patchData[2*4+3] )
	
		canvas.DrawRect( x4,y4,patchData[3*4+2],patchData[3*4+3]+h,image,patchData[3*4+0],patchData[3*4+1],patchData[3*4+2],patchData[3*4+3] )
		canvas.DrawRect( x5,y5,patchData[4*4+2]+w,patchData[4*4+3]+h,image,patchData[4*4+0],patchData[4*4+1],patchData[4*4+2],patchData[4*4+3] )
		canvas.DrawRect( x6,y6,patchData[5*4+2],patchData[5*4+3]+h,image,patchData[5*4+0],patchData[5*4+1],patchData[5*4+2],patchData[5*4+3] )
	
		canvas.DrawRect( x7,y7,patchData[6*4+2],patchData[6*4+3],image,patchData[6*4+0],patchData[6*4+1],patchData[6*4+2],patchData[6*4+3] )
		canvas.DrawRect( x8,y8,patchData[7*4+2]+w,patchData[7*4+3],image,patchData[7*4+0],patchData[7*4+1],patchData[7*4+2],patchData[7*4+3] )
		canvas.DrawRect( x9,y9,patchData[8*4+2],patchData[8*4+3],image,patchData[8*4+0],patchData[8*4+1],patchData[8*4+2],patchData[8*4+3] )
	
		canvas.PopMatrix()
	
		image.Handle=handle

	Else

		DrawImage( canvas,image,x,y,rotation,scaleX,scaleY,handleX,handleY )

	Endif

End

#rem monkeydoc @hidden
#end
Function Draw9Patch( canvas:Canvas,image:Image,patchData:Int,x:Float,y:Float,width:Float,height:Float,rotation:Float=0,scaleX:Float=1,scaleY:Float=1,handleX:Float=.5,handleY:Float=.5 )
	Draw9Patch( canvas,image,CreatePatchData( image,patchData ),x,y,width,height,rotation,scaleX,scaleY,handleX,handleY )
End

#rem monkeydoc Draws a non filled ellipse.
#end
Function DrawEllipse( canvas:Canvas,x:Float,y:Float,width:Float,height:Float )

	Local Rx:=0,Ry:=0
	Local p:=0,px:=0,py:=0,xi:=0,yi:=0
	Local Rx2:=0,Ry2:=0,twoRx2:=0,twoRy2:=0
	Local pFloat:=0.0
	
	Rx=width
	Ry=height
	
	Rx2=Rx*Rx
	Ry2=Ry*Ry
	twoRx2=Rx2 Shl 1
	twoRy2=Ry2 Shl 1
	
	xi=0
	yi=Ry
	canvas.DrawPoint( x+xi,y+yi )
	canvas.DrawPoint( x-xi,y+yi )
	canvas.DrawPoint( x+xi,y-yi )
	canvas.DrawPoint( x-xi,y-yi )
	pFloat=( Ry2-( Rx2*Ry ) )+( 0.25*Rx2 )
	p=Int( pFloat )
	If pFloat Mod 1.0>=0.5 Then p+=1
	px=0
	py=twoRx2*yi
	While px<py
		xi+=1
		px+=twoRy2
		If p>=0
			yi-=1
			py-=twoRx2
		Endif
		If p<0 Then p+=Ry2+px Else p+=Ry2+px-py
		canvas.DrawPoint( x+xi,y+yi )
		canvas.DrawPoint( x-xi,y+yi )
		canvas.DrawPoint( x+xi,y-yi )
		canvas.DrawPoint( x-xi,y-yi )
	Wend

	pFloat=( Ry2*( xi+0.5 )*( xi+0.5 ) )+( Rx2*( yi-1.0 )*( yi-1.0 ) )-( Rx2*( Float( Ry2 ) ) )
	p=Int( pFloat )
	If pFloat Mod 1.0>=0.5 Then p+=1
	While yi>0
		yi-=1
		py-=twoRx2
		If p<=0
			xi+=1
			px+=twoRy2
		Endif
		If p>0 Then p+=Rx2-py Else p+=Rx2-py+px
		canvas.DrawPoint( x+xi,y+yi )
		canvas.DrawPoint( x-xi,y+yi )
		canvas.DrawPoint( x+xi,y-yi )
		canvas.DrawPoint( x-xi,y-yi )
	Wend

End Function

#rem monkeydoc Canvas fader.
#end
Function DrawFader( canvas:Canvas,fade:Float,fadeColor:Color=Color.Black )

		If fade<1

			fade=1.0-fade

			If fade<0 fade=0
			If fade>1 fade=1

			canvas.BlendMode=BlendMode.Alpha
			canvas.Color=New Color( fadeColor.R,fadeColor.G,fadeColor.B,fade )
			canvas.DrawRect( 0,0,canvas.Viewport.Width,canvas.Viewport.Height )

		Endif

End

#rem monkeydoc Draws a simple horizontal progressbar.
#end
Function DrawHorizontalProgressBar( canvas:Canvas,x:Int,y:Int,width:Int,height:Int,value:Int,maximum:Int,surfaceColor:Color=New Color( 0,.5,0,1 ),color:Color=New Color( 0,1,0,1 ),handleX:Float=.5,handleY:Float=.5 )

	Local s:=Float( maximum )/Float( width )

	canvas.Color=surfaceColor

	canvas.PushMatrix()

	canvas.Translate( -width*handleX,-height*handleY )

	canvas.DrawRect( x,y,width,height )

	If value>0
		canvas.Color=color
		If value<width*s
			canvas.DrawRect( x,y,value/s,height )
		Else
			canvas.DrawRect( x,y,width,height )
		Endif
	Endif

	canvas.PopMatrix()

End

#rem monkeydoc @hidden
#end
Function DrawImage( canvas:Canvas,image:Image,x:Float,y:Float,rotation:Float=0,scaleX:Float=1,scaleY:Float=1,handleX:Float=.5,handleY:Float=.5,padding:Bool=False )

	If padding=True

		canvas.PushMatrix()
		canvas.Translate( x,y )
		canvas.Rotate( rotation )
		canvas.Scale( scaleX,scaleY )
		canvas.Translate( -handleX*( image.Width-2 ),-handleY*( image.Height-2 ) )
''		canvas.DrawRect( 0,0,image,1,1,image.Width-2,image.Height-2 )
		canvas.DrawRect( 0,0,image.Width,image.Height,image,1,1,image.Width-2,image.Height-2 )	' Not tested and needs better performance!
		canvas.PopMatrix()

	Else

		If image.Handle.x=handleX And image.Handle.y=handleY

			canvas.DrawImage( image,x,y,rotation,scaleX,scaleY )
	
		Else

			Local handle:=image.Handle
	
			image.Handle=New Vec2f( 0,0 )
	
			canvas.PushMatrix()
	
			canvas.Translate( x,y )
			canvas.Rotate( rotation )
			canvas.Scale( scaleX,scaleY )
			canvas.Translate( -handleX*image.Width,-handleY*image.Height )
	
			canvas.DrawImage( image,0,0 )
	
			canvas.PopMatrix()

			image.Handle=handle
			
		Endif

	Endif

End

#rem monkeydoc @hidden
#end
Function DrawLine( canvas:Canvas,image:Image,x1:Float,y1:Float,x2:Float,y2:Float,drawMode:Int=0 )

	' Debug
	If image=Null
		canvas.DrawLine( x1,y1,x2,y2 )
		Return
	Endif

	If drawMode<0 Or drawMode>2 Return

	canvas.PushMatrix()

	' Normal
	If drawMode=0

		Local width:=Distance( x1,y1,x2,y2 )
		Local height:=image.Height
	
		Local angle:=ATan2( y2-y1,x2-x1 )
	
		canvas.Translate( x1,y1 )
		canvas.Rotate( -angle )
		canvas.Translate( 0,-height*.5 )

		canvas.DrawRect( 0,0,width,height,image )
	
	Endif

	' Stretched
	If drawMode=1

		Local width:=Distance( x1,y1,x2,y2 )
		Local height:=image.Height
	
		Local angle:=ATan2( y2-y1,x2-x1 )
	
		canvas.Translate( x1,y1 )
		canvas.Rotate( -angle )
		canvas.Translate( 0,-height*.5 )
		
		canvas.DrawRect( 0,0,width,height,image,0,0,width,height )

	Endif

	' Not clipped
	If drawMode=2

		Local width:=image.Width
		Local height:=image.Height
	
		Local angle:=ATan2( y2-y1,x2-x1 )
	
		canvas.Translate( x1,y1 )
		canvas.Rotate( -angle )
		canvas.Translate( 0,-height*.5 )

		canvas.DrawRect( 0,0,width,height,image )

	Endif

	canvas.PopMatrix()

End

#rem monkeydoc @hidden
#end
Function DrawLine( canvas:Canvas,image:Image,location1:Vec2f,location2:Vec2f,stretchMode:Bool=false )
	DrawLine( canvas,image,location1.x,location1.y,location2.x,location2.y,stretchMode )
End

#rem monkeydoc @hidden
#end
Function DrawRect( canvas:Canvas,x:Float,y:Float,width:Int,height:Int,pixels:Int=1 )

	canvas.DrawRect( x,y,width,pixels )
	canvas.DrawRect( x,y+pixels,pixels,height-( pixels*2 ) )
	canvas.DrawRect( x,y-pixels+height,width,pixels )
	canvas.DrawRect( x-pixels+width,y+pixels,pixels,height-( pixels*2 ) )

End

#rem monkeydoc @hidden
#end
Function DrawText( canvas:Canvas,text:String,x:Float,y:Float,handleX:Float=.5,handleY:Float=.5,centered:Bool=True,separator:String="~n" )

	Local width:=0

	Local lines:=text.Split( separator )

	For Local i:=0 Until lines.Length
		Local w:=canvas.Font.TextWidth( lines[i] )
		If w>width width=w
	Next

	Local height:=0
	For Local i:=0 Until lines.Length
		height+=canvas.Font.Height
	Next

	canvas.PushMatrix()
	
	canvas.Translate( -width*handleX,-height*handleY )

	For Local i:=0 Until lines.Length

		If centered=True
			canvas.DrawText( lines[i],x-( canvas.Font.TextWidth( lines[i] )/2-width/2 ),y )
		Else
			canvas.DrawText( lines[i],x,y )
		Endif

		y+=canvas.Font.Height

	Next

	canvas.PopMatrix()

End

#rem monkeydoc Draw seemingless tile background from one single image.
#end
Function DrawTiles( canvas:Canvas,image:Image,cameraX:Float,cameraY:Float,width:Int,height:Int ) Override

	Local tileWidth:Int=image.Width
	Local tileHeight:Int=image.Height

	Local x:=Int( cameraX )/tileWidth*tileWidth
	Local y:=Int( cameraY )/tileHeight*tileHeight

	x+=Int( cameraX )/tileWidth
	y+=Int( cameraY )/tileHeight

	canvas.PushMatrix()

	canvas.Translate( -cameraX,-cameraY )

	For Local h:=-2 Until width/tileWidth+4
		For Local v:=-2 Until height/tileHeight+4
			DrawImage( canvas,image,x+h*tileWidth,y+v*tileHeight,0,1,1,0,0 )
		Next
	Next

	canvas.PopMatrix()

End

#rem monkeydoc Draws a simple vertical progressbar.
#end
Function DrawVerticalProgressBar( canvas:Canvas,x:Int,y:Int,width:Int,height:Int,value:Int,maximum:Int,surfaceColor:Color=New Color( 0,.5,0,1 ),color:Color=New Color( 0,1,0,1 ),handleX:Float=.5,handleY:Float=.5 )

	Local s:=Float( maximum )/Float( height )

	canvas.Color=surfaceColor

	canvas.PushMatrix()

	canvas.Translate( -width*handleX,-height*handleY )

	canvas.DrawRect( x,y,width,height )

	If value>0
		canvas.Color=color
		If value<height*s
			canvas.DrawRect( x,y+height,width,-( value/s ) )
		Else
			canvas.DrawRect( x,y,width,height )
		Endif
	Endif

	canvas.PopMatrix()

End

#rem monkeydoc @hidden
#end
Function LightTweaker( light:Image,color:Key=Key.C,depth:Key=Key.D,scale:Key=Key.S,r:Key=Key.R,g:Key=Key.G,b:Key=Key.B,modifier:Key=Key.LeftShift )

	If Keyboard.KeyDown( modifier )

		If Keyboard.KeyDown( color ) light.Color-=.01 ; Print light.Color
		If Keyboard.KeyDown( depth ) light.LightDepth-=10 ; Print light.LightDepth
		If Keyboard.KeyDown( scale ) light.Scale-=.1 ; Print light.Scale

		If Keyboard.KeyDown( r ) light.Color-=New Color( .1,0,0 ) ; Print light.Color
		If Keyboard.KeyDown( g ) light.Color-=New Color( 0,.1,0 ) ; Print light.Color
		If Keyboard.KeyDown( b ) light.Color-=New Color( 0,0,.1 ) ; Print light.Color

	Else

		If Keyboard.KeyDown( color ) light.Color+=.01 ; Print light.Color
		If Keyboard.KeyDown( depth ) light.LightDepth+=10 ; Print light.LightDepth
		If Keyboard.KeyDown( scale ) light.Scale+=.1 ; Print light.Scale

		If Keyboard.KeyDown( r ) light.Color+=New Color( .1,0,0 ) ; Print light.Color
		If Keyboard.KeyDown( g ) light.Color+=New Color( 0,.1,0 ) ; Print light.Color
		If Keyboard.KeyDown( b ) light.Color+=New Color( 0,0,.1 ) ; Print light.Color

	Endif

End

#rem monkeydoc Load image frames from path.

Image frames should be laid out horizontally within the source image.

If padded is true, then each frame is assumed to have a transparent one pixel padding border around it.
#end
Function LoadFrames:Image[]( path:String,numFrames:Int,padded:Bool=False )

	Local image:=Image.Load( SmartPath( path ) )
	If image=Null Return Null

	Local cellWidth:=image.Width/numFrames,cellHeight:=image.Height
	
	Local width:=cellWidth
	Local x:=0

	If padded
		x+=1
		width-=2
	Endif
	
	Local frames:=New Image[numFrames]
	
	For Local i:=0 Until numFrames
		frames[i]=New Image( image,New Recti( i*cellWidth+x,0,i*cellWidth+x+width,cellHeight ) )
	Next
	
	Return frames

End

#rem monkeydoc Saves image to path.
#end
Function SaveImage( image:Image,path:String )

	Local canvas:=New Canvas( image )
	Local pixmap:=canvas.CopyPixmap( canvas.Viewport )

	pixmap.Save( path )

	pixmap.Discard()

End

Function Screenshot:Pixmap( canvas:Canvas )

	Local pixmap:=canvas.CopyPixmap( canvas.Viewport )
	Return pixmap

End

Function ScreenshotImage:Image( canvas:Canvas )
	
	Local pixmap:=Screenshot( canvas )
	Local image:=New Image( pixmap,TextureFlags.Dynamic )
	pixmap.Discard()
	Return image
	
End

#rem monkeydoc Set handles of image batch.
#end
Function SetHandle( image:Image[],handle:Vec2f )
	For Local i:=0 Until image.Length
		image[i].Handle=handle
	Next
End

#rem monkeydoc @hidden
#end
Function TextHeight:Int( font:Font,text:String,separator:String="~n" )

	Local lines:=text.Split( separator )

	Local height:=0
	For Local i:=0 Until lines.Length
		height+=font.Height
	Next

	Return height

End

#rem monkeydoc @hidden
#end
Function TextWidth:Int( font:Font,text:String,separator:String="~n" )

	Local lines:=text.Split( separator )

	Local width:=0
	For Local i:=0 Until lines.Length
		Local w:=font.TextWidth( lines[i] )
		If w>width width=w
	Next

	Return width

End

Function glColor:Color( r:Int,g:Int,b:Int,a:Int=255 )
	Local r2:=1.0/( 256.0/r )
	Local g2:=1.0/( 256.0/g )
	Local b2:=1.0/( 256.0/b )
	Local a2:=1.0/( 256.0/a )
	Return New Color( r2,g2,b2,a2 )
End
