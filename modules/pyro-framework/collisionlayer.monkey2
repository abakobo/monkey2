Namespace pyro.framework.collisionlayer

#rem monkeydoc The CollisionLayer class.
#end
Class CollisionLayer<T>

	Method New()
		_map=New T[640,480]
	End

	Method New( resolution:Int )
		_resolution=resolution
		_map=New T[640,480]
	End

	#rem monkeydoc Height.
	#end
	Property Height:Int()
		Return _map.GetSize( 1 )*_resolution
	End

	#rem monkeydoc Resolution.
	#end
	Property Resolution:Int()
		Return _resolution	
	End

	#rem monkeydoc Width.
	#end
	Property Width:Int()
		Return _map.GetSize( 0 )*_resolution
	End

	#rem monkeydoc Clears the layer.
	#end
	Method Clear()

		For Local y:=0 Until _map.GetSize( 1 )
			For Local x:=0 Until _map.GetSize( 0 )
				_map[x,y]=Null
			Next
		Next

	End

	Method CreateMask:Int[]( pixmap:Pixmap )

		Local mask:=New Int[2+pixmap.Width*pixmap.Height]

		mask[0]=pixmap.Width
		mask[1]=pixmap.Height

		For Local y:=0 Until pixmap.Height
			For Local x:=0 Until pixmap.Width
				Local value:UInt=pixmap.GetPixelARGB( x,y )
				If value&$ff000000 mask[2+pixmap.Width*y+x]=1
			End
		End

		Return mask

	End

	Method CreateMask:Int[]( path:String )
		Return CreateMask( Pixmap.Load( path ) )
	End

	#rem monkeydoc Shows the layer.
	
	Useful for debugging.
	#end
	Method Draw( canvas:Canvas )

		For Local y:=0 Until _map.GetSize( 1 )
			For Local x:=0 Until _map.GetSize( 0 )
				If _map[x,y] canvas.DrawRect( x*_resolution,y*_resolution,_resolution,_resolution )
			Next
		Next

	End

	#rem monkeydoc Draws the mask at coordinates x,y.
	#end
	Method Draw( x:Int,y:Int,value:T,mask:Int[],handleX:Float=.5,handleY:Float=.5 )

		_map=ExpandArray( _map,x+mask[0]*handleX+1,y+mask[1]*handleY+1 )

		x/=_resolution
		y/=_resolution

		x-=mask[0]*handleX
		y-=mask[1]*handleY

		For Local y1:=0 Until mask[1]
			For Local x1:=0 Until mask[0]
				If mask[2+y1*mask[0]+x1] And x+x1>0 And y+y1>0'' And x+x1<_map.GetSize( 0 ) And y+y1<_map.GetSize( 1 )
					_map[x+x1,y+y1]=value
				Endif
			Next
		Next

	End

	#rem monkeydoc Draws the mask at coordinates location.
	#end
	Method Draw( location:Vec2f,value:T,mask:Int[],handle:Vec2f=New Vec2f( .5,.5 ) )
		Draw( location.X,location.Y,value,mask,handle.X,handle.Y )
	End
	
	#rem monkeydoc Fills a rectangular area.
	#end
	Method Fill( x:Int,y:Int,width:Int,height:Int,value:T )

		If width<_resolution width=_resolution
		If height<_resolution height=_resolution

		x/=_resolution
		y/=_resolution

		width/=_resolution
		height/=_resolution

		If x/_resolution*_resolution<>x width+=1
		If y/_resolution*_resolution<>y height+=1

		For Local y1:=0 Until height
			For Local x1:=0 Until width
				If x+x1>0 And y+y1>0 And x+x1<_map.GetSize( 0 ) And y+y1<_map.GetSize( 1 ) _map[x+x1,y+y1]=value
			Next
		Next

	End

	#rem monkeydoc Returns the object from coordinates x,y.
	#end
	Method Get:T( x:Int,y:Int )

		x/=_resolution
		y/=_resolution

		If x>0 And y>0 And x<_map.GetSize( 0 ) And y<_map.GetSize( 1 ) Return _map[x,y]

		Return Null

	End

	#rem monkeydoc Returns the object from coordinates location
	#end
	Method Get:T( location:Vec2f )
		Return Get( location.X,location.Y )
	End
	
	#rem monkeydoc Returns the object from coordinates x,y using a mask.
	#end
	Method Get:T( x:Int,y:Int,mask:Int[],handleX:Float=.5,handleY:Float=.5 )

		x/=_resolution
		y/=_resolution

		x-=mask[0]*handleX
		y-=mask[1]*handleY

		For Local y1:=0 Until mask[1]
			For Local x1:=0 Until mask[0]
				If x+x1>0 And y+y1>0 And x+x1<_map.GetSize( 0 ) And y+y1<_map.GetSize( 1 ) And mask[2+y1*mask[0]+x1]And _map[x+x1,y+y1] Return _map[x+x1,y+y1]
			Next
		Next

		Return Null

	End

	#rem monkeydoc Returns the object from location using a mask.
	#end
	Method Get:T( location:Vec2f,mask:Int[],handle:Vec2f=New Vec2f( .5,.5 ) )
		Return Get( location.X,location.Y,mask,handle.X,handle.Y )
	End

	#rem monkeydoc Loads a mask from path.
	#end
	Method Load:Int[]( path:String )
		Return LoadIntArray( path )
	End

	Private

	Field _map:T[,]
	Field _resolution:=1

End