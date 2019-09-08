Namespace pyro.scenegraph

#Import "<std>"
#Import "<mojo>"
#Import "<pyro-framework>"

#Import "scenegraph/tilehelpers.monkey2"

Using std..
Using mojo..
Using pyro.framework..

#rem monkeydoc @hidden
#end
Global Particles:=0

#rem monkeydoc @hidden
#end
Function CopyLayerObject:LayerObject( source:LayerObject,copy:LayerObject=New LayerObject )

	If source=Null Return Null
	If copy=Null Return Null

	CopyTask( source,copy )

	copy.BlendMode=source.BlendMode
	copy._block=source._block
	copy.Color=source.Color
	copy.GlobalProperties=source.GlobalProperties
	copy._group=source._group
	copy.Handle=source.Handle
	copy._height=source._height
	copy.LoaderData=source.LoaderData
	copy._location=source._location
	copy._layer=source._layer
	copy.Rotation=source.Rotation
	copy.Scale=source.Scale
	copy.Selected=source.Selected
	copy.Tattooed=source.Tattooed
	copy.Visible=source.Visible
	copy._width=source._width
	copy.Z=source.Z

	If source.LoaderData
		copy.LoaderData.CopyData( source.LoaderData )
	Endif

	If source.Properties
		copy.Properties.CopyData( source.Properties )
	Endif

	Return copy

End

#rem monkeydoc @hidden
#end
Function CopyLayerSprite:LayerSprite( source:LayerSprite,copy:LayerSprite=New LayerSprite )

	If source=Null Return Null
	If copy=Null Return Null

	CopyLayerObject( source,copy )

	copy._flippedX=source._flippedX
	copy._flippedY=source._flippedY
	copy.Frame=source.Frame
	copy._image=source._image

	Return copy

End

#rem monkeydoc Convert a Pixmap to a tilemap.
#end
Function PixmapToTilemap( layer:Layer,filePath:String,x:Int=0,y:Int=0,space:Int=1,lineWidth:Int=0 )
	PixmapToTilemap( layer,Pixmap.Load( filePath ),x,y,space,lineWidth )
End

#rem monkeydoc Convert a Pixmap to a tilemap.
#end
Function PixmapToTilemap( layer:Layer,pixmap:Pixmap,x:Int=0,y:Int=0,space:Int=1,lineWidth:Int=0 )

	Local scene:=layer.Scene

	For Local h:=0 Until pixmap.Width

		For Local v:=0 Until pixmap.Height

			Local pixel:UInt=pixmap.GetPixelARGB( h,v )
			If pixel&$ff000000

				Local tile:=New LayerRectangle( layer,layer.TileSize.x,layer.TileSize.y )
				tile.LineWidth=lineWidth
				ToTilePos( tile,x+h,y+v )

				tile.Width-=space
				tile.Height-=space

				local r:=Float( UByte( pixel Shr 16 ) )/255
				local g:=Float( UByte( pixel Shr 8 ) )/255
				local b:=Float( UByte( pixel ) )/255

				tile.Color=New Color( r,g,b )

			Endif

		End

	End

End

#rem monkeydoc Init horizontal split screen.
#end
Function SetHorizontalSplitScreen( camera1:Camera,camera2:Camera,resolution:Vec2i )

	camera1.Viewport=New Recti( 0,0,resolution.x*.5-1,resolution.y )
	camera2.Viewport=New Recti( resolution.x*.5+1,0,resolution.x,resolution.y )

End

#rem monkeydoc Init vertical split screen.
#end
Function SetVerticalSplitScreen( camera1:Camera,camera2:Camera,resolution:Vec2i )

	camera1.Viewport=New Recti( 0,0,resolution.x,resolution.y*.5-1 )
	camera2.Viewport=New Recti( 0,resolution.y*.5+1,resolution.x,resolution.y )

End

Function Synchronize( scene:Scene )

	For Local i1:=0 Until scene.Layers.Length
		Local layer:=scene.Layers.Get( i1 )
		For Local x:=0 Until layer.LayerObjects.GetSize( 0 )
			For Local y:=0 Until layer.LayerObjects.GetSize( 1 )
				If layer.LayerObjects[x,y]<>Null And layer.LayerObjects[x,y].Length<>0
					For Local i2:=0 Until layer.LayerObjects[x,y].Length
						Local layerSprite:=Cast<LayerSprite>( layer.LayerObjects[x,y].Get( i2 ) )
						If layerSprite<>Null And layerSprite.FrameTime<>Null And layerSprite.FrameTime.Length=layerSprite._image.Length And layerSprite.TaskManager<>Null And layerSprite.SyncMaster=Null And layerSprite.UniqueIdentifier=True
							layerSprite.TaskManager=scene.TaskManager
						Endif
					Next
				Endif
			Next
		Next
	Next

	For Local i1:=0 Until scene.Layers.Length
		Local layer:=scene.Layers.Get( i1 )
		For Local x:=0 Until layer.LayerObjects.GetSize( 0 )
			For Local y:=0 Until layer.LayerObjects.GetSize( 1 )
				If layer.LayerObjects[x,y]<>Null And layer.LayerObjects[x,y].Length<>0
					For Local i2:=0 Until layer.LayerObjects[x,y].Length
						Local layerSprite:=Cast<LayerSprite>( layer.LayerObjects[x,y].Get( i2 ) )
						If layerSprite<>Null And layerSprite.FrameTime<>Null And layerSprite.FrameTime.Length=layerSprite._image.Length And layerSprite.TaskManager<>scene.TaskManager And layerSprite.SyncMaster=Null
							layerSprite.SyncMaster=layerSprite.GetSyncronizer()
						Endif
					Next
				Endif
			Next
		Next
	Next

'	Print scene.TaskManager.Tasks.Length

#rem
	For Local i1:=0 Until scene.Layers.Length
		Local layer:=scene.Layers.Get( i1 )
		For Local x:=0 Until layer.LayerObjects.GetSize( 0 )
			For Local y:=0 Until layer.LayerObjects.GetSize( 1 )
				If layer.LayerObjects[x,y]<>Null And layer.LayerObjects[x,y].Length<>0
					For Local i2:=0 Until layer.LayerObjects[x,y].Length
						Local layerSprite:=Cast<LayerSprite>( layer.LayerObjects[x,y].Get( i2 ) )
						If layerSprite<>Null And layerSprite._frameTime<>Null And layerSprite._frameTime.Length=layerSprite._image.Length And layerSprite.TaskManager<>Null And layerSprite.SyncMaster=Null layerSprite.TaskManager=scene.TaskManager
					Next
				Endif
			Next
		Next
	Next
#end

End

#rem monkeydoc The ISorter interface.
#end
Interface ISorter

	Method Update( objects:Stack<LayerObject> )

End

#rem monkeydoc The Camera class.

A scene is assumed to have at least 1 active camera that contains information about the projective transformation used while rendering the scene.

#end
Class Camera

	#rem monkeydoc Clear color.
	#end
	Field ClearColor:Color
	#rem monkeydoc Whether the camera is enabled.
	#end
	Field Enabled:=True
	#rem monkeydoc Location.
	#end
	Field Location:=New Vec2f
	Field Name:=""
	Field Offset:=New Vec2f
	Field Rotation:=0.0
	#rem monkeydoc Rotation point.

	Default is 0,0

	#end
	Field RotationPoint:=New Vec2f
	#rem monkeydoc @hidden
	#end
	Field Scale:=New Vec2f( 1,1 )
	#rem monkeydoc Whether the camera is tattooed.
	
	When tattooed is enabled, the camera will not be removed when Remove() is used.
	
	#end
	Field Tattooed:=False
	#rem monkeydoc Whether the camera is visible.
	#end
	Field Visible:=True
	#rem monkeydoc Location z.
	#end
	Field Z:=0.0
	#rem monkeydoc Zoom.
	#end
	Field Zoom:=1.0
	#rem monkeydoc Zoom point.

	Default is 0,0

	#end
	Field ZoomPoint:=New Vec2f

	Method New( scene:Scene )
		Scene=scene
	End

	#rem monkeydoc Rotation x.

	Default is 0

	#end
	Property RotationPointX:Float()
		Return RotationPoint.x
	Setter( rotationPointX:Float )
		RotationPoint.x=rotationPointX
	End

	#rem monkeydoc Rotation y.

	Default is 0

	#end
	Property RotationPointY:Float()
		Return RotationPoint.y
	Setter( rotationPointY:Float )
		RotationPoint.y=rotationPointY
	End

	#rem monkeydoc @hidden
	#end
	Property ScaleX:Float()
		Return Scale.x
	Setter( scaleX:Float )
		Scale.x=scaleX
	End

	#rem monkeydoc @hidden
	#end
	Property ScaleY:Float()
		Return Scale.y
	Setter( scaleY:Float )
		Scale.y=scaleY
	End

	#rem monkeydoc Scene.
	#end
	Property Scene:Scene()

		Return _scene

	Setter ( scene:Scene )

		If _scene _scene._cameras.RemoveEach( Self )
		_scene=scene
		If _scene _scene._cameras.Push( Self )

	End

	#rem monkeydoc View.
	#end
	Property View:View()
		If _scene=Null Return Null
		Return _scene.View
	End

	#rem monkeydoc Viewport.
	
	The viewport describes the rect within the render target that rendering occurs in.
	
	All rendering is relative to the top-left of the viewport, and is clipped to the intersection of the current viewport and scissor rects.

	By default a camera has no viewport instead using the Window Viewport.

	#end
	Property Viewport:Recti()
		If _viewport<>Null Return _viewport
		Return _scene.View.Rect
	Setter( viewport:Recti )
		_viewport=viewport
	End

	#rem monkeydoc Virtual resolution.
	#end
	Property VirtualResolution:Vec2f()
		Return New Vec2f( _scene.View.Width/_aspectRatio,_scene.View.Height/_aspectRatio )
	End

	#rem monkeydoc Virtual resolution.

	Returns virtual height.

	#end
	Property VirtualHeight:Int()
		Return _scene.View.Height/_aspectRatio
	End

	#rem monkeydoc Virtual resolution.

	Returns virtual width.

	#end
	Property VirtualWidth:Int()
		Return _scene.View.Width/_aspectRatio
	End

	#rem monkeydoc Location x.
	#end
	Property X:Float()
		Return Location.x
	Setter( x:Float )
		Location.x=x
	End

	#rem monkeydoc Location y.
	#end
	Property Y:Float()
		Return Location.y
	Setter( y:Float )
		Location.y=y
	End

	#rem monkeydoc Zoom x.

	Default is 0

	#end
	Property ZoomPointX:Float()
		Return ZoomPoint.x
	Setter( zoomPointX:Float )
		ZoomPoint.x=zoomPointX
	End

	#rem monkeydoc Zoom y.

	Default is 0

	#end
	Property ZoomPointY:Float()
		Return ZoomPoint.y
	Setter( zoomPointY:Float )
		ZoomPoint.y=zoomPointY
	End

	#rem monkeydoc Background draw call.
	
	Draw calls from DrawBackground are drawn first.
	
	#end
	Method DrawBackground( canvas:Canvas ) Virtual
	End
	
	#rem monkeydoc Debug draw call.
	
	Draw calls from DragDebug are drawn last.
	
	#end
	Method DrawDebug( canvas:Canvas ) Virtual
	End

	#rem monkeydoc Foreground draw call.
	
	Draw calls from DrawForeground are drawn after DrawBackground and layers but before DrawDebug.
	
	#end
	Method DrawForeground( canvas:Canvas ) Virtual
	End

	#rem monkeydoc Mouse location relative to the layer.
	#end
	Method GetMouseLocation:Vec2f( layer:Layer )

		Local mouse:=_scene.View.MouseLocation+Offset

		Local x1:=mouse.x/Scale.x
		Local y1:=mouse.y/Scale.y

		Local x2:=ZoomPoint.x
		Local y2:=ZoomPoint.y

		x1-=Viewport.min.x/Scale.x
		y1-=Viewport.min.y/Scale.y
	
		x2-=( ZoomPoint.x-RotationPoint.x )
		y2-=( ZoomPoint.y-RotationPoint.y )
	
		Local angle:=ATan2( y1-y2,x1-x2 )+Rotation*RotationMode
		Local distance:=Sqrt( Pow( y1-y2,2 )+Pow( x1-x2,2 ) )

		x2+=Cos( angle )*distance
		y2+=Sin( angle )*distance

		x2/=Zoom
		x2+=ZoomPoint.x-ZoomPoint.x/Zoom
		x2+=Location.x*layer.Multiplier.x
		x2-=layer.Location.x

		y2/=Zoom
		y2+=ZoomPoint.y-ZoomPoint.y/Zoom
		y2+=Location.y*layer.Multiplier.y
		y2-=layer.Location.y	

		Return New Vec2f( x2,y2 )

	End

	#rem monkeydoc Whether the point is inside the Viewport.
	#end
	Method PointInside:Bool( x:Int,y:Int )
		Return PointInsideRect( Viewport.min.x,Viewport.min.y,Viewport.max.x,Viewport.max.y,x,y,0,0,0 )
	End

	#rem monkeydoc Whether the point is inside the Viewport.
	#end
	Method PointInside:Bool( location:Vec2i )
		Return PointInside( location.x,location.y )
	End

	#rem monkeydoc Remove camera.

	Camera will not be removed when Tattooed is True.

	#end
	Method Remove() Virtual

		If Tattooed=True Return

		If _scene Scene=Null

	End

	Method SetOverlay( canvas:Canvas )

		If _viewport<>Null canvas.Viewport=_viewport

		canvas.Scale( Scale.x,Scale.y )

	End
	
	Method Translate( canvas:Canvas )

		canvas.Translate( -Offset )

		canvas.Translate( RotationPoint.x,RotationPoint.y )
		canvas.Rotate( Rotation*RotationMode )
		canvas.Translate( -RotationPoint.x,-RotationPoint.y )

		canvas.Translate( ZoomPoint.x,ZoomPoint.y )
		canvas.Scale( Zoom,Zoom )
		canvas.Translate( -ZoomPoint.x,-ZoomPoint.y )

		canvas.Translate( -Location.x,-Location.y )

	End

	#rem monkeydoc @hidden
	#end
	Method SetVirtualResolution( width:Float,height:Float )

		Local w:=Float( _scene.View.Width )
		Local h:=Float( _scene.View.Height )
	
		If h/w>=height/width

			Local scale:=w/width

			Scale=New Vec2f( scale,scale )

		Else
		
			Local scale:=h/height

			Scale=New Vec2f( scale,scale )
	
		Endif

		Offset.x=width*Scale.x*.5-VirtualWidth*.5
		Offset.y=height*Scale.y*.5-VirtualHeight*.5

		_aspectRatio=1

	End
	
	#rem monkeydoc Set virtual resolution.

	Width is stretched according to ratio.

	#end
	Method SetVirtualHeight( height:Float )
		_aspectRatio=_scene.View.Height/height
		Scale=New Vec2f( _aspectRatio,_aspectRatio )
	End

	#rem monkeydoc Set virtual resolution.

	Height is stretched according to ratio.

	#end
	Method SetVirtualWidth( width:Float )
		_aspectRatio=_scene.View.Width/width
		Scale=New Vec2f( _aspectRatio,_aspectRatio )
	End

	Private

	Method _Draw( canvas:Canvas )

		Local viewport:=canvas.Viewport

		canvas.PushMatrix()
		
		If _viewport<>Null canvas.Viewport=_viewport

		If ClearColor<>Null canvas.Clear( ClearColor )

		canvas.Translate( -Offset )

		canvas.Scale( Scale.x,Scale.y )

		canvas.Translate( RotationPoint.x,RotationPoint.y )
		canvas.Rotate( Rotation*RotationMode )
		canvas.Translate( -RotationPoint.x,-RotationPoint.y )

		canvas.Translate( ZoomPoint.x,ZoomPoint.y )
		canvas.Scale( Zoom,Zoom )
		canvas.Translate( -ZoomPoint.x,-ZoomPoint.y )

		canvas.PushMatrix()
		canvas.Translate( -Location.x,-Location.y )
		DrawBackground( canvas )
		canvas.PopMatrix()

		_DrawLayers( canvas )

		DrawForeground( canvas )

		DrawDebug( canvas )

		canvas.PopMatrix()

		canvas.Viewport=viewport

	End

	Method _DrawLayers( canvas:Canvas )

		For Local i:=0 Until _scene.Layers.Length
			If _scene.Layers.Get( i ).Enabled=False Continue
			If _scene.Layers.Get( i ).Visible=False Continue
			_scene.Layers.Get( i )._Draw( canvas,Self )
		Next

	End

	Method _Update()
		_UpdateLayers()
	End

	Method _UpdateLayers()

		For Local i:=0 Until _scene.Layers.Length
			If _scene.Layers.Get( i ).Enabled=False Continue
			_scene.Layers.Get( i )._Update( Self )
		Next

	End

	Field _aspectRatio:=1.0
	Field _scene:Scene
	Field _viewport:Recti

End

#rem monkeydoc The CollisionMonitor class.

The collision monitor can detect collisions between layer objects.

#end
Class CollisionMonitor

	#rem monkeydoc @hidden
	#end
	Method Update() Virtual

		Local r:Int

		While r<_readers.Length

			If r<0 Exit

			If Pause() And _readers.Get( r ).Pausable r+=1 ; Continue

			If _readers.Get( r ).Enabled=False r+=1 ; Continue

			Local w:Int

			While w<_writers.Length

				If r<0 Exit

				If _readers.Get( r )' And _writers.Get( w )

					If w<0 w+=1 ; Continue

					If Pause() And _readers.Get( r ).Pausable r+=1 ; Continue
					If Pause() And _writers.Get( w ).Pausable w+=1 ; Continue

					If _readers.Get( r ).Enabled=False r+=1 ; Continue
					If _writers.Get( w ).Enabled=False w+=1 ; Continue
	
					If _readers.Get( r )=_writers.Get( w ) w+=1 ; Continue
	
					If _readers.Get( r ).ScoreCollector=_writers.Get( w ) w+=1 ; Continue
	
					Local readers:=_readers.Length
					Local writers:=_writers.Length
	
					If _readers.Get( r ).CollisionMethod( _writers.Get( w ) ) _readers.Get( r ).OnCollision( _writers.Get( w ) )
	
					If _readers.Length<readers r+=( _readers.Length-readers )
					If _writers.Length<writers w+=( _writers.Length-writers )

				Endif

				w+=1

			Wend

			r+=1

		Wend

	End

	Private

	Field _readers:=New Stack<LayerObject>
	Field _writers:=New Stack<LayerObject>

End

#rem monkeydoc The Ellipse class.
#end
Class LayerEllipse Extends LayerObject

	#rem monkeydoc Orientation
	#end
	Field Orientation:=0

	Method New()
	End

	Method New( layer:Layer )
		Layer=layer
	End

	Method New( layer:Layer,radiusx:Float,radiusy:Float )
		_height=radiusy
		_width=radiusx
		Layer=layer
	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		canvas.BlendMode=BlendMode
		canvas.Color=Brightness( Color )

		canvas.DrawEllipse( _location.x,_location.y,_width,_height )

	End

End

#rem monkeydoc
The Emitter class.
#end
Class Emitter

	#rem monkeydoc @hidden
	#end
	Field Cache:EmitterCache[]
	#rem monkeydoc @hidden
	#end
	Field Data:Config[]

	Function Random:Float( in:Float[] )

		If in.Length=1 Return in[0]

		Return Rnd( in[0],in[1] )

	End

	Method New()
	End
	
	Method New( filePath:String )
		Load( filePath )
	End

	#rem monkeydoc @hidden
	#end
	Property Content:ContentManager()
		If _content<>Null Return _content
		Return pyro.framework.contentmanager.Content
	Setter( content:ContentManager )
		_content=content
	End

	#rem monkeydoc Load particle effect from a file.
	#end
	Method Load( filePath:String )

		Local obj:=LoadString( SmartPath( filePath ) ).Replace( "~n~r","~n" ).Trim().Split( "~n~n" )

		Cache=New EmitterCache[obj.Length]
		Data=New Config[obj.Length]

		For Local i:=0 Until Data.Length

			Data[i]=New Config
			Data[i].CreateData( obj[i] )

			OnLoading( Data[i] )

			Local accelerationX:=Data[i].ReadString( "accelerationX" ).Split( "," )
			Local accelerationY:=Data[i].ReadString( "accelerationY" ).Split( "," )
			Local alpha:=Data[i].ReadString( "alpha" ).Split( "," )
			Local angle:=Data[i].ReadString( "angle" ).Split( "," )
			Local blendMode:=Data[i].ReadString( "blendMode" )
			Local blue:=Data[i].ReadString( "blue" ).Split( "," )
			Local delay:=Data[i].ReadString( "delay" ).Split( "," )
			Local dragX:=Data[i].ReadString( "dragX" ).Split( "," )
			Local dragY:=Data[i].ReadString( "dragY" ).Split( "," )
			Local frame:=Data[i].ReadString( "frame" ).Split( "," )
			Local gravity:=Data[i].ReadString( "gravity" ).Split( "," )
			Local green:=Data[i].ReadString( "green" ).Split( "," )
			Local handleX:=Data[i].ReadString( "handleX",.5 ).Split( "," )
			Local handleY:=Data[i].ReadString( "handleY",.5 ).Split( "," )
			Local imagePath:=Data[i].ReadStringData( "imagePath" )
			Local particles:=Data[i].ReadString( "particles" ).Split( "," )
			Local red:=Data[i].ReadString( "red" ).Split( "," )
			Local scale:=Data[i].ReadString( "scale" ).Split( "," )
			Local spin:=Data[i].ReadString( "spin" ).Split( "," )
			Local targetAlpha:=Data[i].ReadString( "targetAlpha" ).Split( "," )
			Local targetBlue:=Data[i].ReadString( "targetBlue" ).Split( "," )
			Local targetGreen:=Data[i].ReadString( "targetGreen" ).Split( "," )
			Local targetRed:=Data[i].ReadString( "targetRed" ).Split( "," )
			Local targetScale:=Data[i].ReadString( "targetScale" ).Split( "," )
			Local timeToLive:=Data[i].ReadString( "timeToLive" ).Split( "," )
			Local velocityX:=Data[i].ReadString( "velocityX" ).Split( "," )
			Local velocityY:=Data[i].ReadString( "velocityY" ).Split( "," )

			Cache[i]=New EmitterCache
			Cache[i].accelerationX=_ToFloatData( accelerationX )
			Cache[i].accelerationY=_ToFloatData( accelerationY )
			Cache[i].alpha=_ToFloatData( alpha )
			Cache[i].angle=_ToFloatData( angle )
			Cache[i].blendMode=ToBlendMode( blendMode )
			Cache[i].blue=_ToFloatData( blue )
			Cache[i].delay=_ToFloatData( delay )
			Cache[i].dragX=_ToFloatData( dragX )
			Cache[i].dragY=_ToFloatData( dragY )
			Cache[i].frame=_ToFloatData( frame )
			Cache[i].gravity=_ToFloatData( gravity )
			Cache[i].green=_ToFloatData( green )
			Cache[i].handleX=_ToFloatData( handleX )
			Cache[i].handleY=_ToFloatData( handleY )
			Cache[i].particles=_ToFloatData( particles )
			Cache[i].red=_ToFloatData( red )
			Cache[i].scale=_ToFloatData( scale )
			Cache[i].spin=_ToFloatData( spin )
			Cache[i].targetAlpha=_ToFloatData( targetAlpha )
			Cache[i].targetBlue=_ToFloatData( targetBlue )
			Cache[i].targetGreen=_ToFloatData( targetGreen )
			Cache[i].targetRed=_ToFloatData( targetRed )
			Cache[i].targetScale=_ToFloatData( targetScale )
			Cache[i].timeToLive=_ToFloatData( timeToLive )
			Cache[i].velocityX=_ToFloatData( velocityX )
			Cache[i].velocityY=_ToFloatData( velocityY )

			Cache[i].images=ResizeArray( Cache[i].images,imagePath.Length )
	
			For Local i2:=0 Until imagePath.Length
				Cache[i].images[i2]=Content.GetImage( imagePath[i2] )
			Next

		Next

	End

	#rem monkeydoc @hidden
	#end
	Method OnLoading( data:Config ) Virtual
	End

	Method Set( cache:EmitterCache,particle:Particle,size:Float=1 )

		particle.Images=cache.images

		particle.Acceleration=New Vec2f( Random( cache.accelerationX )*size,Random( cache.accelerationY )*size )
		particle.BlendMode=cache.blendMode
		particle.Color=New Color( Random( cache.red ),Random( cache.green ),Random( cache.blue ),Random( cache.alpha ) )
		particle.Delay=Random( cache.delay )
		particle.Frame=Random( cache.frame )
		particle.Drag=New Vec2f( Random( cache.dragX ),Random( cache.dragY ) )
		particle.Gravity=Random( cache.gravity )
		particle.Handle=New Vec2f( Random( cache.handleX ),Random( cache.handleY ) )
		particle.Spin=Random( cache.spin )
		particle.TargetColor=New Color( Random( cache.targetRed ),Random( cache.targetGreen ),Random( cache.targetBlue ),Random( cache.targetAlpha ) )
		particle.TimeToLive=Random( cache.timeToLive )

		Local angle:=Random( cache.angle )/360.0*TwoPi
		Local scale:=Random( cache.scale )*size
		Local targetScale:=Random( cache.targetScale )*size

		particle.Rotation=angle
		particle.Scale=New Vec2f( scale,scale )
		particle.TargetScale=New Vec2f( targetScale,targetScale )
		particle.Velocity=New Vec2f( Cos( angle )*Random( cache.velocityX )*size,Sin( angle )*Random( cache.velocityY )*size )

	End

	#rem monkeydoc Play particle effect previously loaded from a file.
	#end
	Method Play( layer:Layer,x:Float,y:Float,size:Float=1,z:Float=0,floating:Bool=False )

		If DebugPause()=True Return
		If Pause()=True Return

		For Local i:=0 Until Cache.Length

			For Local particles:=0 Until Random( Cache[i].particles )
			
				Local particle:=New Particle

				particle.Layer=layer
				particle.TaskManager=layer.TaskManager

				particle.Location=New Vec2f( x,y )
				particle.Floating=floating
				particle.Z=z

				Set( Cache[i],particle,size )

			Next

		Next

	End

	Method Play( layer:Layer,location:Vec2f,size:Float=1,z:Float=0,floating:Bool=False )
		Play( layer,location.x,location.y,size,z,floating )
	End

	Private

	Function _ToFloatData:Float[]( in:String[] )

		Local out:=New Float[2]

		If in.Length<2

			out[0]=Float( in[0] )
			out[1]=Float( in[0] )

			Return out

		Endif

		out[0]=Float( in[0] )
		out[1]=Float( in[1] )

		Return out

	End

	Field _content:ContentManager

End

Struct EmitterCache

	Field accelerationX:Float[]
	Field accelerationY:Float[]
	Field alpha:Float[]
	Field angle:Float[]
	Field delay:Float[]
	Field blendMode:BlendMode
	Field blue:Float[]
	Field dragX:Float[]
	Field dragY:Float[]
	Field frame:Float[]
	Field gravity:Float[]
	Field green:Float[]
	Field handleX:Float[]
	Field handleY:Float[]
	Field images:Image[]
	Field particles:Float[]
	Field red:Float[]
	Field scale:Float[]
	Field spin:Float[]
	Field targetAlpha:Float[]
	Field targetBlue:Float[]
	Field targetGreen:Float[]
	Field targetRed:Float[]
	Field targetScale:Float[]
	Field timeToLive:Float[]
	Field velocityX:Float[]
	Field velocityY:Float[]

End

#rem monkeydoc The Layer class.

A layer acts like a transparent sheet upon which any number of layer objects and groups of layer objects can be placed.

#end
Class Layer

	#rem monkeydoc Ambient light color.
	
	Enables lighting mode when AmbientLight is set.
	
	#end
	Field AmbientLight:Color
	Field Color:Color
	#rem monkeydoc Cullig overhead.
	#end
	Field CullingOverhead:=New Vec2i( 2,2 )
	#rem monkeydoc @hidden
	#end
	Field DrawFlags:=False
	#rem monkeydoc @hidden
	#end
	Field DrawSelected:=True
	#rem monkeydoc Whether the layer is enabled.
	#end
	Field Enabled:=True
	#rem monkeydoc Height.
	#end
	Field Height:=0.0
	Field Identifier:=""
	#rem monkeydoc Layer objects.
	#end
	Field LayerObjects:=New Stack<LayerObject>[3,3]
	Field Location:=New Vec2f
	#rem monkeydoc Multiplier settings.
	
	Used for parallax scrolling.
	
	#end
	Field Multiplier:=New Vec2f( 1,1 )
	Field Name:=""
	#rem monkeydoc Pproperties.
	#end
	Field Properties:Config
	#rem monkeydoc Sorter.
	#end
	Field Sorter:ISorter
	#rem monkeydoc Task manager.
	#end
	Field TaskManager:=New TaskManager
	#rem monkeydoc Whether the layer is tattooed.
	
	When tattooed is enabled, the layer will not be removed when Remove() is used.
	
	#end
	Field Tattooed:=False
	Field Type:=0
	#rem monkeydoc Whether the layer is enabled.
	#end
	Field Visible:=True
	#rem monkeydoc Width.
	#end
	Field Width:=0.0
	Field Z:=0.0

	Method New()
	End

	Method New( scene:Scene )
		Scene=scene
	End

	#rem monkeydoc Width in tiles.
	#end
	Property HorizontalTiles:Int()
		Return LayerObjects.GetSize( 0 )
	End

	Property Scene:Scene()
		Return _scene
	Setter( scene:Scene )
		If _scene _scene.Layers.RemoveEach( Self )
		_scene=scene
		If _scene _scene.Layers.Push( Self )
	End

	#rem monkeydoc Multiplier X setting.
	
	Used for parallax scrolling.
	
	#end
	Property MultiplierX:Float()
		Return Multiplier.x
	Setter( multiplierX:Float )
		Multiplier.x=multiplierX
	End

	#rem monkeydoc Multiplier Y setting.
	
	Used for parallax scrolling.
	
	#end
	Property MultiplierY:Float()
		Return Multiplier.y
	Setter( multiplierY:Float )
		Multiplier.y=multiplierY
	End

	#rem monkeydoc Tile size.
	#end
	Property TileSize:Vec2i()

		Return _tileSize

	Setter( tileSize:Vec2i )

		_tileSize=tileSize

		For Local x:=0 Until LayerObjects.GetSize( 0 )
			For Local y:=0 Until LayerObjects.GetSize( 1 )
				If LayerObjects[x,y]<>Null
					For Local i:=0 Until LayerObjects[x,y].Length
						LayerObjects[x,y].Get( i ).__UpdateLocation()
					Next
				Endif
			Next
		Next

	End

	#rem monkeydoc Height in tiles.
	#end
	Property VerticalTiles:Int()
		Return LayerObjects.GetSize( 1 )
	End

	#rem monkeydoc View.
	#end
	Property View:View()
		If _scene=Null Return Null
		Return _scene.View
	End

	#rem monkeydoc Location x.
	#end
	Property X:Float()
		Return Location.x
	Setter( x:Float )
		Location.x=x
	End

	#rem monkeydoc Location y.
	#end
	Property Y:Float()
		Return Location.y
	Setter( y:Float )
		Location.y=y
	End

	#rem monkeydoc @hidden
	#end
	Method Center()
		X=_scene.View.Width/2-Width/2
		Y=_scene.View.Height/2-Height/2
	End

	#Rem monkeydoc Clears the layer from all layer objects.

	Object will not be removed when Tattooed is True.

	#End
	Method Clear()

		Local objects:=New Stack<LayerObject>
		
		For Local x:=0 Until LayerObjects.GetSize( 0 )
			For Local y:=0 Until LayerObjects.GetSize( 1 )
				If LayerObjects[x,y]<>Null And LayerObjects[x,y].Length<>0
					For Local i:=0 Until LayerObjects[x,y].Length
						objects.Push( LayerObjects[x,y].Get( i ) )
					Next
				Endif
			Next
		Next

		For Local i:=0 Until objects.Length
			objects.Get( i ).OnClearLayer()
		Next
		
	End

	Method CountLayerObjects:Int()

		Local objects:=0
		
		For Local x:=0 Until LayerObjects.GetSize( 0 )
			For Local y:=0 Until LayerObjects.GetSize( 1 )
				If LayerObjects[x,y]<>Null And LayerObjects[x,y].Length>0
					For Local i:=0 Until LayerObjects[x,y].Length
						objects+=1
					Next
				Endif
			Next
		Next

		Return objects
				
	End

	#rem monkeydoc Background draw call.
	
	Draw calls from DrawBackground are drawn first.
	
	#end
	Method DrawBackground( canvas:Canvas ) Virtual
	End
	
	#rem monkeydoc Debug draw call.
	
	Draw calls from DragDebug are drawn last.
	
	#end
	Method DrawDebug( canvas:Canvas ) Virtual
	End

	#rem monkeydoc Foreground draw call.
	
	Draw calls from DrawForeground are drawn after DrawBackground and layer objects but before DrawDebug.
	
	#end
	Method DrawForeground( canvas:Canvas ) Virtual
	End

	#Rem monkeydoc Get all LayerObject.
	#End
	Method GetAllLayerObjects:Stack<LayerObject>( enabled:Bool=True,visible:Bool=True )

		Local objects:=New Stack<LayerObject>
		
		For Local x:=0 Until LayerObjects.GetSize( 0 )
			For Local y:=0 Until LayerObjects.GetSize( 1 )
				If LayerObjects[x,y]<>Null And LayerObjects[x,y].Length>0
					For Local i:=0 Until LayerObjects[x,y].Length
						If LayerObjects[x,y].Get( i ).Enabled=enabled And LayerObjects[x,y].Get( i ).Visible=visible objects.Push( LayerObjects[x,y].Get( i ) )
					Next
				Endif
			Next
		Next

		Return objects
				
	End

	#Rem monkeydoc Get a layer object from position.
	#End
	Method GetLayerObject:LayerObject( camera:Camera,x:Int,y:Int,enabled:Bool=True,visible:Bool=True )

		Local objects:=GetLayerObjects( x,y )
		If objects.Length=0 Return Null

		For Local i:=objects.Length-1 To 0 Step -1
			Local o:=objects.Get( i )
			If o.Enabled=enabled And o.Visible=visible And o.PointInside( camera,New Vec2i( x,y ) ) Return o
		Next

		Return Null

	End

	#Rem monkeydoc Get a layer object from position.
	#End
	Method GetLayerObject:LayerObject( camera:Camera,location:Vec2i )
		Return GetLayerObject( camera,location.x,location.y )
	End

	#Rem monkeydoc Get layer objects from position.
	#End
	Method GetLayerObjects:Stack<LayerObject>( x:Int,y:Int,pixelCoords:Bool=True )

		If x<0 x=0
		If y<0 y=0

		__Expand( x,y )

		If pixelCoords=True
			x/=_tileSize.x
			y/=_tileSize.y
		Endif
		
		Return LayerObjects[x,y]
		
	End
	
	#Rem monkeydoc Get layer objects from position.
	#End
	Method GetLayerObjects:Stack<LayerObject>( location:Vec2i,pixelCoords:Bool=True )
		Return GetLayerObjects( location.x,location.y,pixelCoords )
	End

	#rem monkeydoc Mouse location relative to the camera.
	#end
	Method GetMouseLocation:Vec2f( camera:Camera )

		Local mouse:=_scene.View.MouseLocation+camera.Offset

		Local x1:=mouse.x/camera.Scale.x
		Local y1:=mouse.y/camera.Scale.y

		Local x2:=camera.ZoomPoint.x
		Local y2:=camera.ZoomPoint.y

		x1-=camera.Viewport.min.x/camera.Scale.x
		y1-=camera.Viewport.min.y/camera.Scale.y
	
		x2-=( camera.ZoomPoint.x-camera.RotationPoint.x )
		y2-=( camera.ZoomPoint.y-camera.RotationPoint.y )
	
		Local angle:=ATan2( y1-y2,x1-x2 )+camera.Rotation*RotationMode
		Local distance:=Sqrt( Pow( y1-y2,2 )+Pow( x1-x2,2 ) )

		x2+=Cos( angle )*distance
		y2+=Sin( angle )*distance

		x2/=camera.Zoom
		x2+=camera.ZoomPoint.x-camera.ZoomPoint.x/camera.Zoom
		x2+=camera.Location.x*Multiplier.x
		x2-=Location.x

		y2/=camera.Zoom
		y2+=camera.ZoomPoint.y-camera.ZoomPoint.y/camera.Zoom
		y2+=camera.Location.y*Multiplier.y
		y2-=Location.y	

		Return New Vec2f( x2,y2 )

	End

	#rem monkeydoc Get object by name.
	#end
	Method GetObject:Object( name:String ) Virtual

			If Name=name Return Self
	
			For Local x:=0 Until LayerObjects.GetSize( 0 )
			For Local y:=0 Until LayerObjects.GetSize( 1 )
				If LayerObjects[x,y]<>Null And LayerObjects[x,y].Length>0
					For Local i:=0 Until LayerObjects[x,y].Length
						Local search:=LayerObjects[x,y].Get( i ).GetObject( name )
						If search<>Null Return search
					Next
				Endif
			Next
		Next

		Return Null

	End

	#rem monkeydoc @hidden
	#end
	Method Groupify()
		
		Local objects:=GetAllLayerObjects()

		For Local i:=0 Until objects.Length
			If objects[i].Group=Null And objects[i].Groupify<>""
				Local master:=Cast<LayerGroup>( GetObject( objects[i].Groupify ) )
				If master<>Null
					objects[i].Group=master
				Endif
			Endif
		Next
		
	End

	Method Insert( scene:Scene,index:Int )

		If _scene _scene.Layers.RemoveEach( Self )
		_scene=scene
		If _scene _scene.Layers.Insert( index,Self )

	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseDown:Bool( mouseButton:MouseButton )
		If Scene=Null Return False
		Return Scene.InputDevice.ButtonDown[ mouseButton ]
	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseHit:Bool( mouseButton:MouseButton )
		If Scene=Null Return False
		Return Scene.InputDevice.ButtonHit[ mouseButton ]
	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseReleased:Bool( mouseButton:MouseButton )
		If Scene=Null Return False
		Return Scene.InputDevice.ButtonReleased[ mouseButton ]
	End

	#rem monkeydoc Remove layer.

	Layer will not be removed when Tattooed is True.

	#end
	Method Remove() Virtual
		If Tattooed=True Return
		If _scene Scene=Null
	End

	Method Scroll:Stack<LayerObject>( x:Float,y:Float )

		Local objects:=GetAllLayerObjects()

		For Local i:=0 Until objects.Length
			objects.Get(i).X+=x
			objects.Get(i).Y+=y
		Next

		Return objects

	End

	#rem monkeydoc @hidden
	#end
	Method ShowTilemapCounts()

		Local message:="No problems detected on layer "+String.FromChar( 39 )+Name+String.FromChar( 39 )

		For Local x:=0 Until LayerObjects.GetSize( 0 )
			For Local y:=0 Until LayerObjects.GetSize( 1 )
				If LayerObjects[x,y]<>Null And LayerObjects[x,y].Length>1
					message=""
					Print "Layer "+String.FromChar( 39 )+Name+String.FromChar( 39 )+" has "+LayerObjects[x,y].Length+" objects at "+x+","+y
				Endif
			Next
		Next

		If message<>"" Print message

	End

	Method __Expand:Stack<LayerObject>( x:Int,y:Int )

		' Deal with negatives later so this can be removed >
		If x<0 x=0
		If y<0 y=0
		' <

		x/=_tileSize.x
		y/=_tileSize.y

		LayerObjects=ExpandArray( LayerObjects,x+1,y+1 )

		If LayerObjects[x,y]=Null
			LayerObjects[x,y]=New Stack<LayerObject>
		Endif

		Return LayerObjects[x,y]

	End

	Private

	Method _CullLayerObjects( stack:Stack<LayerObject>,x:Int,y:Int,width:Int,height:Int )

		stack.Clear()

		If LayerObjects.GetSize( 0 )=0 Return
		If LayerObjects.GetSize( 1 )=0 Return

		Local startX:=x/_tileSize.x
		Local startY:=y/_tileSize.y
		Local endX:=startX+width/_tileSize.x
		Local endY:=startY+height/_tileSize.y

		startX-=CullingOverhead.x
		startY-=CullingOverhead.y
		endX+=CullingOverhead.x*2
		endY+=CullingOverhead.y*2

		If CullingOverhead.x=0
			startX-=1
			endX+=2
		Endif

		If CullingOverhead.y=0
			startY-=1
			endY+=2
		Endif

		LayerObjects=ExpandArray( LayerObjects,endX+1,endY+1 )

		If startX<0 startX=0
		If startY<0 startY=0

		If endX>LayerObjects.GetSize( 0 ) endX=LayerObjects.GetSize( 0 )
		If endY>LayerObjects.GetSize( 1 ) endY=LayerObjects.GetSize( 1 )

		_floaters.Clear()

		For Local x:=startX Until endX
			For Local y:=startY Until endY
				If LayerObjects[x,y]<>Null
					If LayerObjects[x,y].Length>0
						For Local i:=0 Until LayerObjects[x,y].Length
							If LayerObjects[x,y].Get( i ).Enabled=False Continue
							If LayerObjects[x,y].Get( i ).Floating=True
								_floaters.Push( LayerObjects[x,y].Get( i ) )
							Else
								stack.Push( LayerObjects[x,y].Get( i ) )
							Endif
						Next
					Endif
				Endif
			Next
		Next

		For Local i:=0 Until _floaters.Length
			stack.Push( _floaters.Get( i ) )
		Next

	End

	Method _Draw( canvas:Canvas,camera:Camera )

		canvas.PushMatrix()

		canvas.Translate( -camera.Location.x*Multiplier.x,-camera.Location.y*Multiplier.y )

		canvas.Translate( Location.x,Location.y )
		
		DrawBackground( canvas )

		_DrawObjects( canvas,camera )

		DrawForeground( canvas )

		DrawDebug( canvas )

		canvas.PopMatrix()

	End

	Method _DrawObjects( canvas:Canvas,camera:Camera )

		Local x:=camera.Location.x*Multiplier.x
		Local y:=camera.Location.y*Multiplier.y
		Local width:=camera.Viewport.Width
		Local height:=camera.Viewport.Height

		' Temporarly fix needed for editor stuff! ( probably buggy )
		width=width/camera.Zoom
		height=height/camera.Zoom
		width=width/camera.ScaleX
		height=height/camera.ScaleY
		' ------------------------

		_CullLayerObjects( _renders,x,y,width,height )

		If Sorter<>Null Sorter.Update( _renders )

		If AmbientLight<>Null
			canvas.AmbientLight=AmbientLight
			canvas.BeginLighting()
		Endif

		For Local i:=0 Until _renders.Length
			If _renders.Get( i ).Visible=True _renders.Get( i ).Draw( canvas )
		Next

		If AmbientLight<>Null canvas.EndLighting()

	End

	Method _Update( camera:Camera )
		_UpdateObjects( camera )
	End

	Method _UpdateObjects( camera:Camera )

		Local x:=camera.Location.x*Multiplier.x
		Local y:=camera.Location.y*Multiplier.y
		Local width:=camera.Viewport.Width
		Local height:=camera.Viewport.Height

		' Temporarly fix needed for editor stuff! ( probably buggy )
		width=width/camera.Zoom
		height=height/camera.Zoom
		width=width/camera.ScaleX
		height=height/camera.ScaleY
		' ------------------------

		_CullLayerObjects( _updates,x,y,width,height )

		For Local i:=0 Until _updates.Length

			If DebugPause()=True Continue
			If _updates.Get( i ).TaskManager<>TaskManager Continue
			If _updates.Get( i )._updateCounter=_scene._updateCounter Continue
			If _updates.Get( i ).Enabled=False Continue
			If _updates.Get( i ).Pausable=True And Pause()=True Continue

			_updates.Get( i ).OnUpdate()

			_updates.Get( i )._updateCounter=_scene._updateCounter

		Next

	End

	Field _floaters:=New Stack<LayerObject>
	Field _renders:=New Stack<LayerObject>
	Field _scene:Scene
	Field _tileSize:=New Vec2i( 2048,2048 )
	Field _updates:=New Stack<LayerObject>

End

#rem monkeydoc The LayerGroup class.
#end
Class LayerGroup Extends LayerObject

	Field Camera:=New Vec2f
	#rem monkeydoc Member objects.
	#end
	Field Members:=New Stack<LayerObject>

	Method New()
	End
	
	Method New( layer:Layer )
		Layer=layer
	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		canvas.PushMatrix()

		canvas.Translate( _location.x,_location.y )

		canvas.Scale( Scale.x,Scale.y )

		canvas.Translate( -Width*Handle.x,-Height*Handle.y )

		canvas.Translate( Width*Handle.x,Height*Handle.y )
		canvas.Rotate( Rotation*RotationMode )
		canvas.Translate( -Width*Handle.x,-Height*Handle.y )

		canvas.BlendMode=BlendMode
		canvas.Color=Brightness( Color )

		DrawBackground( canvas )

		canvas.Translate( -Camera.x,-Camera.y )

		_DrawMembers( canvas )

		canvas.PopMatrix()

		If Selected __DrawSelected( canvas )

	End

	#rem monkeydoc Background draw call.
	
	Draw calls from DragBackground are drawn first.
	
	#end
	Method DrawBackground( canvas:Canvas ) Virtual
	End

	#rem monkeydoc Content height.
	#end
	Method GetContentHeight:Float()

		Local y1:=0

		For Local i:=0 Until Members.Length
			Local y:=Members.Get( i )._location.y
			y-=Members.Get( i ).ScaledHeight*Members.Get( i ).Handle.y
			If Members.Get( i ).Enabled=True And y<y1 y1=y
		Next

		Local y2:=0

		For Local i:=0 Until Members.Length
			Local y:=Members.Get( i )._location.y
			y+=Members.Get( i ).ScaledHeight*( 1.0-Members.Get( i ).Handle.y )
			If Members.Get( i ).Enabled=True And y>y2 y2=y
		Next

		Return y2-y1

	End

	#rem monkeydoc Content width.
	#end
	Method GetContentWidth:Float()

		Local x1:=0

		For Local i:=0 Until Members.Length
			Local x:=Members.Get( i )._location.x
			x-=Members.Get( i ).ScaledWidth*Members.Get( i ).Handle.x
			If Members.Get( i ).Enabled=True And x<x1 x1=x
		Next

		Local x2:=0

		For Local i:=0 Until Members.Length
			Local x:=Members.Get( i )._location.x
			x+=Members.Get( i ).ScaledWidth*( 1.0-Members.Get( i ).Handle.x )
			If Members.Get( i ).Enabled=True And x>x2 x2=x
		Next

		Return x2-x1

	End

	#rem monkeydoc @hidden
	#end
	Method GetObject:Object( name:String ) Override
		If Name=name Return Self
		For Local i:=0 Until Members.Length
			Local search:=Members[i].GetObject( name )
			If search<>Null Return search
		Next
		Return Null
	End

	Private

	Method _DrawMembers( canvas:Canvas )
		For Local i:=0 Until Members.Length
			If Members.Get( i ).Enabled=False Continue
			If Members.Get( i ).Visible=False Continue
			Members.Get( i ).Draw( canvas )
		Next
	End

End

#rem monkeydoc The LayerLabel class.
#end
Class LayerLabel Extends LayerObject

	#rem monkeydoc Font.
	#end
	Field Font:=NullFont()
	#rem monkeydoc Text.
	#end
	Field Text:=""

	Method New()
	End

	Method New( layer:Layer,font:Font,text:String )
		Layer=layer
		Font=font
		Text=text
	End

	#rem monkeydoc Height.
	#end
	Property Height:Float() Override
		If Font=Null Return 0
		If Text="" Return 0
		Return Font.Height
	End

	#rem monkeydoc Width.
	#end
	Property Width:Float() Override
		If Font=Null Return 0
		If Text="" Return 0
		Return Font.TextWidth( Text )
	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		If Font=Null Return
		If Text="" Return

		canvas.PushMatrix()

		canvas.BlendMode=BlendMode
		canvas.Color=Brightness( Color )

		canvas.Font=Font

		canvas.Translate( _location.x,_location.y )
		canvas.Rotate( Rotation*RotationMode )
		canvas.Scale( Scale.x,Scale.y )
		canvas.Translate( -Handle.x*Width,-Handle.y*Height )

		canvas.DrawText( Text,0,0 )
''		canvas.DrawText( Text,_location.x,_location.y,_handle.x,_handle.y )

		canvas.PopMatrix()

	End

End

#rem monkeydoc The LayerLine class.
#end
Class LayerLine Extends LayerObject

	Field Frame:=0.0
	Field Type:=1

	Method New()
	End

	Method New( layer:Layer )
		Layer=layer
	End

	Method New( layer:Layer,image:Image )
		Layer=layer
		Image=image
	End

	Method New( layer:Layer,images:Image[] )
		Layer=layer
		Images=images
	End

#rem monkeydoc
Constructor.
#end
	Method New( layer:Layer,x:Float,y:Float,x2:Float,y2:Float )

		_location.x=x
		_location.y=y
		_location2.x=x2
		_location2.y=y2

		_width=Abs( _location.x-_location2.x )
		_height=Abs( _location.y-_location2.y )

		Layer=layer

	End

	#rem monkeydoc Number of image frames.
	#end
	Property Frames:Int()
		Return _image.Length
	End

	#rem monkeydoc Height.
	#end
	Property Height:Float() Override
		Return Abs( _location.y-_location2.y )
	Setter( height:Float ) Override
		_location2.y=_location.y+height
	End

	#rem monkeydoc Current image.
	#end
	Property Image:Image()
		Return _image[ ( Round( Frame ) Mod _image.Length ) ]
	Setter ( image:Image )
		_image=ResizeArray( _image,1 )
		_image[0]=image
	End

	#rem monkeydoc Current images.
	#end
	Property Images:Image[]()
		Return _image
	Setter( images:Image[] )
		_image=images
	End

	#rem monkeydoc last frame.
	#end
	Property LastFrame:Int()
		Return _image.Length-1
	End

	#rem monkeydoc Location 2.
	#end
	Property Location2:Vec2f()
		Return _location2
	Setter( location2:Vec2f )
		_location2=location2
	End

	#rem monkeydoc Width.
	#end
	Property Width:Float() Override
		Return Abs( _location.x-_location2.x )
	Setter( width:Float ) Override
		_location2.x=_location.x+width
	End

	#rem monkeydoc Location x2.
	#end
	Property X2:Float()
		Return _location2.x
	Setter( x2:Float )
		_location2.x=x2
	End

	#rem monkeydoc Location y2.
	#end
	Property Y2:Float()
		Return _location2.y
	Setter( y2:Float )
		_location2.y=y2
	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		canvas.BlendMode=BlendMode
		canvas.Color=Brightness( Color )

		If Type=1 And _image.Length=0
			canvas.DrawLine( _location.x,_location.y,_location2.x,_location2.y )
		Else
			Local drawMode:=Type-1
			DrawLine( canvas,Image,_location.x,_location.y,_location2.x,_location2.y,drawMode )
		Endif

	End

	Private

	Field _image:Image[]
	Field _location2:=New Vec2f

End

#rem monkeydoc The LayerObject class.
#end
''Class LayerObject Extends pyro.framework.taskmanager.Task
Class LayerObject Extends Task

	#rem monkeydoc Blend mode.
	#end	
	Field BlendMode:=mojo.graphics.BlendMode.Alpha
	#rem monkeydoc Drawing color.
	
	Note that [[Alpha]] and the alpha component of [[Color]] are multiplied together to produce the final alpha value for rendering. This allows you to use [[Alpha]] as a 'master' alpha level.

	#end
	Field Color:=New Color( 1,1,1,1 )
	Field Flags:=-1
	#rem monkeydoc Whether floating is enabled.
	
	Object will 'float' to the top to combat cell bound artefacts.
	
	Use this when mixing tiles with sprites and object is a sprite.
	
	#end
	Field Floating:=False
	#rem monkeydoc Global properties.
	#end
	Field GlobalProperties:Config
	#rem monkeydoc Handle.
	
	Handle values are fractional, where 0,0 is the top-left and 1,1 is the bottom-right.

	#end
	Field Handle:=New Vec2f( .5, .5 )
	#rem monkeydoc @hidden
	#end
	Field LoaderData:Config
	#rem monkeydoc Properties.
	#end
	Field Properties:Config
	#rem monkeydoc Rotation.
	#end
	Field Rotation:=0.0
	#rem monkeydoc Scale.
	#end
	Field Scale:=New Vec2f( 1,1 )
	#rem monkeydoc Score collector.
	#end
	Field ScoreCollector:LayerObject
	#rem monkeydoc Score system.
	#end
	Field ScoreSystem:ScoreSystem
	#rem monkeydoc @hidden
	#end
	Field Selected:=False
	#rem monkeydoc Whether the layer object is tattooed.
	
	When tattooed is enabled, the layer object will not be removed when Remove() is used.
	
	#end
	Field Tattooed:=False
	#rem monkeydoc Whether the layer object is visible.
	#end
	Field Visible:=True
	#rem monkeydoc Location z.
	#end
	Field Z:=0.0

	Method New()
	End

	#rem monkeydoc Alpha level.
	#end	
	Property Alpha:Float()
		Return Color.a
	Setter( alpha:Float )
		Color.a=alpha
	End

	#rem monkeydoc @hidden
	#end
	Property BottomLeft:Vec2f()
		Return RotateAroundPoint( New Vec2f( X-Width*Handle.x,Y+Height*Handle.y ),_location,Rotation )
	End

	#rem monkeydoc @hidden
	#end
	Property BottomRight:Vec2f()
		Return RotateAroundPoint( New Vec2f( X+Width*Handle.x,Y+Height*Handle.y ),_location,Rotation )
	End

	#rem monkeydoc Whether collision read is enabled.
	#end
	Property CollisionRead:Bool()
	
		Return _collisionRead
	
	Setter( collisionRead:Bool )

		If Not collisionRead And _collisionRead
			_collisionRead._readers.RemoveEach( Self )
			_collisionRead=Null
			Return
		Endif

		If Not _layer Return
		If Not _layer._scene Return
		If Not _layer._scene.CollisionMonitor Return

		If collisionRead And _layer._scene.CollisionMonitor<>_collisionRead
			_layer._scene.CollisionMonitor._readers.Push( Self )
			_collisionRead=_layer._scene.CollisionMonitor
		Endif

	End

	#rem monkeydoc Whether collision write is enabled.
	#end
	Property CollisionWrite:Bool()
	
		Return _collisionWrite
	
	Setter( collisionWrite:Bool )

		If Not collisionWrite And _collisionWrite
			_collisionWrite._writers.RemoveEach( Self )
			_collisionWrite=Null
			Return
		Endif

		If Not _layer Return
		If Not _layer._scene Return
		If Not _layer._scene.CollisionMonitor Return

		If collisionWrite And _layer._scene.CollisionMonitor<>_collisionWrite
			_layer._scene.CollisionMonitor._writers.Push( Self )
			_collisionWrite=_layer._scene.CollisionMonitor
		Endif
	End

	#rem monkeydoc Returns all groups.
	#end
	Property Groups:LayerGroup[]()

		Local group:=Group

		Local groups:LayerGroup[]

		While group
			Local length:=groups.Length
			groups=ResizeArray( groups,length+1 )
			groups[length]=group
			group=group.Group
		Wend

		Return groups

	End

	#rem monkeydoc Group.
	#end
	Property Group:LayerGroup()

		Return _group

	Setter( group:LayerGroup )

		If _layer<>Null Layer=Null

		If _group _group.Members.RemoveEach( Self )
		_group=group
		If _group _group.Members.Push( Self )

	End

	Property Groupify:String()
		Return _groupify
	Setter( group:String )
		_groupify=group
	End

	#rem monkeydoc Handle x.
	#end
	Property HandleX:Float()
		Return Handle.x
	Setter( handleX:Float )
		Handle.x=handleX
	End

	#rem monkeydoc Handle y.
	#end
	Property HandleY:Float()
		Return Handle.y
	Setter( handleY:Float )
		Handle.y=handleY
	End

	#rem monkeydoc Height.
	#end
	Property Height:Float() Virtual
		Return _height
	Setter( height:Float ) Virtual
		_height=height
	End

	#rem monkeydoc Layer.
	#end
	Property Layer:Layer() Virtual

		If _group<>Null
			Local groups:=Groups
			Return groups[groups.Length-1]._layer
		Endif

		Return _layer

	Setter( layer:Layer ) Virtual

		If _layer<>Null And _block<>Null
			_block.RemoveEach( Self )
			_block=Null
		Endif
		
		If _group<>Null Group=Null

		_layer=layer

		__UpdateLocation()

	End

	#rem monkeydoc Location.
	#end
	Property Location:Vec2f() Virtual
		Return _location
	Setter( location:Vec2f ) Virtual
		_location=location
		__UpdateLocation()
	End
#rem
	Property MouseLocation:Vec2i( camera:Camera )
		Return camera._scene._view.MouseLocation
	End
#end

	#rem monkeydoc @hidden
	#end
	Property Root:LayerGroup()

		Local group:=Group
		Local root:LayerGroup

		While group
			root=group
			group=group.Group
		Wend

		Return root

	End

	#rem monkeydoc Scale x.
	#end
	Property ScaleX:Float()
		Return Scale.x
	Setter( scaleX:Float )
		Scale.x=scaleX
	End

	#rem monkeydoc Scale y.
	#end
	Property ScaleY:Float()
		Return Scale.y
	Setter( scaleY:Float )
		Scale.y=scaleY
	End

	#rem monkeydoc Scaled height.
	#end
	Property ScaledHeight:Float()
		Return Height*Scale.y
	End

	#rem monkeydoc Scaled width.
	#end
	Property ScaledWidth:Float()
		Return Width*Scale.x
	End

	#rem monkeydoc @hidden
	#end
	Property TopLeft:Vec2f()
		Return RotateAroundPoint( New Vec2f( X-Width*Handle.x,Y-Height*Handle.y ),_location,Rotation )
	End

	#rem monkeydoc @hidden
	#end
	Property TopRight:Vec2f()
		Return RotateAroundPoint( New Vec2f( X+Width*Handle.x,Y-Height*Handle.y ),_location,Rotation )
	End

	#rem monkeydoc @hidden
	#end
	Property UniqueIdentifier:bool()
	
		For Local i:=0 Until Layer.Scene.TaskManager.Tasks.Length
			If Layer.Scene.TaskManager.Tasks.Get( i ).Identifier=Identifier Return False
		Next
	
		Return True
	
	End

	#rem monkeydoc View.
	#end
	Property View:View()
		If _layer=Null Return Null
		If _layer._scene=Null Return Null
		Return _layer._scene.View
	End

	#rem monkeydoc Width.
	#end
	Property Width:Float() Virtual
		Return _width
	Setter( width:Float ) Virtual
		_width=width
	End

	#rem monkeydoc World scale x.
	#end
	Property WorldScaleX:Float()

		If Group=Null Return Scale.x

		_UpdateWorldData()
		
		Return _worldScale.x

	End

	#rem monkeydoc World scale y.
	#end
	Property WorldScaleY:Float()

		If Group=Null Return Scale.y

		_UpdateWorldData()
		
		Return _worldScale.y

	End

	#rem monkeydoc World height.
	#end
	Property WorldHeight:Float()

		If Group=Null Return ScaledHeight
		
		_UpdateWorldData()
		
		Return _worldHeight

	End

	#rem monkeydoc World rotation.
	#end
	Property WorldRotation:Float()

		If Group=Null Return Rotation
		
		_UpdateWorldData()
		
		Return _worldRotation

	End

	#rem monkeydoc World width.
	#end
	Property WorldWidth:Float()

		If Group=Null Return ScaledWidth

		_UpdateWorldData()
		
		Return _worldWidth
	
	End

	#rem monkeydoc World location x.
	#end
	Property WorldX:Float()

		If Group=Null Return _location.x

		_UpdateWorldData()
		
		Return _worldLocation.x

	End

	#rem monkeydoc World location y.
	#end
	Property WorldY:Float()

		If Group=Null Return _location.y

		_UpdateWorldData()
		
		Return _worldLocation.y

	End

	#rem monkeydoc Location x.
	#end
	Property X:Float() Virtual
		Return _location.x
	Setter( x:Float ) Virtual
		_location.x=x
		__UpdateLocation()
	End

	#rem monkeydoc Location y.
	#end
	Property Y:Float() Virtual
		Return _location.y
	Setter( y:Float ) Virtual
		_location.y=y
		__UpdateLocation()
	End

	#rem monkeydoc @hidden
	#end
	Method Brightness:Color( color:Color )

		If _group=Null
			If _layer<>Null And _layer.Color<>Null color*=_layer.Color
			Return color
		Endif

		Local group:=Group

		While group
			color*=group.Color
			group=group.Group
		Wend

		Local layer:=Layer

		If layer<>Null And layer.Color<>Null color*=layer.Color

		Return color

	End

	#rem monkeydoc Collision method.
	#end
	Method CollisionMethod:Bool( layerObject:LayerObject ) Virtual
		Return Collision.Rectangles( X,Y,ScaledWidth,ScaledHeight,layerObject.X,layerObject.Y,layerObject.ScaledWidth,layerObject.ScaledHeight,True )
	End

	Method Draw( canvas:Canvas ) Virtual
		If Flags<>-1 Or Properties<>Null __DrawFlags( canvas )
		If Selected __DrawSelected( canvas )
	End

	Method GetLayerObject:LayerObject( layer:Layer,x:Int,y:Int )

		If layer=Null Return Null

		Local objects:=layer.GetLayerObjects( X+x,Y+y,True )
		If objects.Length=0 Return Null
		If objects.Length=1 And objects.Get( 0 )<>Self And objects.Get( 0 ).Enabled=True And objects.Get( 0 ).Visible=True Return objects.Get( 0 )

		Local i:=objects.Length-1
		While i>=0
			If objects.Get( i )<>Self And objects.Get( i ).Enabled=True And objects.Get( i ).Visible=True Return objects.Get( i )
			i-=1
		Wend

		Return Null

	End

	Method GetLayerObject:LayerObject( x:Int,y:Int )
		Return GetLayerObject( Layer,x,y )
	End

	Method GetLayerObject:LayerObject( location:Vec2i )
		Return GetLayerObject( Layer,location.x,location.y )
	End

	Method GetLayerObject:LayerObject( layer:Layer,location:Vec2i )
		Return GetLayerObject( layer,location.x,location.y )
	End

	Method GetLayerObject:LayerObject( layer:Layer[],x:Int[],y:Int[],identifiers:Int[] )

		For Local i:=0 Until layer.Length
	
			For Local h:=0 Until x.Length
				For Local v:=0 Until y.Length
					Local obj:=GetLayerObject( layer[i],x[h],y[v] )
					If obj<>Null And obj.Enabled=True And obj.Visible=True And Compare( Int( obj.Identifier ),identifiers )=True Return obj
				Next
			Next

		Next

		Return Null
	
	End

	Method GetLayerObject:LayerObject( layer:Layer[],x:Int[],y:Int[],identifiers:String[] )
	
		For Local i:=0 Until layer.Length

			For Local h:=0 Until x.Length
				For Local v:=0 Until y.Length
					Local obj:=GetLayerObject( layer[i],x[h],y[v] )
					If obj<>Null And obj.Enabled=True And obj.Visible=True And Compare( obj.Identifier,identifiers )=True Return obj
				Next
			Next

		Next

		Return Null
	
	End

	#rem monkeydoc Mouse location relative to the camera.
	#end
	Method GetMouseLocation:Vec2f( camera:Camera )
		Return Layer.GetMouseLocation( camera )
	End

	#rem monkeydoc Get object by name.
	#end
	Method GetObject:Object( name:String ) Virtual
		If Name=name Return Self
		Return Null
	End

	#rem monkeydoc @hidden
	#end
	Method GetSyncronizer:LayerSprite()
		
		For Local i:=0 Until Layer.Scene.TaskManager.Tasks.Length
			If Layer.Scene.TaskManager.Tasks.Get( i ).Identifier=Identifier
				Return Cast<LayerSprite>( Layer.Scene.TaskManager.Tasks.Get( i ) )
			Endif
		Next
	
		Return Null
	
	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseDown:Bool( mouseButton:MouseButton )
		If Layer=Null Return False
		If Layer.Scene=Null Return False
		Return Layer.Scene.InputDevice.ButtonDown[ mouseButton ]
	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseHit:Bool( mouseButton:MouseButton )
		If Layer=Null Return False
		If Layer.Scene=Null Return False
		Return Layer.Scene.InputDevice.ButtonHit[ mouseButton ]
	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseReleased:Bool( mouseButton:MouseButton )
		If Layer=Null Return False
		If Layer.Scene=Null Return False
		Return Layer.Scene.InputDevice.ButtonReleased[ mouseButton ]
	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseDown:Bool( camera:Camera,mouseButton:MouseButton )
		If Layer=Null Return False
		If Layer.Scene=Null Return False
		Return Layer.Scene.InputDevice.ButtonDown[ mouseButton ] And PointInside( camera,Mouse.X,Mouse.Y )
	End
	
	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseHit:Bool( camera:Camera,mouseButton:MouseButton )
		If Layer=Null Return False
		If Layer.Scene=Null Return False
		Return Layer.Scene.InputDevice.ButtonHit[ mouseButton ] And PointInside( camera,Mouse.X,Mouse.Y )
	End

	#rem monkeydoc Checks the mouse button was released.
	#end
	Method MouseReleased:Bool( camera:Camera,mouseButton:MouseButton )
		If Layer=Null Return False
		If Layer.Scene=Null Return False
		Return Layer.Scene.InputDevice.ButtonReleased[ mouseButton ] And PointInside( camera,Mouse.X,Mouse.Y )
	End
	
	#rem monkeydoc Checks the current hover state of the mouse.
	#end
	Method MouseHover:Bool( camera:Camera )
		Return PointInside( camera,Mouse.X,Mouse.Y )
	End
	
	#rem monkeydoc Called when a collision occurs.

	Only collision 'readers' call OnCollision!

	#end
	Method OnCollision( layerObject:LayerObject ) Virtual
	End

	Method OnClearLayer() Virtual

		If Tattooed=True Return

		If TaskManager<>Null TaskManager=Null

		If _layer<>Null Layer=Null
		If _group<>Null Group=Null

		If _collisionRead=True CollisionRead=False
		If _collisionWrite=True CollisionWrite=False

		Enabled=False

	End

	Method OnOutro( layerObject:LayerObject ) Virtual
	End

	#rem monkeydoc Whether the point is inside the layer object.
	#end
	Method PointInside:Bool( camera:Camera,x:Float,y:Float ) Virtual

		x+=camera.Offset.x
		y+=camera.Offset.y

		Local x1:=x/camera.Scale.x
		Local y1:=y/camera.Scale.y

		Local x2:=camera.ZoomPoint.x
		Local y2:=camera.ZoomPoint.y

		x1-=camera.Viewport.min.x/camera.Scale.x
		y1-=camera.Viewport.min.y/camera.Scale.y
	
		x2-=( camera.ZoomPoint.x-camera.RotationPoint.x )
		y2-=( camera.ZoomPoint.y-camera.RotationPoint.y )

		Local angle:=ATan2( y1-y2,x1-x2 )+camera.Rotation*RotationMode
		Local distance:=Sqrt( Pow( y1-y2,2 )+Pow( x1-x2,2 ) )

		x2+=Cos( angle )*distance
		y2+=Sin( angle )*distance

		x2/=camera.Zoom
		x2+=camera.ZoomPoint.x-camera.ZoomPoint.x/camera.Zoom
		x2+=camera.Location.x*Layer.Multiplier.x
		x2-=Layer.Location.x

		y2/=camera.Zoom
		y2+=camera.ZoomPoint.y-camera.ZoomPoint.y/camera.Zoom
		y2+=camera.Location.y*Layer.Multiplier.y
		y2-=Layer.Location.y	

		Return PointInsideRect( WorldX,WorldY,WorldWidth,WorldHeight,x2,y2,WorldRotation*RotationMode,Handle.x,Handle.y )

	End

	#rem monkeydoc Whether the point is inside the layer object.
	#end
	Method PointInside:Bool( camera:Camera,location:Vec2f ) Virtual
		Return PointInside( camera,location.x,location.y )
	End
	
	Method PostBuild() Virtual
	End

	#rem monkeydoc Remove layer object.

	Object will not be removed when Tattooed is True.

	#end
	Method Remove() Virtual

		If Tattooed=True Return

		If TaskManager TaskManager=Null

		If _layer Layer=Null
		If _group Group=Null

		If _collisionRead=True CollisionRead=False
		If _collisionWrite=True CollisionWrite=False

		Enabled=False

	End

	#rem monkeydoc Update score system.

	Call this method from [[OnCollision]].

	#end
	Method UpdateScoreSystem( layerObject:LayerObject )

		If layerObject=Null Return

		If layerObject.ScoreSystem=Null Return

		If ScoreSystem=Null Return

		Local a:=ScoreSystem.Stamina
		Local b:=layerObject.ScoreSystem.Stamina

		If layerObject.Layer<>Null And ScoreSystem.Invincible=False
			ScoreSystem.Stamina-=b
			ScoreSystem.Hits+=1
		Endif

		If Layer<>Null And layerObject.ScoreSystem.Invincible=False
			layerObject.ScoreSystem.Stamina-=a
			layerObject.ScoreSystem.Hits+=1
		Endif
		
		If ScoreSystem.Stamina<1

			ScoreSystem.Stamina=0

			If layerObject.ScoreCollector<>Null
				layerObject.ScoreCollector.ScoreSystem.TotalScore+=ScoreSystem.Reward
			Else
				layerObject.ScoreSystem.TotalScore+=ScoreSystem.Reward
			Endif

			OnOutro( layerObject )
			Remove()

		Endif
		
		If layerObject.ScoreSystem.Stamina<1

			layerObject.ScoreSystem.Stamina=0

			If ScoreCollector<>Null
				If ScoreCollector.ScoreSystem ScoreCollector.ScoreSystem.TotalScore+=layerObject.ScoreSystem.Reward
			Else
				ScoreSystem.TotalScore+=layerObject.ScoreSystem.Reward
			Endif

			layerObject.OnOutro( Self )
			layerObject.Remove()

		Endif

	End

	#rem monkeydoc @hidden
	#end
	Method __DrawFlags( canvas:Canvas )

		Local layer:=Layer
		local text:=""
		
		If layer And layer.DrawFlags=False Return
		If layer And layer._scene And layer._scene.DrawFlags=False Return

		canvas.BlendMode=BlendMode.Alpha

		If Flags<>-1
			canvas.Color=DebugGui.FlagColors[Flags]
			text+=Flags
		Endif

		If Properties<>Null
			If text<>"" text+="/"
			text+="P"
		Endif

		canvas.PushMatrix()

		canvas.Translate( X,Y )
		'canvas.Rotate( _rotation*RotationMode )

		DrawRect( canvas,-Width*HandleX,-Height*HandleY,Width,Height,3 )

		canvas.DrawText( text,0,0,.5,.5 )

		canvas.PopMatrix()

	End

	#rem monkeydoc @hidden
	#end
	Method __DrawSelected( canvas:Canvas )

		If EditorGui.SelectedType=0 Return

		Local layer:=Layer

		If layer And layer.DrawSelected=False Return

		canvas.BlendMode=EditorGui.SelectedBlendMode
		canvas.Color=EditorGui.SelectedColor

		canvas.PushMatrix()

		canvas.Translate( X,Y )
		canvas.Rotate( Rotation*RotationMode )

		Select EditorGui.SelectedType
			Case 1
				canvas.DrawRect( -Width*HandleX,-Height*HandleY,Width,Height )
			Case 2
				DrawRect( canvas,-Width*HandleX,-Height*HandleY,Width,Height )
		End Select

		canvas.PopMatrix()
#rem
		' Debug points for tesing editor (remove later)
		canvas.DrawEllipse TopLeft[0],TopLeft[1],8,8
		canvas.DrawEllipse TopRight[0],TopRight[1],8,8
		canvas.DrawEllipse BottomLeft[0],BottomLeft[1],8,8
		canvas.DrawEllipse BottomRight[0],BottomRight[1],8,8
#end

	End

	#rem monkeydoc @hidden
	#end
	Method __UpdateLocation() Virtual

		If _layer=Null Return
	
		Local x:=Int( _location.x )
		Local y:=Int( _location.y )

		Local location:=_layer.__Expand( x,y )

		If location<>_block

			If _block<>Null _block.RemoveEach( Self )

			location.Push( Self )
	
			_block=location

		Endif

	End

	Private

	Method _UpdateWorldData()

		_worldLocation.x=_location.x
		_worldLocation.y=_location.y
		_worldRotation=Rotation
		_worldScale.x=Scale.x
		_worldScale.y=Scale.y
		_worldWidth=ScaledWidth
		_worldHeight=ScaledHeight
	
		Local group:=Group

		While group

			_worldLocation.x-=group.Camera.x
			_worldLocation.y-=group.Camera.y

			_worldRotation-=group.Rotation*RotationMode
			_worldWidth*=group.Scale.x
			_worldHeight*=group.Scale.y

			_worldLocation.x*=group.Scale.x
			_worldLocation.y*=group.Scale.y

			_worldLocation.x-=group.ScaledWidth*group.Handle.x
			_worldLocation.y-=group.ScaledHeight*group.Handle.y

			_worldLocation.x+=group.X
			_worldLocation.y+=group.Y

			_worldScale.x*=group.ScaleX
			_worldScale.y*=group.ScaleY

			Local angle:=ATan2( _worldLocation.y-group._location.y,_worldLocation.x-group._location.x )-group.Rotation*RotationMode
			Local distance:=Sqrt( Pow( _worldLocation.y-group._location.y,2 )+Pow( _worldLocation.x-group._location.x,2 ) )

			_worldLocation.x=group._location.x+Cos( angle )*distance
			_worldLocation.y=group._location.y+Sin( angle )*distance

			group=group.Group

		Wend

	End

	Field _block:Stack<LayerObject>
	Field _collisionRead:CollisionMonitor
	Field _collisionWrite:CollisionMonitor
	Field _group:LayerGroup
	Field _groupify:=""
	Field _height:=0.0
	Field _layer:Layer
	Field _location:=New Vec2f
	Field _width:=0.0
	Field _worldHeight:=0.0
	Field _worldLocation:=New Vec2f
	Field _worldRotation:=0.0
	Field _worldScale:=New Vec2f
	Field _worldWidth:=0.0

End

#rem monkeydoc The LayerPolygon class.
#end
Class LayerPolygon Extends LayerObject

	#rem monkeydoc Orientation
	#end
	Field Orientation:=0
	#rem monkeydoc Type
	
		Type 1 = Polygon

		Type 2 = Polyline
	
	#end
	Field Type:=1

	Method New( layer:Layer )
		Layer=layer
	End

	Method New( layer:Layer,vertices:Float[],type:Int=1 )
		Layer=layer
		Vertices=vertices
		Type=type
	End

	Property Vertices:Float[]()

		' Dirty solution: Returns floats but are stored and handled as Vec2f so returning vertices is slow. Floats all the way? Fix later!

		Local vertices:=New Float[_vertices.Length*2]
		Local i1:=0

		For Local i2:=0 Until vertices.Length Step 2
			vertices[i2]=_vertices[i1].x
			vertices[i2+1]=_vertices[i1].y
			i1+=1
		Next

		Return vertices

	Setter( vertices:Float[] )

''		_vertices=ResizeArray( _vertices,vertices.Length/2 )
		_vertices=New Vec2f[vertices.Length/2]

		Local c:=0
		Local i:=0

		Local x1:=0
		Local x2:=0
		Local y1:=0
		Local y2:=0

		While i<_vertices.Length

			_vertices[i]=New Vec2f( vertices[c],vertices[c+1] )

			If vertices[c]<x1 x1=vertices[c]
			If vertices[c]>x2 x2=vertices[c]
			If vertices[c]<y1 y1=vertices[c+1]
			If vertices[c]>y2 y2=vertices[c+1]

			c+=2
			i+=1

		Wend

		_width=Abs( x1 )+Abs( x2 )
		_height=Abs( y1 )+Abs( y2 )

	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		Select Type

			Case 1
				_DrawPoly( canvas )

			Case 2
				_DrawPolyLine( canvas )

		End Select

	End

	Private

	Method _DrawPoly( canvas:Canvas )

		For Local i:=0 Until _vertices.Length
			
			Select Orientation

				Case 0
	
					canvas.PushMatrix()
	
					canvas.Translate( _location.x,_location.y )
					canvas.Rotate( Rotation*RotationMode )
					canvas.Scale( Scale.x,Scale.y )
					canvas.Translate( -Handle.x*_width,-Handle.y*_height )
	
					canvas.BlendMode=BlendMode
					canvas.Color=Brightness( Color )
	
					If i=_vertices.Length-1
						canvas.DrawLine( _vertices[i].x,_vertices[i].y,_vertices[0].x,_vertices[0].y )
					Else
						canvas.DrawLine( _vertices[i].x,_vertices[i].y,_vertices[i+1].x,_vertices[i+1].y )
					Endif		
	
					canvas.PopMatrix()

				Case 1
	
					canvas.PushMatrix()
	
					canvas.Translate( OrthoToIsoX( _location.x,_location.y ),OrthoToIsoY( _location.x,_location.y ) )
					canvas.Rotate( Rotation*RotationMode )
					canvas.Scale( Scale.x,Scale.y )
	
					canvas.BlendMode=BlendMode
					canvas.Color=Brightness( Color )
	
					If i=_vertices.Length-1
						canvas.DrawLine( OrthoToIsoX( _vertices[i].x,_vertices[i].y ),OrthoToIsoY( _vertices[i].x,_vertices[i].y ),OrthoToIsoX( _vertices[0].x,_vertices[0].y ),OrthoToIsoY( _vertices[0].x,_vertices[0].y ) )
					Else
						canvas.DrawLine( OrthoToIsoX( _vertices[i].x,_vertices[i].y ),OrthoToIsoY( _vertices[i].x,_vertices[i].y ),OrthoToIsoX( _vertices[i+1].x,_vertices[i+1].y ),OrthoToIsoY( _vertices[i+1].x, _vertices[i+1].y ) )
					Endif		
	
					canvas.PopMatrix()
	
			End Select

		Next

	End

	Method _DrawPolyLine( canvas:Canvas )
		
		For Local i:=0 Until _vertices.Length-1
			
			Select Orientation

				Case 0
	
					canvas.PushMatrix()
	
					canvas.Translate( _location.x,_location.y )
					canvas.Rotate( Rotation*RotationMode )
					canvas.Scale( Scale.x,Scale.y )
					canvas.Translate( -Handle.x*_width,-Handle.y*_height )
	
					canvas.BlendMode=BlendMode
					canvas.Color=Brightness( Color )
	
					canvas.DrawLine( _vertices[i].x,_vertices[i].y,_vertices[i + 1].x,_vertices[i + 1].y )
	
					canvas.PopMatrix()

				Case 1
	
					canvas.PushMatrix()
	
					canvas.Translate( OrthoToIsoX( _location.x,_location.y ),OrthoToIsoY( _location.x,_location.y ) )
					canvas.Rotate( Rotation*RotationMode )
					canvas.Scale( Scale.x,Scale.y )
					canvas.Translate( Handle.x*_width,Handle.y*_height )
	
					canvas.BlendMode=BlendMode
					canvas.Color=Brightness( Color )
	
					canvas.DrawLine( OrthoToIsoX( _vertices[i].x,_vertices[i].y ),OrthoToIsoY( _vertices[i].x,_vertices[i].y ),OrthoToIsoX( _vertices[i+1].x,_vertices[i+1].y ),OrthoToIsoY( _vertices[i+1].x, _vertices[i+1].y ) )
	
					canvas.PopMatrix()
	
			End Select
		
		Next
		
	End
	
	Field _vertices:Vec2f[]

End

#rem monkeydoc The LayerRectangle class.
#end
Class LayerRectangle Extends LayerObject

	#rem monkeydoc Orientation.
	#end
	Field Orientation:=0
	#rem monkeydoc Line width
	
	If LineWidth = 0 the rectangle is filled.
	
	#end
	Field LineWidth:=1

	Method New()
	End
	
	Method New( layer:Layer )
		Layer=layer
	End

	Method New( layer:Layer,width:Float,height:Float )
		Layer=layer
		_height=height
		_width=width
	End

	Method New( width:Float,height:Float )
		_height=height
		_width=width
	End
	
	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		Select Orientation

			Case 0
	
				canvas.PushMatrix()
		
				canvas.Translate( _location.x,_location.y )
				canvas.Rotate( Rotation*RotationMode )
				canvas.Scale( Scale.x,Scale.y )
				canvas.Translate( -Handle.x*_width,-Handle.y*_height )
		
				canvas.BlendMode=BlendMode
				canvas.Color=Brightness( Color )
	
				If LineWidth=0
					canvas.DrawRect( 0,0,_width,_height )
				Else
					canvas.DrawRect( 0,0,_width,LineWidth )
					canvas.DrawRect( 0,LineWidth,LineWidth,_height-( LineWidth*2 ) )
					canvas.DrawRect( 0,-LineWidth+_height,_width,LineWidth )
					canvas.DrawRect( -LineWidth+_width,LineWidth,LineWidth,_height-( LineWidth*2 ) )
				Endif
		
				canvas.PopMatrix()
	
			Case 1
	
				canvas.PushMatrix()

				canvas.Translate( OrthoToIsoX( _location.x,_location.y ),OrthoToIsoY( _location.x,_location.y ) )
				canvas.Rotate( Rotation*RotationMode )
				canvas.Scale( Scale.x,Scale.y )
				canvas.Translate( -Handle.x*_width,-Handle.y*_height )
	
				canvas.BlendMode=BlendMode
				canvas.Color=Brightness( Color )
	
				canvas.DrawLine( OrthoToIsoX( 0,0 ),OrthoToIsoY( 0,0 ),OrthoToIsoX( _width,0 ),OrthoToIsoY( _width,0 ) )
				canvas.DrawLine( OrthoToIsoX( _width,0 ),OrthoToIsoY( _width,0 ),OrthoToIsoX( _width,_height ),OrthoToIsoY( _width,_height ) )
				canvas.DrawLine( OrthoToIsoX( _width,_height ),OrthoToIsoY( _width,_height ),OrthoToIsoX( 0,_height ),OrthoToIsoY( 0,_height ) )
				canvas.DrawLine( OrthoToIsoX( 0,_height ),OrthoToIsoY( 0,_height ),OrthoToIsoX( 0,0 ), OrthoToIsoY( 0,0 ) )
	
				canvas.PopMatrix()
	
		End Select

	End

End

#rem monkeydoc The LayerSprite class.
#end
Class LayerSprite Extends LayerObject

	Field Frame:=0.0
	#rem monkeydoc @hidden
	#end
	Field FrameTime:Int[]
	#rem monkeydoc @hidden
	#end
	Field FrameTimer:=-1
	Field SyncMaster:LayerSprite
	#rem monkeydoc LayerSprite type.
	
		Type 1 = Normal sprite.

		Type 2 = Lighting mode.
	
	#end
	Field Type:=1

	Method New()
	End

	Method New( layer:Layer,image:Image )
		Layer=layer
		Image=image
	End

	Method New( layer:Layer,images:Image[] )
		Layer=layer
		Images=images
	End

	Method New( group:LayerGroup,image:Image )
		Group=group
		Image=image
	End

	Method New( group:LayerGroup,images:Image[] )
		Group=group
		Images=images
	End

	#rem monkeydoc @hidden
	#end
	Property FlippedX:Int()
		Return _flippedX
	Setter( flippedX:Int )
		If flippedX<0 _flippedX=-1
		If flippedX>0 _flippedX=1
	End

	#rem monkeydoc @hidden
	#end
	Property FlippedY:Int()
		Return _flippedY
	Setter( flippedY:Int )
		If flippedY<0 _flippedY=-1
		If flippedY>0 _flippedY=1
	End

	#rem monkeydoc Number of image frames.
	#end
	Property Frames:Int()
		Return _image.Length
	End

	#rem monkeydoc Height.
	#end
	Property Height:Float() Override
		Return Image.Height
	End

	#rem monkeydoc Current image.
	#end
	Property Image:Image()
		If _image=Null Return NullImage()
		Return NullImage( _image[ ( Round( Frame ) Mod _image.Length ) ] )
	Setter ( image:Image )
		_image=ResizeArray( _image,1 )
		_image[0]=image
	End

	#rem monkeydoc Current images.
	#end
	Property Images:Image[]()
		Return _image
	Setter( images:Image[] )
		_image=images
	End

	#rem monkeydoc last frame.
	#end
	Property LastFrame:Int()
		Return _image.Length-1
	End

	#rem monkeydoc Width.
	#end
	Property Width:Float() Override
		Return Image.Width
	End

	#rem monkeydoc @hidden
	#end
	Method Animate()

		If SyncMaster<>Null

			Frame=SyncMaster.Frame

		Else

			If _layer=Null Return
			If _layer._scene=Null Return
			If FrameTime=Null Return
			If FrameTime.Length=0 Return
			If FrameTime.Length<>_image.Length Return
			If _image=Null Return
			If _image.Length=0 Return

			Local timer:=_layer._scene._animationTimer
		
			If timer<( FrameTimer+FrameTime[Frame] ) Return
			
			Frame+=1
			If Frame>LastFrame Frame=0

			FrameTimer=timer

		Endif

	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		Select Type
		
			Case 1
	
				''canvas.PushMatrix()
		
				canvas.BlendMode=BlendMode
				canvas.Color=Color
		
				DrawImage( canvas,Image,_location.x,_location.y,-Rotation,Scale.x*_flippedX,Scale.y*_flippedY,Handle.x,Handle.y )
		
				''canvas.PopMatrix()

			Case 2

				Local layer:=Layer

				If layer<>Null And layer._scene<>Null And layer.AmbientLight<>Null

					canvas.Color=Color
	
					Local handle:=Image.Handle
					Local scale:=Image.Scale
					Image.Scale=Scale
					Image.Handle=Handle
					canvas.AddLight( Image,_location.x,_location.y )
					Image.Handle=handle
					Image.Scale=scale
				
				Endif
				
		End Select

		If Flags<>-1 Or Properties<>Null __DrawFlags( canvas )

		If Selected __DrawSelected( canvas )
		
	End

	#rem monkeydoc @hidden
	#end
	Method Flip( flippedX:Int,flippedY:Int,flippedXY:Int )

		If flippedXY=-1

			If flippedXY=-1 And flippedX=-1 And flippedY=-1
				Rotation+=( 90/360.0*TwoPi )
				_flippedX=-1
			Else If flippedXY=-1 And flippedX=-1
				Rotation+=( 90/360.0*TwoPi )
			Else If flippedXY=-1 And flippedY=-1
				Rotation-=( 90/360.0*TwoPi )
			Else If flippedXY=-1
				Rotation-=( 90/360.0*TwoPi )
				_flippedX=-1
			Endif
		
		Else

			If flippedX=-1 _flippedX=-1
			If flippedY=-1 _flippedY=-1

		Endif

	End

	Method OnUpdate() Override
		Animate()
	End
	
	Private

	Field _flippedX:=1
	Field _flippedY:=1
	Field _image:Image[]

End

#rem monkeydoc The Particle class.
#end
Class Particle Extends LayerSprite

	Field Acceleration:Vec2f
	Field Delay:=0
	Field Drag:Vec2f
	Field Gravity:=0.0
	Field Spin:=0.0
	Field StartColor:Color
	Field StartScale:Vec2f
	Field TargetColor:=New Color( 1,1,1,0 )
	Field TargetScale:=New Vec2f( 1,1 )
	Field TimeToLive:=1000.0
	Field Velocity:Vec2f

	Method New()
	End

	Method New( layer:Layer,image:Image )
		Super.New( layer,image )
	End

	Method New( layer:Layer,images:Image[] )
		Super.New( layer,images )
	End

	Property Enabled:Bool() Override
		Return Super.Enabled
	Setter( enabled:Bool ) Override
		_initialized=False
		StartTime=Milliseconds()
		Super.Enabled=enabled
	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		If Milliseconds()>StartTime+Delay
			Super.Draw( canvas )
		Endif

	End

	#rem monkeydoc @hidden
	#end
	Method OnUpdate() Override

		If Delay>0
			StartTime+=Delay
			Delay=0
			Return
		Endif

		_Init()

		_UpdatePhysics()

		If Color<>TargetColor _UpdateColor()

		If Scale<>TargetScale _UpdateScale()

		If TimeToLive<>0 And Milliseconds()>StartTime+TimeToLive Remove()

	End

	Private

	Method _Init()

		If _initialized=True Return

		StartColor=Color
		StartScale=Scale
		
		_initialized=True

	End

	Method _UpdateColor()

		Color.r=StartColor.r+( Milliseconds()-StartTime )/TimeToLive*( TargetColor.r-StartColor.r )
		Color.g=StartColor.g+( Milliseconds()-StartTime )/TimeToLive*( TargetColor.g-StartColor.g )
		Color.b=StartColor.b+( Milliseconds()-StartTime )/TimeToLive*( TargetColor.b-StartColor.b )
		Color.a=StartColor.a+( Milliseconds()-StartTime )/TimeToLive*( TargetColor.a-StartColor.a )

		If Color.r<0 Color.r=0
		If Color.r>1 Color.r=1
		If Color.g<0 Color.g=0
		If Color.g>1 Color.g=1
		If Color.b<0 Color.b=0
		If Color.b>1 Color.b=1
		If Color.a<0 Color.a=0
		If Color.a>1 Color.a=1

	End

	Method _UpdatePhysics()

		If Velocity<>Null
			X+=Velocity.x
			Y+=Velocity.y
		Endif

		If Acceleration<>Null
			Velocity.x+=Acceleration.x
			Velocity.y+=Acceleration.y
		Endif
		
		If Drag<>Null
			If Drag.x<>0 Velocity.x*=Drag.x
			If Drag.y<>0 Velocity.y*=Drag.y
		Endif

		Velocity.y+=Gravity

		Rotation+=Spin

	End

	Method _UpdateScale()

		Scale.x=StartScale.x+( Milliseconds()-StartTime )/TimeToLive*( TargetScale.x-StartScale.x )
		Scale.y=StartScale.y+( Milliseconds()-StartTime )/TimeToLive*( TargetScale.y-StartScale.y )

	End

	Field _initialized:=False

End

#rem monkeydoc The Scene class.
#end
Class Scene

	Field CollisionMonitor:CollisionMonitor
	#rem monkeydoc @hidden
	#end
	Field DrawFlags:=False
	Field Enabled:=True
	Field Height:=0.0
	Field InputDevice:=New InputDevice
	Field Layers:=New Stack<Layer>
	Field Name:=""
	Field TaskManager:=New TaskManager
	Field View:View
	Field Width:=0.0

	Method New( view:View )
		View=view
	End

	#rem monkeydoc Remove all layer objects.
	#end
	Method ClearLayers()
		For Local i:=0 Until Layers.Length
			Layers.Get( i ).Clear()
		Next
	End

	#rem monkeydoc Draw scene.
	
	Call Draw from main game render loop.
	
	#end
	Method Draw( canvas:Canvas )

		If Enabled=False Return

		Local blendMode:=canvas.BlendMode
		Local color:=canvas.Color
		Local font:=canvas.Font

		canvas.PushMatrix()

		If _updateCounter=0 Return

		For Local i:=0 Until _cameras.Length
			If _cameras.Get( i ).Enabled=False Continue
			If _cameras.Get( i ).Visible=False Continue
			_cameras.Get( i )._Draw( canvas )
		Next

		canvas.PopMatrix()

		canvas.BlendMode=blendMode
		canvas.Color=color
		canvas.Font=font

	End

	#rem monkeydoc Get camera by name.
	#end
	Method GetCamera:Camera( name:String )
		If name="" Return Null
		For Local i:=0 Until _cameras.Length
			If _cameras.Get( i ).Name=name Return _cameras.Get( i )
		Next
		Return Null
	End

	#rem monkeydoc Get camera by index.
	#end
	Method GetCamera:Camera( index:Int )
		Return _cameras.Get( index )
	End

	#rem monkeydoc Get layer by name.
	#end
	Method GetLayer:Layer( name:String )
		If name="" Return Null
		For Local i:=0 Until Layers.Length
			If Layers.Get( i ).Name=name Return Layers.Get( i )
		Next
		Return Null
	End

	#rem monkeydoc Get layer by index.
	#end
	Method GetLayer:Layer( index:Int )
		Return Layers.Get( index )
	End

	#rem monkeydoc Get object by name.
	#end
	Method GetObject:Object( name:String ) Virtual
		For Local i:=0 Until Layers.Length
			Local search:=Layers.Get( i ).GetObject( name )
			If search Return search
		Next
		Return Null
	End

	#rem monkeydoc @hidden
	#end
	Method Groupify()
		For Local i:=0 Until Layers.Length
			Layers.Get( i ).Groupify()
		Next
	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseDown:Bool( mouseButton:MouseButton )
		Return InputDevice.ButtonDown[ mouseButton ]
	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseHit:Bool( mouseButton:MouseButton )
		Return InputDevice.ButtonHit[ mouseButton ]
	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseReleased:Bool( mouseButton:MouseButton )
		Return InputDevice.ButtonReleased[ mouseButton ]
	End

	#rem monkeydoc @hidden
	#end
	Method ShowTilemapCounts()
		For Local i:=0 Until Layers.Length
			Layers.Get( i ).ShowTilemapCounts()
		next
	End
	
	#rem monkeydoc Update scene.
	
	Call Update from main game update loop.
	
	#end
	Method Update()

		If Enabled=False Return

		InputDevice.Update()

		For Local i:=0 Until _cameras.Length
			If _cameras.Get( i ).Enabled=False Continue
'			If _cameras.Get( i ).Visible=False Continue
			_cameras.Get( i )._Update()
		Next

		TaskManager.Update( _updateCounter )

		If Frozen()=True Return
			
		If CollisionMonitor<>Null CollisionMonitor.Update()

		_animationTimer+=1000.0/50

		_updateCounter+=1

	End

	Private

	Field _animationTimer:=0.0
	Field _cameras:=New Stack<Camera>
'	Field _clearColor:=New Color( 0,0,0,1 )
	Field _updateCounter:=0

End

#rem monkeydoc The SceneLoader class.
#end
Class SceneLoader

	Global Content:=New ContentManager

	Field Data:Config[]
	Field FilePath:=""

	Method New()
	End

	Method New( filePath:String,scene:Scene )
		Load( filePath )
		Build( scene )
	End

	#rem monkeydoc @hidden
	#end
	Property Dir:String()
		Return ExtractDir( FilePath )
	End
	
	#rem monkeydoc @hidden
	#end
	Method AddLayerEllipse:LayerEllipse( scene:Scene,data:Config ) Virtual

		Local handleX:=data.ReadFloat( "handleX" )
		Local handleY:=data.ReadFloat( "handleY" )
		Local height:=data.ReadFloat( "height" )
		Local layerName:=data.ReadString( "layer" )
		Local orientation:=data.ReadInt( "orientation" )
		Local rotation:=data.ReadInt( "rotation" )
		Local visible:=data.ReadBool( "visible",True )
		Local width:=data.ReadFloat( "width" )
		Local x:=data.ReadFloat( "x" )
		Local y:=data.ReadFloat( "y" )
		Local z:=data.ReadFloat( "z" )

		Local layer:Layer=scene.GetLayer( layerName )
		If layer=Null
			layer=New Layer( scene )
			layer.Name=layerName
		Endif

		Local ellipse:=New LayerEllipse()
		ellipse.Layer=layer
		ellipse.Orientation=orientation
		ellipse.Height=height
		ellipse.Width=width
		ellipse.Handle=New Vec2f( handleX,handleY )
		ellipse.Location=New Vec2f( x,y )
		ellipse.Rotation=rotation
		ellipse.Visible=visible

		Return ellipse

	End

	#rem monkeydoc @hidden
	#end
	Method AddLayer:Layer( scene:Scene,data:Config ) Virtual

		Local layer:=scene.GetLayer( data.ReadString( "name" ) )

		'If layer Return Null

		Local drawFlags:=data.ReadBool( "drawFlags",False )
		Local enabled:=data.ReadBool( "enabled",True )
		Local identifier:=data.ReadString( "identifier" )
		Local name:=data.ReadString( "name" )
		Local tileWidth:=data.ReadInt( "cellWidth",2048 )
		Local tileHeight:=data.ReadInt( "cellHeight",2048 )
		Local type:=data.ReadInt( "type",0 )
		Local visible:=data.ReadBool( "visible",True )

		If layer=Null
			layer=NewLayer( identifier )
			layer.Scene=scene
		Endif

		layer.TileSize=New Vec2i( tileWidth,tileHeight )
		layer.DrawFlags=drawFlags
		layer.Enabled=enabled
		layer.Identifier=identifier
		layer.Name=name
		layer.Type=type
		layer.Visible=visible

		Return layer

	End

	#rem monkeydoc @hidden
	#end
	Method AddLayerGroup:LayerGroup( scene:Scene,data:Config ) Virtual

		Local groupify:=data.ReadString( "groupify" )
		Local handleX:=data.ReadFloat( "handleX",.5 )
		Local handleY:=data.ReadFloat( "handleY",.5 )
		Local identifier:=data.ReadString( "identifier" )
		Local layerIdentifier:=data.ReadString( "layerIdentifier" )
		Local layerName:=data.ReadString( "layer" )
		Local name:=data.ReadString( "name" )
		Local x:=data.ReadFloat( "x" )
		Local y:=data.ReadFloat( "y" )
		Local z:=data.ReadFloat( "z" )

		Local layer:=scene.GetLayer( layerName )
		If layer=Null
			layer=NewLayer( layerIdentifier )
			layer.Scene=scene
			layer.Name=layerName
		Endif

		Local layerGroup:=NewLayerGroup( identifier )
		layerGroup.Groupify=groupify
		layerGroup.Handle=New Vec2f( handleX,handleY )
		layerGroup.Layer=layer
		layerGroup.Name=name
		layerGroup.X=x
		layerGroup.Y=y
		layerGroup.Z=z

		Return layerGroup

	End
	
	#rem monkeydoc @hidden
	#end
	Method AddLayerObject:LayerObject( scene:Scene,data:Config ) Virtual

		Local alpha:=data.ReadFloat( "alpha",1.0 )
		Local blendMode:=data.ReadInt( "blendmode",BlendMode.Alpha )
		Local flags:=data.ReadInt( "flags",-1 )
		Local height:=data.ReadFloat( "height" )
		Local identifier:=data.ReadString( "identifier" )
		Local layerName:=data.ReadString( "layer" )
		Local width:=data.ReadFloat( "width" )
		Local x:=data.ReadFloat( "x" )
		Local y:=data.ReadFloat( "y" )

		Local layer:=scene.GetLayer( layerName )
		If layer=Null
			layer=New Layer( scene )
			layer.Name=layerName
		Endif

		Local layerObject:=NewLayerObject( identifier )

		layerObject.Alpha=alpha
		layerObject.BlendMode=ToBlendMode( blendMode )
		layerObject.Flags=flags
		layerObject.Height=height
		layerObject.Identifier=identifier
		layerObject.Layer=layer
		layerObject.Location=New Vec2f( x,y )
		layerObject.Width=width

		Return layerObject

	End
	
	#rem monkeydoc @hidden
	#end
	Method AddLayerPolygon:LayerPolygon( scene:Scene,data:Config ) Virtual

		Local handleX:=data.ReadFloat( "handleX" )
		Local handleY:=data.ReadFloat( "handleY" )
		Local layerName:=data.ReadString( "layer" )
		Local orientation:=data.ReadInt( "orientation" )
		Local rotation:=data.ReadInt( "rotation" )
		Local type:=data.ReadInt( "type" )
		Local vertices:=data.ReadFloatData( "vertices" )
		Local visible:=data.ReadBool( "visible",True )
		Local x:=data.ReadFloat( "x" )
		Local y:=data.ReadFloat( "y" )
		Local z:=data.ReadFloat( "z" )

		Local layer:=scene.GetLayer( layerName )
		If layer=Null
			layer=New Layer( scene )
			layer.Name=layerName
		Endif

		Local polygon:=New LayerPolygon( layer )
		polygon.Type=type
		polygon.Orientation=orientation
		polygon.Vertices=vertices
		polygon.Handle=New Vec2f( handleX,handleY )
		polygon.Location=New Vec2f( x,y )
		polygon.Rotation=rotation
		polygon.Visible=visible

		Return polygon

	End

	#rem monkeydoc @hidden
	#end
	Method AddLayerRectangle:LayerRectangle( scene:Scene,data:Config ) Virtual

		Local handleX:=data.ReadFloat( "handleX" )
		Local handleY:=data.ReadFloat( "handleY" )
		Local height:=data.ReadFloat( "height" )
		Local identifier:=data.ReadString( "identifier" )
		Local layerName:=data.ReadString( "layer" )
		Local orientation:=data.ReadInt( "orientation" )
		Local rotation:=data.ReadInt( "rotation" )
		Local visible:=data.ReadBool( "visible",True )
		Local width:=data.ReadFloat( "width" )
		Local x:=data.ReadFloat( "x" )
		Local y:=data.ReadFloat( "y" )
		Local z:=data.ReadFloat( "z" )

		Local layer:Layer=scene.GetLayer( layerName )
		If layer=Null
			layer=New Layer( scene )
			layer.Name=layerName
		Endif

		Local rectangle:=NewLayerRectangle( identifier )
		rectangle.Layer=layer
		rectangle.Orientation=orientation
		rectangle.Height=height
		rectangle.Width=width
		rectangle.Handle=New Vec2f( handleX,handleY )
		rectangle.Identifier=identifier
		rectangle.Location=New Vec2f( x,y )
		rectangle.Rotation=rotation
		rectangle.Visible=visible

		Return rectangle

	End

	#rem monkeydoc @hidden
	#end
	Method AddLayerSprite:LayerSprite( scene:Scene,data:Config ) Virtual

		Local alpha:=data.ReadFloat( "alpha",1.0 )
		Local blendMode:=data.ReadInt( "blendmode",BlendMode.Alpha )
		Local enabled:=data.ReadBool( "enabled",True )
		Local filter:=data.ReadString( "filter" )
		Local flags:=data.ReadInt( "flags",-1 )
		Local flippedX:=data.ReadInt( "flippedX",1 )
		Local flippedY:=data.ReadInt( "flippedY",1 )
		Local flippedXY:=data.ReadInt( "flippedXY",1 )
		Local floating:=data.ReadBool( "floating",False )
		Local frame:=data.ReadFloat( "frame" )
		Local frameTime:=data.ReadIntData( "frameTime" )
		Local globalPropertiesPath:=data.ReadString( "globalPropertiesPath" )
		Local groupify:=data.ReadString( "groupify" )
		Local handleX:=data.ReadFloat( "handleX",.5 )
		Local handleY:=data.ReadFloat( "handleY",.5 )
		Local identifier:=data.ReadString( "identifier" )
		Local imagePath:=data.ReadStringData( "imagePath" )
		Local layerIdentifier:=data.ReadString( "layerIdentifier" )
		Local layerName:=data.ReadString( "layer" )
		Local name:=data.ReadString( "name" )
		Local properties:=data.ReadString( "properties" )
		Local rotation:=data.ReadInt( "rotation" )
		Local scaleX:=data.ReadFloat( "scaleX",1.0 )
		Local scaleY:=data.ReadFloat( "scaleY",1.0 )
		Local selected:=data.ReadBool( "selected",False )
'		Local taskManager:=data.ReadBool( "taskManager" )
		Local tattooed:=data.ReadBool( "tattooed",False )
		Local visible:=data.ReadBool( "visible",True )
		Local x:=data.ReadFloat( "x" )
		Local y:=data.ReadFloat( "y" )
		Local z:=data.ReadFloat( "z" )

		Local layer:=scene.GetLayer( layerName )
		If layer=Null
			layer=NewLayer( layerIdentifier )
			layer.Scene=scene
			layer.Name=layerName
		Endif

		Local sprite:=NewLayerSprite( identifier )
		sprite.Images=ResizeArray( sprite.Images,imagePath.Length )

		For Local i:=0 Until imagePath.Length
			sprite.Images[i]=LoadImage( imagePath[i] )
		Next

		If frameTime<>Null And frameTime.Length=imagePath.Length
			sprite.FrameTime=frameTime
			sprite.FrameTimer=layer._scene._animationTimer
		Endif

		sprite.Layer=layer
		sprite.Alpha=alpha
		sprite.BlendMode=ToBlendMode( blendMode )
		sprite.Enabled=enabled
		sprite.Frame=frame
		sprite.Floating=floating
		sprite.Groupify=groupify
		sprite.Handle=New Vec2f( handleX,handleY )
		sprite.Identifier=identifier
		sprite.X=x
		sprite.Y=y
		sprite.Name=name
		sprite.Rotation=rotation
		sprite.Scale=New Vec2f( scaleX,scaleY )
		sprite.Selected=selected
		sprite.Tattooed=tattooed
		sprite.Flags=flags
		sprite.Visible=visible

		sprite.Flip( flippedX,flippedY,flippedXY )

		If globalPropertiesPath sprite.GlobalProperties=LoadConfig( globalPropertiesPath )

		If properties sprite.Properties=GetProperties( properties )

		If frameTime<>Null And frameTime.Length=imagePath.Length

			sprite.TaskManager=layer.TaskManager

			''sprite.SyncMaster=GetSyncMaster( sprite )

		Endif

		sprite.LoaderData=data

		sprite.PostBuild()

		Return sprite
	
	End

	#rem monkeydoc @hidden
	#end
	Method AddFromTexturePacker( scene:Scene,data:Config ) Virtual

		Local dataPath:=data.ReadString( "dataPath" )
		Local imagePath:=data.ReadString( "imagePath" )

		Local sourceImage:=Image.Load( SmartPath( imagePath ) )
		Local textureData:=LoadString( SmartPath( dataPath ) )

		For Local l:=Eachin textureData.Split( "~n" )
			Local i:=l.Split( ":" )
			If i.Length=5
				Content.AddImage( New Image( sourceImage,New Recti( Int( i[1] ),Int( i[2] ),Int( i[1] )+Int( i[3] ),Int( i[2] )+Int( i[4] ) ) ),i[0] )
			Endif
		Next

	End

	#rem monkeydoc @hidden
	#end
	Method AddVirtualTileset:VirtualTileset( scene:Scene,data:Config ) Virtual

		Local handleX:=data.ReadFloat( "handleX",.5 )
		Local handleY:=data.ReadFloat( "handleY",.5 )
		Local imagePath:=data.ReadString( "imagePath" )
		Local margin:=data.ReadInt( "margin" )
		Local spacing:=data.ReadInt( "spacing" )
		Local tileHeight:=data.ReadInt( "tileHeight" )
		Local tileWidth:=data.ReadInt( "tileWidth" )

		Local tileset:=New VirtualTileset( imagePath,tileWidth,tileHeight,margin,spacing )

		Return tileset

	End
	
	#rem monkeydoc Build the scene.
	#end
	Method Build( scene:Scene )

		For Local i:=0 Until Data.Length

			Select Data[i].ReadString( "class" )

				Case "LayerSprite"

					AddLayerSprite( scene,Data[i] )
	
					Continue
	
				Case "LayerObject"
	
					AddLayerObject( scene,Data[i] )
	
					Continue

				Case "LayerEllipse"
	
					AddLayerEllipse( scene,Data[i] )
	
					Continue
	
				Case "LayerPolygon"
	
					AddLayerPolygon( scene,Data[i] )
	
					Continue
	
				Case "LayerRectangle"
	
					AddLayerRectangle( scene,Data[i] )
	
					Continue
	
				Case "LayerGroup"

					AddLayerGroup( scene,Data[i] )
	
					Continue
	
				Case "Layer"
	
					AddLayer( scene,Data[i] )
	
					Continue

				Case "VirtualTileset"
	
					AddVirtualTileset( scene,Data[i] )
					
					Continue
	
				Case "TexturePacker"
	
					AddFromTexturePacker( scene,Data[i] )
					
					Continue

				Default

					InitScene( scene,Data[i] )

			End Select

		Next

		''Synchronize( scene )

	End

	#rem monkeydoc Import data.
	#end
	Method ImportData( data:String[] )

		Data=New Config[data.Length]

		For Local i:=0 Until data.Length

			Data[i]=New Config
			Data[i].CreateData( data[i] )

			OnLoading( Data[i] )

		Next

	End Method

	Method ImportData( data:String )

		Local obj:=data.Replace( "~n~r","~n" ).Trim().Split( "~n~n" )

		ImportData( obj )

	End
	
	Method InitScene( scene:Scene,data:Config ) Virtual

		Local height:=data.ReadInt( "height" )
		Local name:=data.ReadString( "name" )
		Local width:=data.ReadInt( "width" )

		scene.Height=height
		scene.Name=name
		scene.Width=width

	End
	
	#rem monkeydoc @hidden
	#end
	Method LoadImage:Image( path:String,shader:Shader=Null ) Virtual
		Return Content.GetImage( SmartPath( path,Dir ),shader )
	End

	#rem monkeydoc @hidden
	#end
	Method LoadConfig:Config( path:String )
		Return Content.GetConfig( SmartPath( path,Dir ) )
	End

	#rem monkeydoc Load a scene from a file.
	#end
	Method Load( filePath:String ) Virtual

		filePath=filePath.Replace( "\","/" )

		FilePath=filePath

		ImportData( LoadString( SmartPath( filePath ) ) )
#rem
		Local obj:=LoadString( SmartPath( filePath ) ).Replace( "~n~r","~n" ).Trim().Split( "~n~n" )

		_data=New Config[obj.Length]

		For Local i:=0 Until _data.Length

			_data[i]=New Config
			_data[i].CreateData( obj[i] )

			OnLoading( _data[i] )

		Next
#end
	End

	#rem monkeydoc @hidden
	#end
	Method NewLayer:Layer( identifier:String ) Virtual
		Return New Layer
	End

	#rem monkeydoc @hidden
	#end
	Method NewLayerGroup:LayerGroup( identifier:String ) Virtual
		Return New LayerGroup
	End

	#rem monkeydoc @hidden
	#end
	Method NewLayerObject:LayerObject( identifier:String ) Virtual
		Return New LayerObject
	End

	#rem monkeydoc @hidden
	#end
	Method NewLayerRectangle:LayerRectangle( identifier:String ) Virtual
		Return New LayerRectangle
	End

	#rem monkeydoc @hidden
	#end
	Method NewLayerSprite:LayerSprite( identifier:String ) Virtual
		Return New LayerSprite
	End

	#rem monkeydoc @hidden
	#end
	Method OnLoading( data:Config ) Virtual
	End

	#rem monkeydoc @hidden
	#end
	Method ToString:String()
		Local data:=""
		For Local i:=0 Until Data.Length
			If data<>"" data+="~n~n"
			data+=Data[i].ToString()
		Next
		Return data
	End

	#rem monkeydoc @hidden
	#end
	Function GetProperties:Config( data:String )
	
		If data="" Return Null
	
		data=data.Replace( "],[","~n" )

		data=data.Left( data.Length-1 )
		data=data.Right( data.Length-1 )

		Local properties:=New Config()
		properties.CreateData( data )

		Return properties

	End

End

#rem monkeydoc The ScoreSystem class.
#end
Class ScoreSystem

	Field Hits:=0
	Field Invincible:=False
	Field Level:=1
	Field Lives:=3
	Field Reward:=0
	Field Stamina:=1
	Field TotalScore:=0

	Method New()
	End

End

#rem monkeydoc @hidden
#end
Class VirtualTileset

	Global Content:=New ContentManager

	Method New( imagePath:String,tileWidth:Int,tileHeight:Int,margin:Int=0,spacing:Int=0,handle:Vec2f=New Vec2f( .5,.5 ),shader:Shader=Null )
		Create( imagePath,tileWidth,tileHeight,margin,spacing,handle,shader )
	End

	Method Create( imagePath:String,tileWidth:Int,tileHeight:Int,margin:Int=0,spacing:Int=0,handle:Vec2f=New Vec2f( .5,.5 ),shader:Shader=Null )

		Local image:=Content.GetImage( imagePath,shader )

'		DebugAssert( image, "Error loading image!" )

		_imagePath=imagePath
		_tileWidth=tileWidth
		_tileHeight=tileHeight
		_margin=margin
		_spacing=spacing

		_imageWidth=image.Width
		_imageHeight=image.Height

		_tilesWidth=Int( image.Width/( tileWidth+spacing ) )
		_tilesHeight=Int( image.Height/( tileHeight+spacing ) )
	
		Local id:=0
	
		Local x:=0
		Local y:=margin
	
		For Local h:=0 Until _tilesHeight
	
			x=margin
	
			For Local w:=0 Until _tilesWidth
	
				Local virtualPath:=StripExt( imagePath )+"."+id

				Local tileImage:=Content.AddImage( New Image( image,New Recti( x,y,x+tileWidth,y+tileHeight ) ),virtualPath )
				If tileImage
					tileImage.Handle=handle
				Endif
	
				id+=1
	
				x+=tileWidth+spacing
	
			Next
	
			y+=tileHeight+spacing
	
		Next

	End

	Method GetTileImage:Image( id:Int )

		Local imagePath:=StripExt( _imagePath )+"."+id

		Return Content.GetImage( imagePath )

	End

	Field _imageHeight:=0
	Field _imagePath:=""
	Field _imageWidth:=0
	Field _margin:=0
	Field _spacing:=0
	Field _tileHeight:=0
	Field _tileWidth:=0
	Field _tilesHeight:=0
	Field _tilesWidth:=0

End
