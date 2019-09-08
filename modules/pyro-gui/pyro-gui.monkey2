Namespace pyro.gui

#Import "<std>"
#Import "<mojo>"
#Import "<pyro-framework>"

Using std..
Using mojo..
Using pyro.framework..

#rem monkeydoc @hidden
#end
Global GuiContent:=New ContentManager
#rem monkeydoc @hidden
#end
Global GuiDialogColor:=New Color( .5,.5,.5,1 )

#rem monkeydoc @hidden
#end
Global CurrentMenu:GuiMenu
#rem monkeydoc @hidden
#end
Global CurrentSubMenu:GuiMenu

#rem monkeydoc @hidden
#end
Global KeyboardReceiver:GuiObject

#rem monkeydoc Gui states.

Every Gui object can have these states.

#end
Enum GuiState
	Idle=0
	Selected=1
	RollOver=2
	Down=3
	Ghost=4
End

#rem monkeydoc @hidden
#end
Function CreateGroup:GuiGroup( obj:GuiGroup,image:Image )

	obj.Width=image.Width
	obj.Height=image.Height

	obj.Surface.DrawData( GuiState.Idle ).Image=image

	Return obj

End

#rem monkeydoc @hidden
#end
Function CreateGroup:GuiGroup( image:Image )
	Return CreateGroup( New GuiGroup,image )
End

#rem monkeydoc Draw gui layer.
#end
Function Draw( layer:GuiLayer,canvas:Canvas )
	layer.Draw( canvas )
End

#rem monkeydoc Draw gui layers.
#end
Function Draw( layer:GuiLayer[],canvas:Canvas )
	For Local i:=0 Until layer.Length
		layer[i].Draw( canvas )
	Next
End

#rem monkeydoc Update gui layer.
#end
Function Update( layer:GuiLayer )
	layer.Update()
End

#rem monkeydoc Update gui layers.
#end
Function Update( layer:GuiLayer[],canvas:Canvas )
	For Local i:=0 Until layer.Length
		layer[i].Update()
	Next
End

#rem monkeydoc Send key event to layer.
Belongs in the main OnKeyEvent of your app.
#end
Function SendKeyEvent( event:KeyEvent,layer:GuiLayer )
	layer.SendKeyEvent( event )
End

#rem monkeydoc Send key event to layers.
Belongs in the main OnKeyEvent of your app.
#end
Function SendKeyEvent( event:KeyEvent,layer:GuiLayer[] )
	For Local i:=0 Until layer.Length
		layer[i].SendKeyEvent( event )
	Next
End

#rem monkeydoc Send mouse event to layer.
Belongs in the main OnMouseEvent of your app.
#end
Function SendMouseEvent( event:MouseEvent,layer:GuiLayer )
	layer.SendMouseEvent( event )
End

#rem monkeydoc Send mouse event to layers.
Belongs in the main OnMouseEvent of your app.
#end
Function SendMouseEvent( event:MouseEvent,layer:GuiLayer[] )
	For Local i:=0 Until layer.Length
		layer[i].SendMouseEvent( event )
	Next
End

#rem monkeydoc @hidden
#end
Class GuiLoader

	Field Data:Config[]

	Method New()
	End

	Method New( filePath:String,layer:GuiLayer )
		Load( filePath )
		Build( layer )
	End

	#rem monkeydoc @hidden
	#end
	Method NewGuiButton:GuiButton( identifier:String ) Virtual
		Return New GuiButton
	End

	#rem monkeydoc Build the gui.
	#end
	Method Build( layer:GuiLayer )

		For Local i:=0 Until Data.Length

			Local name:=Data[i].ReadString( "name" )
			If name<>""
				Local obj:=Cast<GuiObject>( layer.GetObject( name ) )
				If obj<>Null obj.SetSkin( Data[i] )
			Endif

		Next

	End
	
	#rem monkeydoc Load the gui from a file.
	#end
	Method Load( filePath:String ) Virtual

		filePath=filePath.Replace( "\","/" )

		Local obj:=LoadString( SmartPath( filePath ) ).Replace( "~n~r","~n" ).Trim().Split( "~n~n" )

		Data=New Config[obj.Length]

		For Local i:=0 Until Data.Length

			Data[i]=New Config
			Data[i].CreateData( obj[i] )

			OnLoading( Data[i] )

		Next

	End

	#rem monkeydoc @hidden
	#end
	Method OnLoading( data:Config ) Virtual
	End

End

#rem monkeydoc The GuiButton class.
#end
Class GuiButton Extends GuiImage

	#rem monkeydoc Label.
	#end
	Field Label:=New TextSet

	#rem monkeydoc Whether the text is centered.
	#end
	Field TextCenter:=True

	Property Font:Font()
		Return Label.Font
	Setter( font:Font )
		Label.Font=font
	End

	Property Text:String()
		Return Label.Text
	Setter( text:String )
		Label.Text=text
	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		Super.Draw( canvas )

		If Label.Text="" And Label.Font=Null Return

		Local renderState:=RenderState( GuiState.Idle )
		If renderState=-1 Return

		Local x:=Label.X
		Local y:=Label.Y

		If TextCenter=True
			x+=Width*.5
			y+=Height*.5
			x-=Label.Width*.5
			y-=Label.Height*.5
		Endif

		canvas.PushMatrix()
		Label.DrawData( renderState,Frame ).InitDraw( canvas,Location.x,Location.y,ScaledWidth,ScaledHeight,Rotation*RotationMode,Handle.x,Handle.y,Brightness( Color ) )
		canvas.Font=Label.Font
		canvas.Scale( Scale.x,Scale.y )
		canvas.DrawText( Label.Text,x,y )
		canvas.PopMatrix()

	End

End

#rem monkeydoc The GuiCheckbox class.
#end
Class GuiCheckbox Extends GuiImage

	#rem monkeydoc @hidden
	#end
	Method __OnReleased() Override
		Frame+=1
		If Frame>=Surface.Frames Frame=0
	End

End

#rem monkeydoc The GuiGroup class.
#end
Class GuiGroup Extends GuiObject

	#rem monkeydoc @hidden
	#end
	Class Closer Extends GuiButton
	
		Field GroupToClose:GuiGroup
	
		Method OnReleased() Override
			If GroupToClose<>Null GroupToClose.Remove()
		End
	
	End

	Field Camera:=new Vec2f

	#rem monkeydoc Members.
	#end
	Field Members:=New Stack<GuiObject>

	#rem monkeydoc Surface.
	#end
	Field Surface:=New ImageSet

	Method New()
	End

	Method New( guiLayer:GuiLayer )
		Layer=guiLayer
	End

	Method New( group:GuiGroup )
		Group=group
	End

	#rem monkeydoc Whether the group is enabled.
	#end
	Property Enabled:Bool() Override
		Return Super.Enabled
	Setter( enabled:Bool ) Override
		Super.Enabled=enabled
		If enabled=True Return
		For Local i:=0 Until Members.Length
			Members.Get( i ).Entered=False
			Members.Get( i ).Hit=False
			Members.Get( i ).Left=False
			Members.Get( i ).Over=False
			Members.Get( i ).Released=False
		Next
	End

	#rem monkeydoc @hidden
	#end
	Property CameraX:Float()
		Return Camera.x
	Setter( cameraX:Float )
		Camera.x=cameraX
	End

	#rem monkeydoc @hidden
	#end
	Property CameraY:Float()
		Return Camera.y
	Setter( cameraY:Float )
		Camera.y=cameraY
	End

	#rem monkeydoc @hidden
	#end
	Method Clear()
		Members.Clear()
	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		__DrawSurface( canvas )
		__Draw( canvas )

		If Layer And Layer.DebugEditor And Layer._sticky=Self __DrawDebugSelected( canvas )

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
			Local y:=Members.Get( i ).Location.y
			y-=Members.Get( i ).ScaledHeight*Members.Get( i ).Handle.y
			If Members.Get( i ).Enabled=True And y<y1 y1=y
		Next

		Local y2:=0

		For Local i:=0 Until Members.Length
			Local y:=Members.Get( i ).Location.y
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
			Local x:=Members.Get( i ).Location.x
			x-=Members.Get( i ).ScaledWidth*Members.Get( i ).Handle.x
			If Members.Get( i ).Enabled=True And x<x1 x1=x
		Next

		Local x2:=0

		For Local i:=0 Until Members.Length
			Local x:=Members.Get( i ).Location.x
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

	#rem monkeydoc @hidden
	#end
	Method Snapshot:String() Override

		Local data:= Super.Snapshot()
		For Local i:=0 Until Members.Length
			If Members[i].Name<>""
				If data data+="~n"
				data+=Members[i].Snapshot()
			Endif
		Next

		Return data

	End

	#rem monkeydoc @hidden
	#end
	Method __Draw( canvas:Canvas )
	
		canvas.PushMatrix()

		canvas.Translate( Location.x,Location.y )

		canvas.Scale( Scale.x,Scale.y )

		canvas.Translate( -Width*Handle.x,-Height*Handle.y )

		canvas.Translate( Width*Handle.x,Height*Handle.y )
		canvas.Rotate( Rotation*RotationMode )
		canvas.Translate( -Width*Handle.x,-Height*Handle.y )

		DrawBackground( canvas )

		canvas.Translate( -Camera.x,-Camera.y )

		__DrawMembers( canvas )

		canvas.PopMatrix()

	End

	#rem monkeydoc @hidden
	#end
	Method __DrawMembers( canvas:Canvas )
		For Local i:=0 Until Members.Length
			If Members.Get( i ).Enabled=False Continue
			If Members.Get( i ).Visible=False Continue
			Members.Get( i ).Draw( canvas )
		Next
	End

	#rem monkeydoc @hidden
	#end
	Method __DrawSurface( canvas:Canvas ) Virtual

		Local renderState:=RenderState( GuiState.Idle )
		If renderState=-1 Return

		Local surfaceImage:=Surface.DrawData( GuiState.Idle ).Image

		If Surface.DrawData( renderState ).Image<>Null surfaceImage=Surface.DrawData( renderState ).Image

		If surfaceImage<>Null
			canvas.PushMatrix()
			Surface.DrawData( renderState,Frame ).InitDraw( canvas,Location.x,Location.y,ScaledWidth,ScaledHeight,Rotation*RotationMode,Handle.x,Handle.y,Brightness( Color ) )
			Draw9Patch( canvas,surfaceImage,Surface.PatchData,0,0,Width,Height,0,Scale.x,Scale.y,0,0 )
			canvas.PopMatrix()
		Endif
		
	End

	#rem monkeydoc @hidden
	#end
	Method __SendKeyEvent( event:KeyEvent ) Override
		For Local i:=0 Until Members.Length
			If Members.Get( i ).Enabled=True And Members.Get( i ).AcceptsKeyEvents=True Members.Get( i ).__SendKeyEvent( event )
		Next
		Super.__SendKeyEvent( event )
	End

	#rem monkeydoc @hidden
	#end
	Method __Update() Override
		__UpdateMembers()
	End

	#rem monkeydoc @hidden
	#end
	Method __UpdateMembers()

		For Local i:=0 Until Members.Length
			If Members.Get( i ).Enabled=True Members.Get( i ).__Update()
		Next

		For Local i:=0 Until Members.Length
			If Members.Get( i ).Enabled=True Members.Get( i ).OnUpdate()
		Next

	End

	#rem monkeydoc @hidden
	#end
	Method __UpdateState() Override
		For Local i:=0 Until Members.Length
			Members.Get( i ).__UpdateState()
		Next
		Super.__UpdateState()
	End

	#rem monkeydoc @hidden
	#end
	Method __UpdateCurrentGuiObject() Override

		If RememberAsTopObject=False or Enabled=False or Visible=False Return

		For Local i:=Members.Length-1 To 0 Step -1
			Members.Get( i ).__UpdateCurrentGuiObject()
		Next

		Super.__UpdateCurrentGuiObject()

	End

End

#rem monkeydoc The GuiHProgressBar class.
#end
Class GuiHProgressBar Extends GuiObject

	#rem monkeydoc Indicator.
	#end
	Field Indicator:=New ImageSet
	#rem monkeydoc Maximum value.
	#end
	Field Maximum:=100
	#rem monkeydoc Surface.
	#end
	Field Surface:=New ImageSet
	#rem monkeydoc Value.
	#end
	Field Value:=0

	#rem monkeydoc Height.
	#end
	Property Height:Float() Override
		Return Surface.Height
	Setter( height:Float ) Override
		Surface.Height=height
	End

	#rem monkeydoc Width.
	#end
	Property Width:Float() Override
		Return Surface.Width
	Setter( width:Float ) Override
		Surface.Width=width
	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		Local renderState:=RenderState( GuiState.Idle )
		If renderState=-1 Return

		Local surfaceImage:=Surface.DrawData( GuiState.Idle ).Image

		If Surface.DrawData( renderState ).Image<>Null surfaceImage=Surface.DrawData( renderState ).Image

		If surfaceImage<>Null
			canvas.PushMatrix()
			Surface.DrawData( renderState,Frame ).InitDraw( canvas,Location.x,Location.y,ScaledWidth,ScaledHeight,Rotation*RotationMode,Handle.x,Handle.y,Brightness( Color ) )
			Draw9Patch( canvas,surfaceImage,Surface.PatchData,0,0,Width,Height,0,Scale.x,Scale.y,0,0 )
			canvas.PopMatrix()
		Endif
		
		Local indicatorImage:=Indicator.DrawData( GuiState.Idle ).Image

		If Indicator.DrawData( renderState ).Image<>Null indicatorImage=Indicator.DrawData( renderState ).Image

		If indicatorImage<>Null

			Local s:=Float( Maximum )/Float( Indicator.Width )

			Local width:=Value/s
			Local height:=Indicator.Height
	
			If width<0 width=0
			If width>Indicator.Width width=Indicator.Width

			canvas.PushMatrix()
	
			If Indicator.PatchData.Length<>36

				Indicator.DrawData( renderState ).InitDraw( canvas,Location.x,Location.y,ScaledWidth,ScaledHeight,Rotation*RotationMode,Handle.x,Handle.y,Brightness( Color ) )

				Indicator.InitDraw( canvas )

				canvas.Scale( Scale.x,Scale.y )

				canvas.DrawRect( 0,0,width,height,indicatorImage,0,0,width,height )

			Else

				Indicator.DrawData( renderState ).InitDraw( canvas,Location.x,Location.y,ScaledWidth,ScaledHeight,Rotation*RotationMode,Handle.x,Handle.y,Brightness( Color ) )

				Indicator.InitDraw( canvas )

				Draw9Patch( canvas,indicatorImage,Indicator.PatchData,0,0,width,height,0,Scale.x,Scale.y,0,0 )

			Endif

			canvas.PopMatrix()

			If Selected __DrawSelected( canvas )

			If Layer And Layer.DebugEditor And Layer._sticky=Self __DrawDebugSelected( canvas )

		Endif

	End

End

#rem monkeydoc The GuiImage class.
#end
Class GuiImage Extends GuiObject

	#rem monkeydoc Surface.
	#end
	Field Surface:=New ImageSet

	Method New()
	End
	
	Method New( layer:GuiLayer,image:Image )
		Layer=layer
		Surface.DrawData( GuiState.Idle ).Image=image
	End

	Method New( group:GuiGroup,image:Image )
		Group=group
		Surface.DrawData( GuiState.Idle ).Image=image
	End

	#rem monkeydoc Height.
	#end
	Property Height:Float() Override
		Return Surface.Height
	Setter( height:Float ) Override
		Surface.Height=height
	End

	#rem monkeydoc Width.
	#end
	Property Width:Float() Override
		Return Surface.Width
	Setter( width:Float ) Override
		Surface.Width=width
	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		Local renderState:=RenderState( GuiState.Idle )
		If renderState=-1 Return

		Local surfaceImage:=Surface.DrawData( GuiState.Idle,Frame ).Image

		If Surface.DrawData( renderState,Frame ).Image<>Null surfaceImage=Surface.DrawData( renderState,Frame ).Image

		If surfaceImage<>Null
			canvas.PushMatrix()
			Surface.DrawData( renderState,Frame ).InitDraw( canvas,Location.x,Location.y,ScaledWidth,ScaledHeight,Rotation*RotationMode,Handle.x,Handle.y,Brightness( Color ) )
			Draw9Patch( canvas,surfaceImage,Surface.PatchData,0,0,Width,Height,0,Scale.x,Scale.y,0,0 )
			canvas.PopMatrix()
		Endif

		If Selected __DrawSelected( canvas )

		If Layer And Layer.DebugEditor And Layer._sticky=Self __DrawDebugSelected( canvas )
		
	End

End

#rem monkeydoc The GuiInputBox class.
#end
Class GuiInputBox Extends GuiImage

	#rem monkeydoc @hidden
	#end
	Field AllowedChars:Int[]
	#rem monkeydoc Border size.
	#end
	Field Border:=0
	Field Cursor:=New Cursor
	#rem monkeydoc Cursor color.
	#end
	Field CursorColor:=New Color( .5,.5,.5 )
	Field EnteredKey:=False
	#rem monkeydoc @hidden
	#end
	Field ForbiddenChars:Int[]
	#rem monkeydoc Label.
	#end
	Field Label:=New TextSet
	#rem monkeydoc Maximum allowed characters.
	#end
	Field MaxChar:=0
	Field TextCenter:=True

	Property EnterKey:Bool()
		Local enterKey:=EnteredKey
		EnteredKey=False
		Return enterKey
	Setter( enterKey:Bool )
		EnteredKey=enterKey
	End

	Property Font:Font()
		Return Label.Font
	Setter( font:Font )
		Label.Font=font
	End

	Property Text:String()
		Return Label.Text
	Setter( text:String )
		Label.Text=text
		Cursor.CursorPosition=text.Length
	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		Super.Draw( canvas )

		If Label.Text="" And Label.Font=Null Return

		Local renderState:=RenderState( GuiState.Idle )
		If renderState=-1 Return

		Local showCursor:=False

		If KeyboardReceiver=Self showCursor=True

		Local x:=Border*.5
		Local y:=Height*.5-Label.Height*.5

		If TextCenter x=Width*.5-Label.Width*.5

		canvas.PushMatrix()
		Label.DrawData( renderState,Frame ).InitDraw( canvas,Location.x,Location.y,ScaledWidth,ScaledHeight,Rotation*RotationMode,Handle.x,Handle.y,Brightness( Color ) )
		canvas.Font=Label.Font
		canvas.Scale( Scale.x,Scale.y )
		Cursor.DrawText( canvas,Label.Text,x,y,showCursor,canvas.Color,CursorColor )
		canvas.PopMatrix()

	End

	#rem monkeydoc @hidden
	#end
	Method OnKeyEvent( event:KeyEvent ) Override

		If Ghost=True Return

		If KeyboardReceiver=Self

			If MaxChar<>0 And Label.Text.Length>=MaxChar
				Label.Text=Cursor.SpecialKeys( event,Label.Text )
				Return
			Endif
			If Label.Width>Surface.Width-Border
				Label.Text=Cursor.SpecialKeys( event,Label.Text )
				Return
			Endif

			If event.Type=EventType.KeyDown And event.Key=Key.Enter EnteredKey=True

			Label.Text=Cursor.KeyEvent( event,Label.Text )

		Endif

	End

	#rem monkeydoc @hidden
	#end
	Method __OnReleased() Override
		If Ghost=True Return
		KeyboardReceiver=Self
	End

	Private

	Method _AllowedChars:Bool( char:Int )

		For Local i:=0 Until AllowedChars.Length
			If AllowedChars[i]=char Return True
		Next

		Return False

	End

	Method _ForbiddenChars:Bool( char:Int )

		For Local i:=0 Until ForbiddenChars.Length
			If ForbiddenChars[i]=char Return True
		Next

		Return False

	End

End

#rem monkeydoc The GuiLabel class.
#end
Class GuiLabel Extends GuiObject

	#rem monkeydoc Label.
	#end
	Field Label:=New TextSet

	Method New()
	End

	Method New( layer:GuiLayer,font:Font,text:String )
		Layer=layer
		Label.Font=font
		Label.Text=text
	End

	Method New( group:GuiGroup,font:Font,text:String )
		Group=group
		Label.Font=font
		Label.Text=text
	End

	Property Font:Font()
		Return Label.Font
	Setter( font:Font )
		Label.Font=font
	End

	#rem monkeydoc Height.
	#end
	Property Height:Float() Override
		Return Label.Height
	Setter( height:Float ) Override
		Label.Height=height
	End

	Property Text:String()
		Return Label.Text
	Setter( text:String )
		Label.Text=text
	End

	#rem monkeydoc Width.
	#end
	Property Width:Float() Override
		Return Label.Width
	Setter( width:Float ) Override
		Label.Width=width
	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas ) Override

		If Label.Text="" Or Label.Font=Null Return

		Local renderState:=RenderState( GuiState.Idle )
		If renderState=-1 Return

		canvas.PushMatrix()
		Label.DrawData( renderState,Frame ).InitDraw( canvas,Location.x,Location.y,ScaledWidth,ScaledHeight,Rotation*RotationMode,Handle.x,Handle.y,Brightness( Color ) )
		canvas.Font=Label.Font
		canvas.DrawText( Label.Text,0,0 )
		canvas.PopMatrix()

		If Selected __DrawSelected( canvas )

		If Layer And Layer.DebugEditor And Layer._sticky=Self __DrawDebugSelected( canvas )

	End

End

#rem monkeydoc The GuiLayer class.
#end
Class GuiLayer

	#rem monkeydoc Whether the layer accepts key events.
	#end
	Field AcceptsKeyEvents:=True
	#rem monkeydoc Ambient light color.
	
	Enables lighting mode when AmbientLight is set.
	
	#end
	Field AmbientLight:Color
	#rem monkeydoc Clear color.
	#end
	Field ClearColor:Color
	Field Color:=New Color( 1,1,1,1 )
	Field DebugEditor:=False
	Field Enabled:=True
	Field GuiObjects:=New Stack<GuiObject>
	Field Height:=0.0
	Field InputDevice:=New InputDevice
	Field Location:=New Vec2f
	Field Name:=""
	Field Offset:=New Vec2f
	Field Rotation:=0.0
	Field RotationPoint:=New Vec2f
	Field Scale:=New Vec2f( 1,1 )
	Field Tattooed:=False
	Field View:View
	Field Visible:=True
	Field Width:=0.0
	Field Z:=0.0
	Field Zoom:=1.0
	Field ZoomPoint:=New Vec2f

	#rem monkeydoc @hidden
	#end
	Field CurrentGuiObject:GuiObject
	Field GuiDialog:GuiObject

	Method New( view:View )
		View=view
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

	#rem monkeydoc Viewport.
	
	The viewport describes the rect within the render target that rendering occurs in.
	
	All rendering is relative to the top-left of the viewport, and is clipped to the intersection of the current viewport and scissor rects.
		
	#end
	Property Viewport:Recti()
		If _viewport<>Null Return _viewport
		Return View.Rect
	Setter( viewport:Recti )
		_viewport=viewport
	End

	#rem monkeydoc Virtual resolution.
	#end
	Property VirtualResolution:Vec2f()
		Return New Vec2f( View.Width/_aspectRatio,View.Height/_aspectRatio )
	End

	#rem monkeydoc Virtual resolution.

	Returns virtual height.

	#end
	Property VirtualHeight:Int()
		Return View.Height/_aspectRatio
	End

	#rem monkeydoc Virtual resolution.

	Returns virtual width.

	#end
	Property VirtualWidth:Int()
		Return View.Width/_aspectRatio
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

	#rem monkeydoc Draw gui layer.
	
	Call Draw from main game render loop.
	
	#end
	Method Draw( canvas:Canvas )

		If Enabled=False Return

		Local font:=canvas.Font

		canvas.PushMatrix()

		If _viewport<>Null canvas.Viewport=_viewport

		If ClearColor<>Null canvas.Clear( ClearColor )

		canvas.Translate( Offset )

		canvas.Scale( Scale.x,Scale.y )

		canvas.Translate( Location.x,Location.y )

		If AmbientLight<>Null
			canvas.AmbientLight=AmbientLight
			canvas.BeginLighting()
		Endif

		Local color:=Color

		If GuiDialog<>Null
			If Color=Null Color=GuiDialogColor Else Color*=GuiDialogColor
		Endif
		
		For Local i:=0 Until GuiObjects.Length
			If GuiDialog<>Null And GuiDialog=GuiObjects.Get( i ) Continue
			If GuiObjects.Get( i ).Enabled=True And GuiObjects.Get( i ).Visible=True GuiObjects.Get( i ).Draw( canvas )
		Next

		Color=color

		If GuiDialog<>Null And GuiDialog.Enabled=True And GuiDialog.Visible=True GuiDialog.Draw( canvas )

		If AmbientLight<>Null canvas.EndLighting()

		canvas.PopMatrix()

		canvas.Font=font
		
		If DebugEditor=True _DrawDebugEditor( canvas )

	End

	#rem monkeydoc Mouse location relative to the gui layer.
	#end
	Method GetMouseLocation:Vec2i()

		Local mouse:=View.MouseLocation+Offset

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
		x2-=Location.x

		y2/=Zoom
		y2+=ZoomPoint.y-ZoomPoint.y/Zoom
		y2-=Location.y	

		Return New Vec2i( x2,y2 )

	End

	#rem monkeydoc Get object by name.
	#end
	Method GetObject:Object( name:String ) Virtual

		For Local i:=0 Until GuiObjects.Length
			Local search:=GuiObjects[i].GetObject( name )
			If search<>Null Return search
		Next

		Return Null

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

	Method SendKeyEvent( event:KeyEvent )

		If AcceptsKeyEvents=False Return

		For Local i:=0 Until GuiObjects.Length
			If GuiObjects.Get( i ).Enabled=True And GuiObjects.Get( i ).AcceptsKeyEvents=True GuiObjects.Get( i ).__SendKeyEvent( event )
		Next

		If DebugEditor=True _OnDebugEditorKeyEvents( event )

	End

	Method SendMouseEvent( event:MouseEvent )
	End

	#rem monkeydoc @hidden
	#end
	Method SetOverlay( canvas:Canvas )

		If _viewport<>Null canvas.Viewport=_viewport

		canvas.Scale( Scale.x,Scale.y )

	End

	#rem monkeydoc Set virtual resolution.

	Width is stretched according to ratio.

	#end
	Method SetVirtualHeight( height:Float )
		_aspectRatio=View.Height/height
		Scale=New Vec2f( _aspectRatio,_aspectRatio )
	End

	#rem monkeydoc @hidden
	#end
	Method SetVirtualResolution( width:Float,height:Float )

		Local w:=Float( View.Width )
		Local h:=Float( View.Height )
	
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

	Height is stretched according to ratio.

	#end
	Method SetVirtualWidth( width:Float )
		_aspectRatio=View.Width/width
		Scale=New Vec2f( _aspectRatio,_aspectRatio )
	End

	#rem monkeydoc @hidden
	#end
	Method Snapshot:String()

		If Name="" Return ""

		Local data:=""
		
		data+="name="+Name+"~n"
		
		If X<>0 data+="x="+X+"~n"
		If Y<>0 data+="y="+Y+"~n"

		For Local i:=0 Until GuiObjects.Length
			If GuiObjects[i].Name<>""
				If data<>"" data+="~n"
				data+=GuiObjects[i].Snapshot()
			Endif
		Next

		Return data

	End

	#rem monkeydoc @hidden
	#end
	Method Translate( canvas:Canvas )

		canvas.Translate( -Offset )

		canvas.Scale( Scale.x,Scale.y )

		canvas.Translate( Location.x,Location.y )

	End

	#rem monkeydoc Update gui layer.
	
	Call Update from main game render loop.
	
	#end

	Method Update()

		If Enabled=False Return

		InputDevice.Update()

'		If App.HoverView<>_view Return

		Local resetGuiObject:=CurrentGuiObject

		If resetGuiObject<>Null

			resetGuiObject.Entered=False
			resetGuiObject.Hit=False
			resetGuiObject.Left=False
			resetGuiObject.Released=False

		Endif

		If CurrentGuiObject<>Null And CurrentGuiObject._busy=False
			CurrentGuiObject=Null
		Endif

		If GuiDialog=Null

			For Local i:=GuiObjects.Length-1 To 0 Step -1
				GuiObjects.Get( i ).__UpdateCurrentGuiObject()
			Next

			If TouchMode()=False And resetGuiObject<>Null And resetGuiObject<>CurrentGuiObject And resetGuiObject.Over=True

				resetGuiObject.OnLeft()
				resetGuiObject.SendMessage( "Left" )
				resetGuiObject.Left=True

				resetGuiObject.Over=False

			Endif

			If CurrentGuiObject<>Null
				CurrentGuiObject.__UpdateState()
			Endif

			For Local i:=0 Until GuiObjects.Length
				If GuiObjects.Get( i ).Enabled=True GuiObjects.Get( i ).__Update()
				If GuiObjects.Get( i ).Enabled=True GuiObjects.Get( i ).OnUpdate()
			Next
	
		Else

			CurrentGuiObject=GuiDialog

			CurrentGuiObject.__UpdateCurrentGuiObject()

			If TouchMode()=False And resetGuiObject<>Null And resetGuiObject<>CurrentGuiObject And resetGuiObject.Over=True

				resetGuiObject.OnLeft()
				resetGuiObject.SendMessage( "Left" )
				resetGuiObject.Left=True

				resetGuiObject.Over=False

			Endif

			CurrentGuiObject.__UpdateState()

			If CurrentGuiObject.Enabled=True CurrentGuiObject.__Update()
			If CurrentGuiObject.Enabled=True CurrentGuiObject.OnUpdate()

		Endif

		If DebugEditor=True _UpdateDebugEditor() Else _UpdateDraggables()

		If _sticky _sticky.Location+=( GetMouseLocation()-_previouseMouseLocation )

''		If _previouseMouseLocation<>GetMouseLocation() Mouse.ButtonPressed( MouseButton.Left ) ' Ugly monkey hack to 'reset' button state!

		_previouseMouseLocation=GetMouseLocation()

	End

	Method ZSort()

		Local index1:=GuiObjects.Length-1
 
		While index1>0

			For Local index2:=0 Until GuiObjects.Length-1
 
				Local o1:=GuiObjects.Get( index2 )
				Local o2:=GuiObjects.Get( index2+1 )

				If o1.Z<o2.Z GuiObjects.Swap( index2,index2+1 )

			Next

			index1-=1

		Wend
	
	End

	Private

	Method _DrawDebugEditor( canvas:Canvas )

		Local editorWidth:=View.Width
		Local editorHeight:=canvas.Font.Height*1.25
		Local editorX:=0
		Local editorY:=View.Height-editorHeight
		
		Local info:=""
		Local name:=String.FromChar(34)+String.FromChar(34)
		Local x:=0
		Local y:=0

		If _sticky

			If _sticky.Name name=_sticky.Name
				
			info="Name="+name+" | Location="+_sticky.X+","+_sticky.Y

		Endif

		canvas.BlendMode=DebugGui.BlendMode
		canvas.Color=DebugGui.Color
		canvas.DrawRect( editorX,editorY,editorWidth,editorHeight )

		If info<>""
			canvas.Color=DebugGui.TextColor
			canvas.DrawText( info,editorX+8,editorY+editorHeight*.5-canvas.Font.Height*.5 )
		Endif

	End

	Method _OnDebugEditorKeyEvents( event:KeyEvent )

		If _sticky<>Null

			If event.Type=EventType.KeyDown Or event.Type=EventType.KeyRepeat
				If event.Key=Key.Left _sticky.X-=1
				If event.Key=Key.Right _sticky.X+=1
				If event.Key=Key.Up _sticky.Y-=1
				If event.Key=Key.Down _sticky.Y+=1
			Endif

			If event.Type=EventType.KeyDown
				If event.Key=Key.T _sticky.Enabled=Not _sticky.Enabled
			Endif

		Endif

	End

	Method _UpdateDebugEditor()

		If _sticky=Null And CurrentGuiObject<>Null And CurrentGuiObject.Down=True
			_sticky=CurrentGuiObject
		Endif

		If _sticky And _sticky.Down=False
			_sticky=Null
		Endif

	End

	Method _UpdateDraggables()

		If _sticky=Null And CurrentGuiObject<>Null And CurrentGuiObject.Down=True And CurrentGuiObject.Draggable=True
			_sticky=CurrentGuiObject
			If _sticky._layer<>Null _sticky.Layer=_sticky._layer ' Dirty test
		Endif

		If _sticky And _sticky.Down=False
			_sticky=Null
		Endif

	End

	Method _UpdateContextMenu()

		If CurrentGuiObject=Null
			If CurrentMenu<>Null And CurrentMenu.Enabled=True CurrentMenu.Enabled=False
			If CurrentSubMenu<>Null And CurrentSubMenu.Enabled=True CurrentSubMenu.Enabled=False
		Endif

	End

	Field _aspectRatio:=1.0
	Field _previouseMouseLocation:=New Vec2i
	Field _sticky:GuiObject
	Field _viewport:Recti

End

#rem monkeydoc @hidden
#end
Class GuiMenu Extends GuiGroup

	Method GetItem:GuiMenuItem( name:String )

		For Local i:=0 Until Members.Length
			Local item:=Cast<GuiMenuItem>( Members.Get( i ) )
			If item And item.Name=name Return item
		Next

		Return Null

	End

	Method Show( x:Float,y:Float )

		If CurrentMenu And CurrentMenu<>Self
''			If SubMenu=Null
				If CurrentMenu.Enabled CurrentMenu.Enabled=False
''			Endif
		Endif

		If CurrentSubMenu And CurrentSubMenu<>Self
			If CurrentSubMenu.Enabled CurrentSubMenu.Enabled=False
		Endif

		CurrentMenu=Self
		X=x
		Y=y
		Enabled=True

	End

End

#rem monkeydoc @hidden
#end
Class GuiMenuBar Extends GuiGroup

	Property Height:Float() Override
		If Layer=Null Return 0
		Return _height
	Setter( height:Float ) Override
		_height=height
	End

	Property Width:Float() Override
		If Layer=Null Return 0
		Return Layer.Viewport.Width
	Setter( width:Float ) Override
		_width=width
	End

	Method GetTab:GuiMenuTab( name:String )

		For Local i:=0 Until Members.Length
			Local tab:=Cast<GuiMenuTab>( Members.Get( i ) )
			If tab And tab.Name=name Return tab
		Next

		Return Null

	End

	Method __DrawSurface( canvas:Canvas ) Override

		Local renderState:=RenderState( GuiState.Idle )
		If renderState=-1 Return

		Local surfaceImage:=Surface.DrawData( GuiState.Idle ).Image

		If Surface.DrawData( renderState ).Image<>Null surfaceImage=Surface.DrawData( renderState ).Image

		If surfaceImage<>Null
			canvas.PushMatrix()
			Surface.DrawData( renderState,Frame ).InitDraw( canvas,Location.x,Location.y,ScaledWidth,ScaledHeight,Rotation*RotationMode,Handle.x,Handle.y,Brightness( Color ) )
			Draw9Patch( canvas,surfaceImage,Surface.PatchData,_width,0,Layer.View.Width-_width,Height,0,Scale.x,Scale.y,0,0 )
			canvas.PopMatrix()
		Endif
		
	End

End

#rem monkeydoc @hidden
#end
Class GuiMenuItem Extends GuiButton

	Field SubMenu:GuiMenu

	Method OnReleased() Override

		If CurrentMenu
			If SubMenu=Null
				If CurrentMenu.Enabled CurrentMenu.Enabled=False
			Endif
		Endif

		If CurrentSubMenu
			If CurrentSubMenu.Enabled CurrentSubMenu.Enabled=False
		Endif

		If SubMenu

			If SubMenu.Enabled=False
				CurrentSubMenu=SubMenu
				SubMenu.Enabled=True
			Endif
		
		Endif

	End

End

#rem monkeydoc @hidden
#end
Class GuiMenuTab Extends GuiButton

	Field Menu:GuiMenu

	Method OnReleased() Override

		If CurrentMenu
			CurrentMenu.Enabled=False
			CurrentMenu=Null
		Endif

		If CurrentSubMenu
			CurrentSubMenu.Enabled=False
			CurrentSubMenu=Null
		Endif

		If Menu
		
			If Menu.Enabled=False

				CurrentMenu=Menu
				Menu.Enabled=True

			Endif
		
		Endif

	End

	Method OnRollOver() Override

		If CurrentMenu And CurrentMenu.Enabled And Menu And Menu<>CurrentMenu And Group=CurrentMenu.Group
		
			If CurrentMenu
				CurrentMenu.Enabled=False
				CurrentMenu=Null
			Endif
	
			If CurrentSubMenu
				CurrentSubMenu.Enabled=False
				CurrentSubMenu=Null
			Endif

			CurrentMenu=Menu

			Menu.Enabled=True
		
		Endif

	End

End

#rem monkeydoc The GuiObject class.
#end
''Class GuiObject Extends pyro.framework.taskmanager.Task
Class GuiObject Extends Task

	Field AcceptsKeyEvents:=True
	Field BlendMode:=mojo.graphics.BlendMode.Alpha
	Field Clipper:GuiGroup
	Field Color:=New Color( 1,1,1,1 )
	Field DisableIfTattooedOnRemove:=False
	Field Down:=False
	Field DownUntilRelease:=False
	Field Draggable:=False
	Field Entered:=False
	Field Handle:=New Vec2f( .5, .5 )
	Field Hit:=False
	Field Frame:=0
	Field Ghost:=False
	Field Left:=False
	Field Location:=New Vec2f
	Field Over:=False
	Field Released:=False
	Field RememberAsTopObject:=True
	Field RenderStates:=New Int[]( GuiState.Idle,GuiState.RollOver,GuiState.Down,GuiState.Ghost )
	Field Rotation:=0.0
	Field Scale:=New Vec2f( 1,1 )
	Field Selected:=False
	Field SkinDir:=""
	Field SkinPrefix:=""
	Field Tattooed:=False
	Field Visible:=True
	Field Z:=0.0

	Property Alpha:Float()
		Return Color.a
	Setter( a:Float )
		Color.a=a
	End

	Property Groups:GuiGroup[]()

		Local group:=Group

		Local groups:GuiGroup[]

		While group
			Local length:=groups.Length
			groups=ResizeArray( groups,length+1 )
			groups[length]=group
			group=group.Group
		Wend

		Return groups

	End

	Property Group:GuiGroup()

		Return _group

	Setter( group:GuiGroup )

		If _layer<>Null Layer=Null

		If _group _group.Members.RemoveEach( Self )
		_group=group
		If _group _group.Members.Push( Self )

	End
	
	Property HandleX:Float()
		Return Handle.x
	Setter( handleX:Float )
		Handle.x=handleX
	End

	Property HandleY:Float()
		Return Handle.y
	Setter( handleY:Float )
		Handle.y=handleY
	End

	Property Height:Float() Virtual
		Return _height
	Setter( height:Float ) Virtual
		_height=height
	End

	Property Layer:GuiLayer() Virtual

		If _group<>Null
			Local groups:=Groups
			Return groups[groups.Length-1]._layer
		Endif

		Return _layer

	Setter( layer:GuiLayer ) Virtual

		If _group<>Null Group=Null

		If _layer _layer.GuiObjects.RemoveEach( Self )
		_layer=layer
		If _layer _layer.GuiObjects.Push( Self )

	End

	Property MouseOver:Bool()
		Return PointInside( Mouse.Location )
	End
	
	Property Root:GuiGroup()

		Local group:=Group
		Local root:GuiGroup

		While group
			root=group
			group=group.Group
		Wend

		Return root

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

	Property ScaledHeight:Float()
		Return Height*Scale.y
	End

	Property ScaledWidth:Float()
		Return Width*Scale.x
	End

	Property Width:Float() Virtual
		Return _width
	Setter( width:Float ) Virtual
		_width=width
	End

	Property WorldScaleX:Float()

		If Group=Null Return Scale.x

		_UpdateWorldData()
		
		Return _worldScale.x

	End

	Property WorldScaleY:Float()

		If Group=Null Return Scale.y

		_UpdateWorldData()
		
		Return _worldScale.y

	End

	Property WorldHeight:Float()

		If Group=Null Return ScaledHeight
		
		_UpdateWorldData()
		
		Return _worldHeight

	End

	Property WorldRotation:Float()

		If Group=Null Return Rotation
		
		_UpdateWorldData()
		
		Return _worldRotation

	End

	Property WorldWidth:Float()

		If Group=Null Return ScaledWidth

		_UpdateWorldData()
		
		Return _worldWidth
	
	End

	Property WorldX:Float()

		If Group=Null Return Location.x

		_UpdateWorldData()
		
		Return _worldLocation.x

	End

	Property WorldY:Float()

		If Group=Null Return Location.y

		_UpdateWorldData()
		
		Return _worldLocation.y

	End

	Property X:Float() Virtual
		Return Location.x
	Setter( x:Float ) Virtual
		Location.x=x
	End

	Property Y:Float() Virtual
		Return Location.y
	Setter( y:Float ) Virtual
		Location.y=y
	End

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

	Method Draw( canvas:Canvas ) Virtual
	End

	#rem monkeydoc @hidden
	#end
	Method GetObject:Object( name:String ) Virtual
		If Name=name Return Self
		Return Null
	End

	Method Ghosted:Bool( ghost:Bool )

		If _group=Null Return ghost

		Local group:=Group

		While group
			If group.Ghost=True Return True
			group=group.Group
		Wend

		Return ghost

	End

	Method GroupInsert( group:GuiGroup,index:Int )

		If _layer<>Null Layer=Null

		If _group _group.Members.RemoveEach( Self )
		_group=group
		If _group _group.Members.Insert( index,Self )

	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseDown:Bool( mouseButton:MouseButton )
		If Layer=Null Return False
		Return Layer.InputDevice.ButtonDown[ mouseButton ]
	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseHit:Bool( mouseButton:MouseButton )
		If Layer=Null Return False
		Return Layer.InputDevice.ButtonHit[ mouseButton ]
	End

	#rem monkeydoc Checks the current up/down state of a mouse button.
	#end
	Method MouseReleased:Bool( mouseButton:MouseButton )
		If Layer=Null Return False
		Return Layer.InputDevice.ButtonReleased[ mouseButton ]
	End

	Method OnDown() Virtual
	End

	Method OnEntered() Virtual
	End

	Method OnHit() Virtual
	End

	Method OnKeyEvent( event:KeyEvent ) Virtual
	End

	Method OnLeft() Virtual
	End

	Method OnReleased() Virtual
	End

	Method OnRollOver() Virtual
	End

	Method PointInside:Bool( x:Int,y:Float ) Virtual

		x+=Layer.Offset.x
		y+=Layer.Offset.y

		If Clipper<>Null And Clipper.PointInside( x,y )=False Return False

		Local x1:=x/Layer.Scale.x
		Local y1:=y/Layer.Scale.y

		Local x2:=Layer.ZoomPoint.x
		Local y2:=Layer.ZoomPoint.y

		x1-=Layer.Viewport.min.x/Layer.Scale.x
		y1-=Layer.Viewport.min.y/Layer.Scale.y
	
		x2-=( Layer.ZoomPoint.x-Layer.RotationPoint.x )
		y2-=( Layer.ZoomPoint.y-Layer.RotationPoint.y )
	
		Local angle:=ATan2( y1-y2,x1-x2 )+Layer.Rotation*RotationMode
		Local distance:=Sqrt( Pow( y1-y2,2 )+Pow( x1-x2,2 ) )

		x2+=Cos( angle )*distance
		y2+=Sin( angle )*distance

		x2/=Layer.Zoom
		x2+=Layer.ZoomPoint.x-Layer.ZoomPoint.x/Layer.Zoom
		x2-=Layer.Location.x

		y2/=Layer.Zoom
		y2+=Layer.ZoomPoint.y-Layer.ZoomPoint.y/Layer.Zoom
		y2-=Layer.Location.y	

		Return PointInsideRect( WorldX,WorldY,WorldWidth,WorldHeight,x2,y2,WorldRotation*RotationMode,Handle.x,Handle.y )

	End

	Method PointInside:Bool( location:Vec2f ) Virtual
		Return PointInside( location.x,location.y )
	End
	
	Method Remove() Virtual

		If Tattooed=True
			If DisableIfTattooedOnRemove=True Enabled=False
			Return
		Endif

		If _layer<>Null And _layer.CurrentGuiObject=Self _layer.CurrentGuiObject=Null

		If _layer<>Null Layer=Null
		If _group<>Null Group=Null

	End

	Method RenderState:Int( fallback:Int )

		Local renderState:=GuiState.Idle

		If Ghosted( Ghost )

			If RenderStateAllowed( GuiState.Ghost ) renderState=GuiState.Ghost

		Else

			If KeyboardReceiver=Self And RenderStateAllowed( GuiState.Selected ) renderState=GuiState.Selected

			If Down
				If RenderStateAllowed( GuiState.Down ) renderState=GuiState.Down
			Else
				If Over And RenderStateAllowed( GuiState.RollOver ) renderState=GuiState.RollOver
			Endif

		End If

		Return renderState

	End

	Method RenderStateAllowed:Bool( renderState:Int )

		For Local i:=0 Until RenderStates.Length
			If RenderStates[i]=renderState Return True
		Next

		Return False

	End

	Method SendMessage( data:String )

		If Name="" Return

		If data="" Return

		Local message:=""

		Local groups:=Groups

		If groups
			For Local i:=groups.Length-1 To 0 Step -1
				If groups[i].Name<>""
					If message<>"" message+="|"
					message+=groups[i].Name
				Endif
			Next
		Endif

		If message<>"" message+="|"

		message+=Name
		message+="="
		message+=data

		Message.Create( message,Self )

	End

	#rem monkeydoc @hidden
	#end
	Method SetSkin( data:Config ) Virtual

		Local enabled:=data.ReadBool( "enabled",Enabled )
		Local name:=data.ReadString( "name",Name )
		Local x:=data.ReadFloat( "x",X )
		Local y:=data.ReadFloat( "y",Y )
		Local z:=data.ReadFloat( "z",Z )

		Enabled=enabled
		Name=name
		X=x
		Y=y
		Z=z

	End

	#rem monkeydoc @hidden
	#end
	Method Snapshot:String() Virtual

		If Name="" Return ""

		Local data:=""

		data+="name="+Name+"~n"

		If Enabled=False data+="enabled=False"+"~n"
		If X<>0 data+="x="+X+"~n"
		If Y<>0 data+="y="+Y+"~n"

		Return data

	End
	
	#rem monkeydoc @hidden
	#end
	Method __DrawSelected( canvas:Canvas )

		If EditorGui.SelectedType=0 Return

		canvas.BlendMode=EditorGui.SelectedBlendMode
		canvas.Color=EditorGui.SelectedColor

		canvas.PushMatrix()

		canvas.Translate( X,Y )
		canvas.Rotate( Rotation*RotationMode )

		Select EditorGui.SelectedType
			Case 1
				canvas.DrawRect( -Width*HandleX,-Height*HandleY,ScaledWidth,ScaledHeight )
			Case 2
				DrawRect( canvas,-Width*HandleX,-Height*HandleY,ScaledWidth,ScaledHeight )
		End Select

		canvas.PopMatrix()

	End

	#rem monkeydoc @hidden
	#end
	Method __DrawDebugSelected( canvas:Canvas )

		canvas.BlendMode=DebugGui.SelectedBlendMode
		canvas.Color=DebugGui.SelectedColor

		canvas.PushMatrix()

		canvas.Translate( X,Y )
		canvas.Rotate( Rotation*RotationMode )

		DrawRect( canvas,-Width*HandleX,-Height*HandleY,Width,Height,-2 )

		canvas.PopMatrix()
		
	End

	#rem monkeydoc @hidden
	#end
	Method __OnReleased() Virtual
	End

	#rem monkeydoc @hidden
	#end
	Method __SendKeyEvent( event:KeyEvent ) Virtual
		OnKeyEvent( event )
	End

	#rem monkeydoc @hidden
	#end
	Method __Update() Virtual
	End

	#rem monkeydoc @hidden
	#end
	Method __UpdateCurrentGuiObject() Virtual
		If Pause()=True And Pausable=True Return
		If Layer=Null Return
		If Layer.CurrentGuiObject<>Null Return
		If RememberAsTopObject=False Return
		If Enabled=False Return
		''If Visible=False Return
		If TouchMode()=False And PointInside( Layer.View.MouseLocation )=True Layer.CurrentGuiObject=Self
		If MouseHit( MouseButton.Left )=True And PointInside( Layer.View.MouseLocation )=True Layer.CurrentGuiObject=Self
	End

	Method __UpdateState() Virtual

		If Layer=Null Return

		If MouseHit( MouseButton.Left )=True And _busy=False And Down=False

			If PointInside( Layer.View.MouseLocation )=True

				OnHit()
				SendMessage( "Hit" )
				Hit=True

			Endif

			Down=True
			_busy=True

		Endif

		If Layer.InputDevice.ButtonReleased[MouseButton.Left]=True And _busy=True

			If PointInside( Layer.View.MouseLocation )=True 
				__OnReleased()
				OnReleased()
				SendMessage( "Released" )
				Released=True
			Endif

			Down=False
			_busy=False
								
		Endif

		If _busy=True

			If ( PointInside( Layer.View.MouseLocation )=False And DownUntilRelease=False )

				Down=False

			Else

				Down=True

			Endif
			
		Endif

		If TouchMode()=False And PointInside( Layer.View.MouseLocation )=True

			If Over=False
				OnEntered()
				SendMessage( "Entered" )
				Entered=True
			Endif

			OnRollOver()
			''SendMessage( "RollOver" )
			Over=True

		Endif

	End

	Private

	Method _UpdateWorldData()

		_worldLocation.x=Location.x
		_worldLocation.y=Location.y
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

			Local angle:=ATan2( _worldLocation.y-group.Location.y,_worldLocation.x-group.Location.x )-group.Rotation*RotationMode
			Local distance:=Sqrt( Pow( _worldLocation.y-group.Location.y,2 )+Pow( _worldLocation.x-group.Location.x,2 ) )

			_worldLocation.x=group.Location.x+Cos( angle )*distance
			_worldLocation.y=group.Location.y+Sin( angle )*distance

			group=group.Group

		Wend

	End

	Field _busy:=False
	Field _group:GuiGroup
	Field _height:=0.0
	Field _layer:GuiLayer
	Field _width:=0.0
	Field _worldHeight:=0.0
	Field _worldLocation:=New Vec2f
	Field _worldRotation:=0.0
	Field _worldScale:=New Vec2f
	Field _worldWidth:=0.0

End

#rem monkeydoc @hidden
#end
Class GuiPanel Extends GuiGroup

	Field Header:=New ImageSet
	Field Label:=New TextSet

	Method Draw( canvas:Canvas ) Override

		__DrawSurface( canvas )

		Local renderState:=RenderState( GuiState.Idle )
		If renderState=-1 Return

		Local headerImage:=Header.DrawData( GuiState.Idle ).Image

		If Header.DrawData( renderState ).Image<>Null headerImage=Header.DrawData( renderState ).Image

		If headerImage<>Null
			canvas.PushMatrix()
			Header.DrawData( renderState,Frame ).InitDraw( canvas,Location.x,Location.y,ScaledWidth,ScaledHeight,Rotation*RotationMode,Handle.x,Handle.y,Brightness( Color ) )
			Draw9Patch( canvas,headerImage,Header.PatchData,0,0,Header.Width,Header.Height,0,Scale.x,Scale.y,0,0 )
			canvas.PopMatrix()
		Endif

		If Label.Text<>"" And Label.Font<>Null
			canvas.PushMatrix()
			Label.DrawData( renderState,Frame ).InitDraw( canvas,Location.x,Location.y,ScaledWidth,ScaledHeight,Rotation*RotationMode,Handle.x,Handle.y,Brightness( Color ) )
			canvas.Font=Label.Font
			canvas.Scale( Scale.x,Scale.y )
			canvas.DrawText( Label.Text,Label.X,Label.Y )
			canvas.PopMatrix()
		Endif

		__Draw( canvas )

		If Selected __DrawSelected( canvas )

		If Layer And Layer.DebugEditor And Layer._sticky=Self __DrawDebugSelected( canvas )

	End

End

#rem monkeydoc The Rectangle class.
#end
Class GuiRectangle Extends GuiObject

	Field LineWidth:=1

	Method New()
	End
	
	Method New( guiLayer:GuiLayer )
		Layer=guiLayer
	End

	Method New( guiLayer:GuiLayer,width:Int,height:Int )
		Layer=guiLayer
		_height=height
		_width=width
	End

	Method New( width:Int,height:Int )
		_height=height
		_width=width
	End
	
	Method New( guiGroup:GuiGroup )
		Group=guiGroup
	End

	Method New( guiGroup:GuiGroup,width:Int,height:Int )
		Group=guiGroup
		_height=height
		_width=width
	End

	Method Draw( canvas:Canvas ) Override

		canvas.PushMatrix()

		canvas.Translate( Location.x,Location.y )
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

		If Layer And Layer.DebugEditor And Layer._sticky=Self __DrawDebugSelected( canvas )

	End

End

#rem monkeydoc @hidden
#end
Class GuiTabber Extends GuiGroup

	#rem monkeydoc @hidden
	#end
	Class Tab Extends GuiButton
	End

	Property Document:Int()
		For Local i:=0 Until _document.Length
			If _document[i].Enabled=True Return i
		Next
		Return -1
	End

	Method GetDocument:GuiGroup( index:Int )
		Return _document[index]
	End

	Method GetDocumentName:String( index:Int )
		Return _documentTab[index].Name
	End

	Method GetDocumentNames:String[]()
		Local name:=New String[_document.Length]
		For Local i:=0 Until _document.Length
			name[i]=_documentTab[i].Name
		Next
		Return name
	End

	Method SetDocument( index:Int )
		For Local i:=0 Until _document.Length
			If i=index
				_document[i].Enabled=True
				_document[i].Group=_document[i].Group
				_documentTab[i].Group=_documentTab[i].Group
				_documentTab[i].Frame=0
			Else
				_document[i].Enabled=False
				_documentTab[i].Frame=1
			Endif
		Next
	End

	#rem monkeydoc @hidden
	#end
	Method __Update() Override

		Super.__Update()

		For Local i:=0 Until _documentTab.Length
			If _documentTab[i].Released SetDocument( i )
		Next

	End

	Field _document:GuiGroup[]
	Field _documentTab:GuiTabber.Tab[]

End

#rem monkeydoc The GuiScrollBox class.
#end
Class GuiScrollBox Extends GuiGroup

	Field ClearColor:=New Color( 0,0,0,1 )
	Field HorizontalSlider:GuiSlider
	Field SliderSpace:=0
	Field VerticalSlider:GuiSlider

	Method New()
	End

	Method New( width:Int,height:Int )
		_width=width
		_height=height
		_UpdateCanvas( width,height )
	End

	Method New( layer:GuiLayer,width:Int,height:Int )
		Layer=layer
		_width=width
		_height=height
		_UpdateCanvas( width,height )
	End

	Method New( group:GuiGroup,width:Int,height:Int )
		Group=group
		_width=width
		_height=height
		_UpdateCanvas( width,height )
	End

	Method LoadScript( filePath:String )

		Local content:=New ContentManager

		Local color:=Color.White 
		Local font:Font
		Local x:=0
		Local y:=0

		Local lastObj:GuiObject

		Local obj:=New Config( filePath )

		For Local i:=0 Until obj.Data.Length

			If lastObj<>Null
				y+=lastObj.Height*.5
				lastObj=Null
			Endif

			Local line:=obj.Data[i].Split( "=" )

			line[0]=line[0].ToLower().Trim()

			Select line[0]

				Case "clearcolor"

					Local data:=line[1].Split( "," )

					If line[1].Contains( "%" )

						If data.Length=3
							ClearColor=New Color( Float( data[0].Replace("%","") )/100,Float( data[1].Replace("%","") )/100,Float( data[2].Replace("%","") )/100 )
						Elseif data.Length=4
							ClearColor=New Color( Float( data[0].Replace("%","") )/100,Float( data[1].Replace("%","") )/100,Float( data[2].Replace("%","") )/100,Float( data[3].Replace("%","") )/100 )
						Endif

					Else

						If data.Length=3
							ClearColor=glColor( Float( data[0] ),Float( data[1] ),Float( data[2] ) )
						Elseif data.Length=4
							ClearColor=glColor( Float( data[0] ),Float( data[1] ),Float( data[2] ),Float( data[3] ) )
						Endif

					Endif

				Case "color"

					Local data:=line[1].Split( "," )

					If line[1].Contains( "%" )

						If data.Length=3
							color=New Color( Float( data[0].Replace("%","") )/100,Float( data[1].Replace("%","") )/100,Float( data[2].Replace("%","") )/100 )
						ElseIf data.Length=4
							color=New Color( Float( data[0].Replace("%","") )/100,Float( data[1].Replace("%","") )/100,Float( data[2].Replace("%","") )/100,Float( data[3].Replace("%","") )/100 )
						Endif

					Else

						If data.Length=3
							color=glColor( Float( data[0] ),Float( data[1] ),Float( data[2] ) )
						Elseif data.Length=4
							color=glColor( Float( data[0] ),Float( data[1] ),Float( data[2] ),Float( data[3] ) )
						Endif

					Endif

				Case "font"

					Local data:=line[1].Split( "," )

					font=content.GetFont( data[0],Int( data[1] ) )

				Case "label"

					Local label:=New GuiLabel( Self,font,line[1] )

					y+=label.Height*.5
					
					label.X=x
					label.Y=y

					label.Color=color

					lastObj=label

				Case "textfield"

					Local width:=0

					Local data:=Split( line[1],"," )
					
					If data.Length=2
						If data[1].Contains( "%" ) width=Width*Float( data[1].Replace("%","") )/100 Else width=Int( data[1].Replace("%","") )
					Else
						width=Width
					Endif

					Local text:=FitText( font,data[0],width )

					For Local i:=0 Until text.Length 

						Local label:=New GuiLabel( Self,font,text[i].ToUpper() )

						y+=label.Height

						label.X=x
						label.Y=y

						label.Color=color

						lastObj=label

					Next

				Case "image"

					Local image:=New GuiImage( Self,content.GetImage( line[1] ) )

					y+=image.Height*.5

					image.X=x
					image.Y=y

					image.Color=color

					lastObj=image

				Case "x"
					
					If line[1].Contains( "%" ) x=Width*Float( line[1].Replace("%","") )/100 Else x=Int( line[1] )
	
				Case "x+"
					
					x+=Int( line[1] )
	
				Case "x-"
					
					x-=Int( line[1] )
	
				Case "y"
					
					If line[1].Contains( "%" ) y=Height*Float( line[1].Replace("%","") )/100 Else y=Int( line[1] )
	
				Case "y+"
					
					y+=Int( line[1] )
	
				Case "y-"
					
					y-=Int( line[1] )
	
			End select
			
		Next

	End

	Method Discard()
		_image.Discard()
	End

	Method Draw( canvas:Canvas ) Override

		Local width:=_width
		Local height:=_height

		If width=0 Or height=0 Return

		If VerticalSlider<>Null And VerticalSlider.Enabled=True
			width-=VerticalSlider.Width+SliderSpace
		Endif

		If HorizontalSlider<>Null And HorizontalSlider.Enabled=True
			height-=HorizontalSlider.Height+SliderSpace
		Endif

		If width<>_image.Width Or height<>_image.Height
			_UpdateCanvas( width,height )
			_UpdateSliders()
		Endif

		_canvas.PushMatrix()
		_canvas.Clear( Brightness( ClearColor ) )
''		_canvas.Color=Brightness( Color )
''		_canvas.BlendMode=mojo.graphics.BlendMode.Alpha
		_canvas.Translate( -Camera.x,-Camera.y )
		__DrawMembers( _canvas )
		_canvas.PopMatrix()
		_canvas.Flush()

		canvas.Color=Brightness( Color )
		canvas.BlendMode=BlendMode

		DrawImage( canvas,_image,Location.x,Location.y,Rotation*RotationMode,Scale.x,Scale.y,Handle.x,Handle.y )

		If Selected __DrawSelected( canvas )

		If Layer And Layer.DebugEditor And Layer._sticky=Self __DrawDebugSelected( canvas )

	End

	#rem monkeydoc @hidden
	#end
	Method __Update() Override

		_UpdateSliders()
	
		Super.__Update()

	End

	Method _UpdateCanvas( width:Int,height:Int )

		If _image _image.Discard()

		_image=New Image( width,height )
		_canvas=New Canvas( _image )

	End

	Method _UpdateSliders()

		If HorizontalSlider<>Null

			HorizontalSlider.Knob.Maximum=GetContentWidth()-_image.Width
			Camera.x=HorizontalSlider.Knob.Value
			If GetContentWidth()>_image.Width HorizontalSlider.Enabled=True Else HorizontalSlider.Enabled=False

			If SliderSpace>0
				HorizontalSlider.X=X
				HorizontalSlider.Y=Y+_image.Height+SliderSpace
			Endif

		Endif

		If VerticalSlider<>Null

			VerticalSlider.Knob.Maximum=GetContentHeight()-_image.Height
			Camera.y=VerticalSlider.Knob.Value
			If GetContentHeight()>_image.Height VerticalSlider.Enabled=True Else VerticalSlider.Enabled=False

			If SliderSpace>0
				VerticalSlider.X=X+_image.Width+SliderSpace
				VerticalSlider.Y=Y
			Endif
			
		Endif

	End

	Field _canvas:Canvas
	Field _image:Image

End

#rem monkeydoc @hidden
#end
Class GuiSlider Extends GuiGroup

	Field Button:=New GuiButton[2]
	Field Knob:GuiSliderKnob

	Method __Update() Override

		Super.__Update()

		If Button[0] And Button[0].Down Knob.Value-=1
		If Button[1] And Button[1].Down Knob.Value+=1

		If Knob.Value<0 Knob.Value=0
		If Knob.Value>Knob.Maximum Knob.Value=Knob.Maximum

	End

End

#rem monkeydoc The GuiSliderKnob class.
#end
Class GuiSliderKnob Extends GuiObject

	Method New()
		DownUntilRelease=True
	End

	Property Height:Float() Override
		Return _surface.Height
	Setter( height:Float ) Override
		_surface.Height=height
	End

	Property Length:Int()
		Return _length
	Setter( length:Int )
		_length=length
	End

	Property Maximum:Int()
		Return _maximum
	Setter( maximum:Int )
		_maximum=maximum
	End

	Property Type:Int()
		Return _type
	Setter( type:Int )
		_type=type
	End

	Property Surface:ImageSet()
		Return _surface
	End

	Property Value:Float()
		Return Float( _maximum )/_length*_pos
	Setter( value:Float )
		_pos=value/_maximum*_length
	End

	Property ValueInt:Int()
		Return Round( Float( _maximum )/_length*_pos )
	End
	
	Property Width:Float() Override
		Return _surface.Width
	Setter( width:Float ) Override
		_surface.Width=width
	End

	Method Draw( canvas:Canvas ) Override

		Local renderState:=RenderState( GuiState.Idle )
		If renderState=-1 Return

		Local surfaceImage:=_surface.DrawData( GuiState.Idle ).Image

		If _surface.DrawData( renderState ).Image<>Null surfaceImage=_surface.DrawData( renderState ).Image

		If surfaceImage<>Null
			canvas.PushMatrix()
			_surface.DrawData( renderState,Frame ).InitDraw( canvas,Location.x,Location.y,ScaledWidth,ScaledHeight,Rotation,Handle.x,Handle.y,Brightness( Color ) )
			If _type=1 Draw9Patch( canvas,surfaceImage,_surface.PatchData,_pos/_surface.DrawData( renderState ).ScaleX,0,Width,Height,0,Scale.x,Scale.y,0,0 )
			If _type=2 Draw9Patch( canvas,surfaceImage,_surface.PatchData,0,_pos/_surface.DrawData( renderState ).ScaleY,Width,Height,0,Scale.x,Scale.y,0,0 )
			canvas.PopMatrix()
		Endif

		If Selected __DrawSelected( canvas )

		If Layer And Layer.DebugEditor And Layer._sticky=Self __DrawDebugSelected( canvas )

	End

	Method PointInside:Bool( location:Vec2f ) Override

		Local x:=location.x
		Local y:=location.y

		If _type=1 x-=_pos
		If _type=2 y-=_pos

		Return PointInside( x,y )

	End

	#rem monkeydoc @hidden
	#end
	Method __Update() Override

		Local x:=Layer.View.MouseLocation.x
		Local y:=Layer.View.MouseLocation.y

		If _type=1
			_speed=x-_previous
			_previous=x
		Endif

		If _type=2
			_speed=y-_previous
			_previous=y
		Endif

 		If Down

 			If _type=1 _pos+=_speed
 			If _type=2 _pos+=_speed

			If _pos<0 Value=0

			If _pos>_length Value=_maximum

 		Endif

	End

	Private

	Field _length:=100
	Field _maximum:=1000
	Field _previous:=0.0
	Field _speed:=0.0
	Field _surface:=New ImageSet
	Field _type:=1
	Field _pos:=0.0

End
