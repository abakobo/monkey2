Namespace pyro.framework.screenmanager

Using pyro.framework.gfx
Using pyro.framework.taskmanager

#rem monkeydoc The Screen class.
#end
Class Screen

	Method New()
		Set()
	End

	Property ScreenManager:ScreenManager()
		Return _screenManager
	End

	Method OnKeyEvent( event:KeyEvent ) Virtual
	End

	#rem monkeydoc @hidden
	#end
	Method OnMeasure:Vec2i() Virtual
		Return New Vec2i( 0,0 )
	End
	
	Method OnMouseEvent( event:MouseEvent ) Virtual
	End

	Method OnRender( canvas:Canvas ) Virtual
	End

	Method OnStart() Virtual
	End

	Method OnStop() Virtual
	End

	Method OnUpdate() Virtual
	End

	Method OnWindowEvent( event:WindowEvent ) Virtual
	End

	Method RunOnce() Virtual
	End

	Method Set()
		If _screenManager._currentScreen<>Null And _screenManager._currentScreen._started=True
			_screenManager._stop=_screenManager._currentScreen
		Endif
		_screenManager._currentScreen=Self
	End

	#rem monkeydoc @hidden
	#end
	Method Set( screen:Screen )
		screen._returnScreen=Self
		_screenManager._currentScreen=screen
	End

	#rem monkeydoc @hidden
	#end
	Method RTS()
		If _returnScreen=Null Return
		_screenManager._currentScreen=_returnScreen
		_returnScreen=Null
		_screenManager._stop=Self
	End

	Private

	Global _parent:ScreenManager
	
	Field _runOnce:=True
	Field _returnScreen:Screen
	Field _screenManager:=_parent
	Field _started:=False

End

#rem monkeydoc The ScreenManager class.
#end
Class ScreenManager Extends Window

	Field FadeColor:=Color.Black

	Method New( title:String="Window",width:Int=640,height:Int=480,flags:WindowFlags=Null )
		Super.New( title,New Recti( 0,0,width,height ),flags|WindowFlags.Center )
		Screen._parent=Self
	End

	Method New( title:String,rect:Recti,flags:WindowFlags=Null )
		Super.New( title,rect,flags )
		Screen._parent=Self
	End

	#rem monkeydoc @hidden
	#end
	Property CurrentScreen:Screen()
		Return _currentScreen
	End

	Method Fade( screen:Screen,speed:Float=.02 )
		If _fadeScreen<>Null Return
		_fadeScreen=screen
		_fadeSpeed=-Abs( speed )
	End

	#rem monkeydoc @hidden
	#end
	Method OnKeyEvent( event:KeyEvent ) Override
		If _currentScreen<>Null _currentScreen.OnKeyEvent( event )
	End

	Method OnMeasure:Vec2i() Override
	
		If _currentScreen<>Null Return _currentScreen.OnMeasure()
		
		Return New Vec2i( 0,0 )
		
	End

	#rem monkeydoc @hidden
	#end
	Method OnMouseEvent( event:MouseEvent ) Override
		If _currentScreen<>Null _currentScreen.OnMouseEvent( event )
	End

	#rem monkeydoc @hidden
	#end
	Method OnRender( canvas:Canvas ) Override

		If Minimized Return

		App.RequestRender()

		If _currentScreen._runOnce=True
			_currentScreen._runOnce=False
			_currentScreen.RunOnce()
		Endif
		
		If _currentScreen._started=False
			_currentScreen._started=True
			_currentScreen.OnStart()
		Endif

		_currentScreen.OnUpdate()

		If _stop

			Pause( False )
			_stop._started=False
			_stop.OnStop()
			_stop=Null

		Elseif ( _currentScreen._runOnce=False And _currentScreen._started=True )

			_currentScreen.OnRender( canvas )

		Endif

		If _fadeScreen<>Null
			
			DrawFader( canvas,_fade,FadeColor )
		
			_fade+=_fadeSpeed

			If _fade<0
				_fade=0
				_fadeSpeed=Abs( _fadeSpeed )
				_fadeScreen.Set()
			Endif
			
			If _fade>1
				_fade=1
				_fadeSpeed=0
				_fadeScreen=Null
			Endif

		Endif
			

	End

	#rem monkeydoc @hidden
	#end
	Method OnWindowEvent( event:WindowEvent ) Override
		If _currentScreen<>Null _currentScreen.OnWindowEvent( event )
		Super.OnWindowEvent( event )
	End

	Private

	Field _currentScreen:Screen
	Field _fade:=1.0
	Field _fadeScreen:Screen
	Field _fadeSpeed:=0.0
	Field _stop:Screen
	
End
