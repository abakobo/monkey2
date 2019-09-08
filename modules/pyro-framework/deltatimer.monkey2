Namespace pyro.framework.deltatimer

#rem monkeydoc Delta timer class.
#end
Class DeltaTimer

	#rem monkeydoc Returns the delta time.
	#end
	Field DeltaTime:=0.0
	#rem monkeydoc Returns the elapsed delta time in seconds.
	#end
	Field ElapsedDelta:=0.0
	#rem monkeydoc Returns the elapsed time in seconds.
	#end
	Field ElapsedTime:=0.0
	#rem monkeydoc Returns the elapsed frame time in milliseconds.
	#end
	Field FrameTime:=0.0
	#rem monkeydoc Returns the target frames per second.
	#end
	Field TargetFPS:=60.0
	#rem monkeydoc Returns the time scale.
	#end
	Field TimeScale:=1.0

	Method New()
	End

	Method New( targetFPS:Float )
		TargetFPS=targetFPS
		_lastTicks=Millisecs()
	End

	#rem monkeydoc Call this when your application resumes after a suspend.
	#end
	Method Resume()
		_lastTicks=Millisecs()
	End
	
	#rem monkeydoc Updates the delta timer.
	#end
	Method Update()
		FrameTime=Millisecs()-_lastTicks
		_lastTicks=Millisecs()
		ElapsedTime+=FrameTime
		DeltaTime=FrameTime/( 1000.0/TargetFPS )*TimeScale
		ElapsedDelta=ElapsedDelta+( DeltaTime/TargetFPS )
	End

	Private

	Field _lastTicks:=0

End