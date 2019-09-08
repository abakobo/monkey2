Namespace pyro.framework.loaderscreen

Using pyro.framework.screenmanager

#rem monkeydoc The LoaderScreen class.
#end
Class LoaderScreen Extends Screen
	
	#Rem monkeydoc Returns the current loading step.
	#End
	Property CurrentStep:Int()
		Return _currentStep
	End

	#Rem monkeydoc The splash screen needs to know what screen to display after loading is done.
	#End
	Property FollowUpScreen:Screen()
		Return _followUpScreen
	Setter( followUpScreen:Screen )
		_followUpScreen=followUpScreen
	End

	#Rem monkeydoc Returns the number of loading steps.
	#End
	Property LoadingSteps:Int()
		Return _loadingSteps
	End

	#rem monkeydoc @hidden
	#end
	Method Draw( canvas:Canvas)

		OnLoading()

		If _loaded

			If _currentStep>=_loadingSteps

				_currentStep=_loadingSteps

				If _followUpScreen _followUpScreen.Set()

			Else

				_allow=True
	
				_tracker=0

			Endif

		Else

			_loaded=ResizeArray( _loaded,_loadingSteps )

		Endif

	End

	#Rem monkeydoc Returns true while loading.
	#End
	Method Loading:Bool()

		If _loaded

			If _allow And Not _loaded [_tracker]
				_allow=False
				_loaded [_tracker]=True
				_currentStep+=1
				Return True
			Endif
	
			_tracker+=1

		Else

			_loadingSteps+=1

		Endif

		Return False
		
	End

	Method OnLoading() Virtual
	End

	Method OnRender( canvas:Canvas ) Override
		Draw( canvas )
	End

#Rem monkeydoc Sets the loading steps and current loading steps to 0.

This needs to be done before the splash screen is reused.
#End
	Method Reset()

		_loadingSteps=0
		_currentStep=0

		If _loaded.Length>0	_loaded=ResizeArray( _loaded,0 )

	End

	Private

	Field _allow:=False
	Field _currentStep:=0
	Field _followUpScreen:Screen
	Field _loaded:Bool[]
	Field _loadingSteps:=0
	Field _tracker:=0

End

