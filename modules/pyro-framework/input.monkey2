Namespace pyro.framework.input

Class InputDevice

	Field ButtonDown:=New Bool[4]
	Field ButtonHit:=New Bool[4]
	Field ButtonReleased:=New Bool[4]
	Field ButtonState:=New Int[4]

	Method Update()
		_UpdateMouse( MouseButton.Left )
		_UpdateMouse( MouseButton.Middle )
		_UpdateMouse( MouseButton.Right )
	End
	
	Private
	
	Method _UpdateMouse( mouseButton:MouseButton )

		ButtonHit[mouseButton]=False
		ButtonReleased[mouseButton]=False

		ButtonDown[mouseButton]=Mouse.ButtonDown( mouseButton )

		If ButtonDown[mouseButton]=True And ButtonState[mouseButton]=0
			ButtonHit[mouseButton]=True
			ButtonState[mouseButton]=1
		Endif

		If ButtonDown[mouseButton]=False And ButtonState[mouseButton]=1
			ButtonReleased[mouseButton]=True
			ButtonState[mouseButton]=0
		Endif

	End
	
End

#rem monkeydoc @hidden
#end
Function FingerCount:Int()
	Local fingers:=0
	For Local i:=0 Until 5
		If Touch.FingerDown( i )=True fingers=i+1
	Next
	Return fingers
End

#rem monkeydoc @hidden
#end
Function GetTouchIndex:Int()
	For Local i:=0 Until 5
		If Touch.FingerDown( i )=True Return i
	Next
	Return -1
End

#rem monkeydoc @hidden
#end
Function TouchMode:Bool()

	#If __TARGET__="android"
		Return True
	#Else if __TARGET__="ios"
		Return False
	#Else
		Return False
	#End

End

#rem
Function KeyRepeat:Bool( key:Key,delay:Int=25 )

	Global keyDelays:=New Int[512]

	Local k:=Int( key )

	If Keyboard.KeyPressed( key ) Return True

	If Keyboard.KeyDown( key )

		keyDelays[k]+=1

		If keyDelays[k]>=delay Return True
	
	Else
	
		keyDelays[k]=0

	Endif

	Return False

End
#end