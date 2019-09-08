Namespace pyro.framework.commoncode

#rem monkeydoc
Set bit of an integer value.
#end
Function BitSet:Int( bit:Int,value:Int )
	Return value|1 Shl bit
End

#rem monkeydoc
Test bit of an integer value.
#end
Function BitTest:Bool( bit:Int,value:Int )
	If 1 Shl bit & value Return True
	Return False
End

#rem monkeydoc @hidden
#end
Function BoolToString:String( data:Bool )
	If data=True Return "True"
	Return "False"
End

#rem monkeydoc @hidden
#end
Function Compare:Bool( data:Int,dataRange:Int[] )
	For Local i:=0 Until dataRange.Length
		If dataRange[i]=data Return True
	Next
	Return False
End

#rem monkeydoc @hidden
#end
Function Compare:Bool( data:String,dataRange:String[] )
	For Local i:=0 Until dataRange.Length
		If dataRange[i]=data Return True
	Next
	Return False
End

#rem monkeydoc
'Freezes' your app for the amount of time given.

Useful for debugging your app.
#end
Function Delay( time:Int )

	Local delay:=Millisecs()+time
	While Millisecs()<delay
	Wend
	
End

#rem monkeydoc
Calculates the frames per second.
#end
Function FPS:Int()

	If _fpsT
	
		_fpsC=_fpsC+1
	
		If Millisecs()>=_fpsT
	
			_fps=_fpsC
	
			_fpsT=Millisecs()+1000
	
			_fpsC=0
	
		Endif
	
	Else
	
		_fpsT=Millisecs()+1000
	
	Endif
	
	Return _fps

End

#rem monkeydoc @hidden
#end
Function MeasureTime:Int()
	Global _start:=0
	Local time:=0
	If _start=0
		_start=Millisecs()
	Else
		time=Millisecs()-_start
		_start=0
	Endif
	Return time
End

#rem monkeydoc @hidden
#end
Function PercentageOf:Int( percentage:Float, value:Float )
	Return percentage/100*value
End

#rem monkeydoc @hidden
#end
Function ToBlendMode:BlendMode( blendMode:String )

	blendMode=blendMode.ToLower()

	Select blendMode
		Case "opaque"
			Return BlendMode.Opaque
		Case "alpha"
			Return BlendMode.Alpha
		Case "multiply"
			Return BlendMode.Multiply
		Case "additive"
			Return BlendMode.Additive
	End Select

	Return BlendMode.Alpha

End

#rem monkeydoc @hidden
#end
Function ToBlendMode:BlendMode( blendMode:Int )

	Select blendMode
		Case BlendMode.Opaque
			Return BlendMode.Opaque
		Case BlendMode.Alpha
			Return BlendMode.Alpha
		Case BlendMode.Multiply
			Return BlendMode.Multiply
		Case BlendMode.Additive
			Return BlendMode.Additive
	End Select

	Return BlendMode.Alpha

End

Private

Global _fpsT:=0
Global _fpsC:=0
Global _fps:=0
