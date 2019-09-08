Namespace pyro.framework.debugtools

#rem monkeydoc @hidden
#end
Struct DebugGui

	Global BlendMode:=mojo.graphics.BlendMode.Alpha
	Global Color:=New Color( 0,.5,0,.5 )
	Global FlagColors:=New Color[]( New Color( .5,.5,.5 ),New Color( 1,0,0 ),New Color( 0,1,0 ),New Color( 0,0,1 ),New Color( 1,1,0 ),New Color( 0,1,1 ),New Color( 1,.5,0 ),New Color( 1,0,1 ) )
	Global SelectedBlendMode:=mojo.graphics.BlendMode.Alpha
	Global SelectedColor:=New Color( 0,.5,0,.5 )
	Global TextColor:=New Color( 0,1,0,.5 )

End

#rem monkeydoc @hidden
#end
Struct EditorGui

	Global SelectedBlendMode:=mojo.graphics.BlendMode.Alpha
	Global SelectedColor:=New Color( 0,0,.5,.5 )
	Global SelectedType:=1

End
