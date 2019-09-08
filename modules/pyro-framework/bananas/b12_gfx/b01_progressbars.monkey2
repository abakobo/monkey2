' Really simple progressbars!

#Import "<std>"
#Import "<mojo>"
#Import "<pyro-framework>"

Using std..
Using mojo..
Using pyro.framework..

Class PyroExample Extends Window

	Field hval:=100
	Field hmax:=200
	
	Field vval:=250
	Field vmax:=500

	Method OnRender( canvas:Canvas ) Override
	
		App.RequestRender()

		If Keyboard.KeyDown( Key.Left ) hval-=1
		If Keyboard.KeyDown( Key.Right ) hval+=1

		If Keyboard.KeyDown( Key.Up ) vval-=1
		If Keyboard.KeyDown( Key.Down ) vval+=1

		If hval<0 hval=0
		If hval>hmax hval=hmax

		If vval<0 vval=0
		If vval>vmax vval=vmax

		canvas.Clear( Color.Black )

		DrawHorizontalProgressBar( canvas,364,348,256,8,hval,hmax,New Color( .5,0,0,1 ),New Color( 1,0,0,1 ) )
		DrawVerticalProgressBar( canvas,132,248,8,256,vval,vmax )

		canvas.DrawText( "Use cursor keys to change the values.",8,8 )

	End

End

Function Main()

	New AppInstance
	
	New PyroExample
	
	App.Run()

End
