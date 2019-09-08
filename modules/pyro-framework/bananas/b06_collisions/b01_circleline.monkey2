#Import "<std>"
#Import "<mojo>"
#Import "<pyro-framework>"																					' Import pyro framework.

Using std..
Using mojo..

Using pyro.framework..

Class PyroExample Extends Window

	Method OnRender( canvas:Canvas ) Override
	
		App.RequestRender()

		Local linex1:=200
		Local liney1:=20
		Local linex2:=400
		Local liney2:=300
		
		Local circlex:=App.MouseLocation.X
		Local circley:=App.MouseLocation.Y
		Local circler:=70

		canvas.Clear( Color.Black )

		' Draw 'debug' information:
		canvas.DrawLine( linex1,liney1,linex2,liney2 )
		canvas.DrawOval( circlex-circler,circley-circler,circler*2,circler*2 )
		
		' Collision check:
		If Collision.CircleLine( circlex,circley,circler,linex1,liney1,linex2,liney2 )
			canvas.DrawText( "Collision!",0,0 )
		Endif

	End

End

Function Main()

	New AppInstance
	
	New PyroExample
	
	App.Run()

End
