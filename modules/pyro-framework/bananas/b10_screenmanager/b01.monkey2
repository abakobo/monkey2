#Import "<std>"
#Import "<mojo>"
#Import "<pyro-framework>"

Using std..
Using mojo..
Using pyro.framework..

Global game:Game
Global menu:Menu

Class Game Extends Screen

	Method RunOnce() Override
		Print "Game initialized"
	End

	Method OnKeyEvent( event:KeyEvent ) Override
	End

	Method OnMouseEvent( event:MouseEvent ) Override
	End

	Method OnStart() Override
		Print "Game started"
	End

	Method OnStop() Override
		Print "Game stopped"
	End

	Method OnRender( canvas:Canvas ) Override

		canvas.Clear( New Color( .25,0,0,1 ) )
		canvas.DrawText( "Game screen",0,0 )

	End

	Method OnUpdate() Override
		If Keyboard.KeyHit( Key.Space )
'			menu.Set()								' Instant screen change!
			ScreenManager.Fade( menu )				' Screen change with fade out/in
		Endif
	End

End

Class Menu Extends Screen

	Method RunOnce() Override
		Print "Menu initialized"
	End

	Method OnKeyEvent( event:KeyEvent ) Override
	End

	Method OnMouseEvent( event:MouseEvent ) Override
	End

	Method OnStart() Override
		Print "Menu started"
	End

	Method OnStop() Override
		Print "Menu stopped"
	End

	Method OnRender( canvas:Canvas ) Override

		canvas.Clear( New Color( 0,.25,0,1 ) )
		canvas.DrawText( "Menu screen",0,0 )

	End

	Method OnUpdate() Override
		If Keyboard.KeyHit( Key.Space )
'			game.Set()								' Instant screen change!
			ScreenManager.Fade( game )				' Screen change with fade out/in
		Endif
	End

End

Class PyroExample Extends ScreenManager

	Method New()

		' You can chnage the FadeColor if you like ( default = black ):
		'FadeColor=Color.White

		Print "Use space to switch screens!"

		game=New Game
		menu=New Menu
'		game.Set()								' Start with to game screen.

	End
	
End

Function Main()

	New AppInstance
	
	New PyroExample
	
	App.Run()

End
