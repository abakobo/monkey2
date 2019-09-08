
Namespace ted2go


Class WindowActions

	Field nextTab:Action
	Field prevTab:Action
	Field zoomIn:Action
	Field zoomOut:Action
	Field zoomDefault:Action
	Field themes:Action
	Field fullscreenWindow:Action
	Field fullscreenEditor:Action
	
	Method New( docs:DocumentManager )
		
		nextTab=docs.nextDocument
		prevTab=docs.prevDocument
		
		fullscreenWindow=ActionById( ActionId.FullscreenWindow )
		fullscreenWindow.Triggered=MainWindow.SwapFullscreenWindow
		
		fullscreenEditor=ActionById( ActionId.FullscreenEditor )
		fullscreenEditor.Triggered=Lambda()
			MainWindow.SwapFullscreenEditor()
		End
		
		zoomIn=ActionById( ActionId.ZoomIn )
		zoomIn.Triggered=Lambda()
		
			Local sc:=App.Theme.Scale.x
			If sc>=4 Return
			sc+=.125
			App.Theme.Scale=New Vec2f( sc )
		End
		
		zoomOut=ActionById( ActionId.ZoomOut )
		zoomOut.Triggered=Lambda()
		
			Local sc:=App.Theme.Scale.x
			If sc<=.5 Return
			sc-=.125
			App.Theme.Scale=New Vec2f( sc )
		End
		
		zoomDefault=ActionById( ActionId.ZoomDefault )
		zoomDefault.Triggered=Lambda()
		
			App.Theme.Scale=New Vec2f( 1 )
		End
		
	End
	
End
