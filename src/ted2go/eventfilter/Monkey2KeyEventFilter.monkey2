
Namespace ted2go


Class Monkey2KeyEventFilter Extends TextViewKeyEventFilter

	Property Name:String() Override
		Return "Monkey2KeyEventFilter"
	End
	
	
	Protected

	Method OnFilterKeyEvent( event:KeyEvent,textView:TextView ) Override
		
		Local ctrl:=(event.Modifiers & Modifier.Control)
		Local shift:=(event.Modifiers & Modifier.Shift)
		
		Select event.Type
		Case EventType.KeyDown
			
			Select event.Key
				
				Case Key.F1
					
					MainWindow.ShowHelp( "",ctrl )
					event.Eat()
					
				Case Key.F2
				
					MainWindow.GotoDeclaration()
					event.Eat()
				
'				Case Key.F10
'				
'					Print "works"
'					Local doc:=MainWindow.DocsManager.CurrentDocument
'					If Not doc Return
'					
'					New Fiber( Lambda()
'						
'						Local cmd:=Monkey2Parser.GetParseCommand( doc.Path )
'						
'						Local str:=LoadString( "process::"+cmd )
'						Local i:=str.Find( "{" )
'						If i=-1 Return
'						str=str.Slice( i )
'						
'						Local jobj:=JsonObject.Parse( str )
'						If Not jobj Return
'						
'						Local jsonTree:=New JsonTreeView( jobj )
'						
'						Local dialog:=New Dialog( "ParseInfo",jsonTree )
'						dialog.AddAction( "Close" ).Triggered=dialog.Close
'						dialog.MinSize=New Vec2i( 512,600 )
'						
'						dialog.Open()
'						
'					End )
			End
		End
		
	End
	
	
	Private
	
	Method New()
		Super.New()
		_types=New String[]( ".monkey2" )
	End
	 
	Global _instance:=New Monkey2KeyEventFilter
	
End
