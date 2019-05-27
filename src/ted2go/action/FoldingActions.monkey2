
Namespace ted2go


Class FoldingActions
	
	Field foldCurrent:Action
	Field foldScope:Action
	Field foldAll:Action
	Field unfoldCurrent:Action
	Field unfoldScope:Action
	Field unfoldAll:Action
	
	Method New()
		
		foldCurrent=ActionById( ActionId.FoldCurrent )
		foldCurrent.Triggered=OnFoldCurrent
		
		foldScope=ActionById( ActionId.FoldCurrentAndParent )
		foldScope.Triggered=OnFoldScope
		
		foldAll=ActionById( ActionId.FoldAll )
		foldAll.Triggered=OnFoldAll
		
		unfoldCurrent=ActionById( ActionId.UnfoldCurrent )
		unfoldCurrent.Triggered=OnUnfoldCurrent
		
		unfoldScope=ActionById( ActionId.UnfoldCurrentAndChildren )
		unfoldScope.Triggered=OnUnfoldScope
		
		unfoldAll=ActionById( ActionId.UnfoldAll )
		unfoldAll.Triggered=OnUnfoldAll
		
	End
	
	
	Private
	
	Property CurrentCodeDocument:CodeTextView()
		
		Return Cast<CodeTextView>( App.KeyView )
	End
	
	Method OnFoldCurrent()
		
		Local code:=CurrentCodeDocument
		If code
			code.FoldBlock( code.LineNumAtCursor,True,True )
		Endif
	End
	
	Method OnFoldScope()
		
		Local code:=CurrentCodeDocument
		If code
			Local f:=code.FindNearestFolding( code.LineNumAtCursor )
			If Not f Return
			While f And f.parent ' find root folding
				f=f.parent
			Wend
			Local all:=New Stack<CodeTextView.Folding>
			For Local i:=f.startLine Until f.endLine
				Local f2:=code.GetFolding( i )
				If f2 Then all.Add( f2 )
			Next
			For Local i:=all.Length-1 To 0 Step -1
				code.FoldBlock( all[i].startLine,(i=0) )
			Next
		Endif
	End
	
	Method OnFoldAll()
		
		CurrentCodeDocument?.FoldAll()
	End
	
	Method OnUnfoldCurrent()
		
		Local code:=CurrentCodeDocument
		If code
			code.UnfoldBlock( code.LineNumAtCursor,True,True )
		Endif
	End
	
	Method OnUnfoldScope()
		
		Local code:=CurrentCodeDocument
		If code
			Local f:=code.FindNearestFolding( code.LineNumAtCursor )
			If Not f Return
			While f And f.parent ' find root folding
				f=f.parent
			Wend
			Local all:=New Stack<CodeTextView.Folding>
			For Local i:=f.startLine Until f.endLine
				Local f2:=code.GetFolding( i )
				If f2 Then all.Add( f2 )
			Next
			For Local i:=0 Until all.Length
				code.UnfoldBlock( all[i].startLine,(i=all.Length-1) )
			Next
		Endif
	End
	
	Method OnUnfoldAll()
		
		CurrentCodeDocument?.UnfoldAll()
	End
	
End
