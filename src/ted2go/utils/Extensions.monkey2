
Namespace ted2go


Class Stack<T> Extension
	
	Property First:T()
		Return Self.Empty ? Null Else Self[0]
	End
	
	Property Last:T()
		Return Self.Empty ? Null Else Self[Self.Length-1]
	End
	
End
