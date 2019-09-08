Namespace pyro.framework.highscoremanager

#rem monkeydoc The HighScoreManager class.
#end
Class HighScoreManager

	#rem monkeydoc The number of entries in the highscore table.
	#end
	Field MaxEntries:=10
	#rem monkeydoc The ( players ) name.
	#end
	Field Name:=""
	#rem monkeydoc The ( players ) score.
	#end
	Field Score:=0

	#rem monkeydoc Returns the highest score.
	#end
	Property Highest:HighScoreManager()

		If _table.Length=0 Return New HighScoreManager

		Local h:HighScoreManager
	
		Local r:HighScoreManager=_table[0]
	
		For Local i:=0 Until _table.Length
		If _table[i].Score>r.Score r=_table[i]
	
		Next
	
		Return r
	
	End

	#rem monkeydoc Helper to determine if the score would be a new entry.
	
	True if score would be a new entry.
	#end
	Method IsNewEntry:Bool( score:Int )
		If score>=Lowest.Score Return True
		Return False
	End

	#rem monkeydoc Returns the lowest score.
	#end
	Property Lowest:HighScoreManager()

		If _table.Length=0 Return New HighScoreManager

		Local h:HighScoreManager
	
		Local r:=_table[0]
	
		For Local i:=0 Until _table.Length
			If _table[i].Score<r.Score r=_table[i]
		Next
	
		Return r
	
	End

	#rem monkeydoc Adds a new score to the highscore table.
	
	The highscore table is automatically sorted each time a score is added.
	#end
	Method Add( name:String,score:Int )

		_table=ResizeArray( _table,_table.Length+1 )
	
		Local h:=New HighScoreManager

		h.Name=name
		h.Score=score
	
		_table[ _table.Length-1 ]=h

		_Sort()
	
		If _table.Length> MaxEntries _table=ResizeArray( _table,MaxEntries )

	End

	#rem monkeydoc Clears highscore table.
	#end
	Method Clear()
		_table=ResizeArray( _table,0 )
	End

	#rem monkeydoc Returns the number of highscore entries.
	#end
	Method Count:Int()
		Return _table.Length
	End

	#rem monkeydoc Formats the highscore table with data.
	#end
	Method Format( name:String="-" )
	
		For Local i:=0 Until MaxEntries
			Add( name,0 )
		Next
	
	End

	#rem monkeydoc Returns the highscore from position.
	#end
	Method Get:HighScoreManager( position:Int )
		Return _table[position]
	End

	#rem monkeydoc Shows the highscore table.
	
	Useful for debugging.
	#end
	Method ToString:String()

		Local out:=""

		For Local i:=0 Until _table.Length
			If out out+="~n"
''				out+=( i+1 )+" - "+prScoreFormat( _table[i].Score )+" - "+_table[i].Name
			out+=( i+1 )+" - "+_table[i].Score+" - "+_table[i].Name
		Next

		Return out

	End

	Private

	Method _Sort()

		Local s:=_table.Length-1
		
		While s>0
		
			For Local i:=0 Until _table.Length-1
			
				If _table[i].Score < _table[i+1].Score
				
					Local t:=_table[i]
					
					_table[i]=_table[i+1]
					
					_table[i+1]=t
				
				Endif
			
			Next
			
			s=s-1
		
		Wend
	
	End

	Field _table:HighScoreManager[]

End
