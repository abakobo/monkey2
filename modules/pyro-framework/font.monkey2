Namespace pyro.framework.font

#rem monkeydoc @hidden
#end
Function FitText:String[]( font:Font,text:String,width:Int,separator:String="~n" )

	text=text.Replace( separator," "+separator+" " )

	Local line:=""
	Local lines:String[]
	Local word:=text.Split( " " )
	
	For Local i:=0 Until word.Length

		If word[i].Contains( separator )
			lines=lines.Resize( lines.Length+1 )
			lines[ lines.Length-1 ]=line.Trim()
			line=""
		Else
			If font.TextWidth( line+word[i] )<width
				line+=word[i]+" "
			Else
				lines=lines.Resize( lines.Length+1 )
				lines[ lines.Length-1 ]=line.Trim()
				line=word[i]+" "
			Endif
		Endif

	Next

	If line
		lines=lines.Resize( lines.Length+1 )
		lines[ lines.Length-1 ]=line.Trim()
	Endif
	
	Return lines

End
