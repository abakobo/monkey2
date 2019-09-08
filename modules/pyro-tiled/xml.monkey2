#rem

Parses XML files and/or strings to generate an XMLDocument instance.

Based on the original version created by Steve Revill and Shane Woolcock, part of the Diddy framework.

Modified by Tony Smits and Stuart Tresadern for the Ignition X framework.

Modified by Tony Smits for the Pyro and Pyro 2 framework.

#end

' Check for ???

Namespace pyro.tiled

#rem monkeydoc @hidden
#end
Class XMLParser

	Method ParseFile:XMLDocument( filename:String )

		Local xmlString:=LoadString( filename )

		Try
			If filename="" Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'ParseFile' : File not specified!" )
			If xmlString="" Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'ParseFile' : Error loading file '"+filename+"'!" )
		Catch err:_XMLException
			Print err.Message
		End Try

		Return ParseString( xmlString )

	End
	
	Method ParseString:XMLDocument( str:String )

		Self.str=str
		
		Local doc:=New XMLDocument()
		Local elements:=New Stack<XMLElement>
		Local thisE:XMLElement=Null,newE:XMLElement=Null
		Local index:=0,a:=0,b:=0,c:=0,nextIndex:=0
		Local trimmed:=New Int[2]
		
		' cache all the control characters
		CacheControlCharacters()
		
		' find first opening tag
		Try

			If tagCount=0 New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'ParseString' : Something seriously wrong... no tags!" )
	
			' parse processing instructions
			index=0
			a=pis[index]+2
			b=pis[index+1]-1
			While index < piCount
				TrimString( a,b,trimmed )
				If trimmed[0] <> trimmed[1]
					newE=GetTagContents( trimmed[0],trimmed[1] )
					newE.pi=True
					doc.pi.Push( newE )
					newE=Null
				Else
	
					Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'ParseString' : Empty processing instruction!" )
				
				End
				index += 2
			End
			
			' loop on tags
			index=0
			While index+1 < tagCount
				' we skip comments
				If tagType[index]=TAG_COMMENT
					' skip comments
				
				' if it's cdata
				Else If tagType[index]=TAG_CDATA
					' get the text between < and >
					a=tags[index]+9 ' "![CDATA[".Length
					b=tags[index+1]-2 ' "]]".Length
					
					' add a cdata element
					newE=New XMLElement
					newE.cdata=True
''					newE.value=str.Slice (a,b )
					newE.parent=thisE
					thisE.AddChild( newE )
					newE=Null
					
				' otherwise we do normal tag stuff
				Else
					' get the text between < and >
					a=tags[index]+1
					b=tags[index+1]
					
					' trim the string
					TrimString( a,b,trimmed )
					
					' if it's a completely empty tag name,die
	
					If trimmed[0]=trimmed[1] Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'ParseString' : Empty tag!" )
					
					' check if the first character is a slash ( end tag )
					If str[trimmed[0]]=ASC_SLASH
						' if no current element,die
	
						If thisE=Null Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'ParseString' : Closing tag found outside main document tag!" )
					
						' strip the slash
						trimmed[0] += 1
						
						' check that the tag name length matches
	
						If trimmed[1] - trimmed[0] <> thisE.name.Length
''							Throw New Exception( "Exception in module 'xml.monkey',class 'XMLParser',method 'ParseString' : "+"Closing tag ~q" + str[trimmed[0] .. trimmed[1]] + "~q does not match opening tag ~q" + thisE.name + "~q" ) '???
						Endif
		
						' check that the tag name matches ( manually so that we don't create an entire string slice when the first character could be wrong! )
						For Local nameIdx:Int=0 Until thisE.name.Length
	
							If str[trimmed[0] + nameIdx] <> thisE.name[nameIdx]
''								Throw New Exception( "Exception in module 'xml.monkey',class 'XMLParser',method 'ParseString' : "+"Closing tag ~q" + str[trimmed[0] .. trimmed[1]] + "~q does not match opening tag ~q" + thisE.name + "~q" )'???
							Endif
						
						Next
						
						' pop the element from the stack,or set the document root
						If elements.Length>0
							thisE=elements.Pop()
						Else
							'doc.root=thisE
							Exit
						End
						
					' check if the last character is a slash ( self closing tag )
					Else If str[trimmed[1]-1]=ASC_SLASH
						' strip the slash
						trimmed[1] -= 1
						
						' create an element from the tag
						newE=GetTagContents( trimmed[0],trimmed[1] )
						
						' add as a child or set as the root
						If doc.root=Null doc.root=newE
						If thisE <> Null
							thisE.AddChild( newE )
						Else
							'doc.root=newE
							Exit
						End
						newE=Null
						
					' otherwise it's an opening tag
					Else
						' create an element from the tag
						newE=GetTagContents( trimmed[0],trimmed[1] )
	
						If doc.root=Null doc.root=newE
						
						' add as a child if we already have an element
						If thisE <> Null
							thisE.AddChild( newE )
						End
						
						' push this element
						elements.Push( thisE )
						
						' and set as the current
						thisE=newE
						newE=Null
					End
				End
				
				' get any text between tags
				index += 1
				If index < tagCount
					a=tags[index]+1
					b=tags[index+1]
					TrimString( a,b,trimmed )
					If trimmed[0] <> trimmed[1]
						If thisE <> Null
							thisE.value += _UnescapeXMLString( str.Slice( trimmed[0],trimmed[1] ) )
						Else
							'AssertError( "Loose text outside of any tag!" ) - Getting this for some reason,I'll fix it later :/
						End
					End
				End
				
				' next tag
				index += 1
			End
			
			If doc.root=Null Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'ParseString' : Error parsing XML: no document tag found!" )
	
		Catch err:_XMLException
			Print err.Message
		End Try
							
		Return doc
		
	End	 Method

	Private

	Const TAG_DEFAULT:Int=0
	Const TAG_COMMENT:Int=1
	Const TAG_CDATA:Int=2
	Const TAG_DOCTYPE:Int=3

	Field str:String
	Field tags:Int[] ' tag character indexes,alternating < and >
	Field tagType:Int[] 
	Field tagCount:Int=0
	Field tagsLength:Int
	Field quotes:Int[] ' quote character indexes,alternating opening and closing
	Field quoteCount:Int=0
	Field quotesLength:Int
	Field pis:Int[] ' pi character indexes,alternating <? And ?> ( index is on <> )
	Field piCount:Int=0
	Field pisLength:Int
	
	Method CacheControlCharacters()
		tagsLength=128
		quotesLength=128
		pisLength=128
		tags=New Int[tagsLength]
		tagType=New Int[tagsLength]
		quotes=New Int[quotesLength]
		pis=New Int[quotesLength]
		tagCount=0
		quoteCount=0
		piCount=0
		Local inTag:Bool=False
		Local inQuote:Bool=False
		Local inComment:Bool=False
		Local inCdata:Bool=False
		Local inDoctype:Bool=False
		Local inPi:Bool=False
		Local strlen:Int=str.Length
		
		Try
			For Local i:Int=0 Until strlen
				' if we're in a comment,we're only looking for -->
				If inComment
					If str[i]=ASC_GREATER_THAN And str[i-1]=ASC_HYPHEN And str[i-2]=ASC_HYPHEN
						If tagCount+1 >= tagsLength
							tagsLength *= 2
							tags=ResizeArray( tags,tagsLength )
							tagType=ResizeArray( tagType,tagsLength )
						Endif
						tags[tagCount]=i
						tagType[tagCount]=TAG_COMMENT
						tagCount += 1
						inComment=False
					End
				' if we're in a cdata,we're only looking for ]]>
				Else If inCdata
					If str[i]=ASC_GREATER_THAN And str[i-1]=ASC_CLOSE_BRACKET And str[i-2]=ASC_CLOSE_BRACKET
						If tagCount+1 >= tagsLength
							tagsLength *= 2
							tags=ResizeArray( tags,tagsLength )
							tagType=ResizeArray( tagType,tagsLength )
						End
						tags[tagCount]=i
						tagType[tagCount]=TAG_CDATA
						tagCount += 1
						inCdata=False
					End
				' if we're in a quoted string,we're only looking for "
				Else If inQuote
					If str[i]=ASC_DOUBLE_QUOTE
						If quoteCount+1 >= quotesLength
							quotesLength *= 2
							quotes=ResizeArray( quotes,quotesLength )
						Endif
						quotes[quoteCount]=i
						quoteCount += 1
						inQuote=False
					End
				' check if we should start a new quoted string
				Else If str[i]=ASC_DOUBLE_QUOTE
					If quoteCount+1 >= quotesLength
						quotesLength *= 2
						quotes=ResizeArray( quotes,quotesLength )
					End
					quotes[quoteCount]=i
					quoteCount += 1
					inQuote=True
				' if we're in a processing instruction,we're only looking for ?>
				Else If inPi
					If str[i]=ASC_GREATER_THAN And str[i-1]=ASC_QUESTION
						If piCount+1 >= pisLength
							pisLength *= 2
							pis=ResizeArray( pis,pisLength )
						End
						pis[piCount]=i
						piCount += 1
						inPi=False
					End
				' if we're in a doctype,we're only looking for >
				Else If inDoctype
					If str[i]=ASC_GREATER_THAN
						If tagCount+1 >= tagsLength
							tagsLength *= 2
							tags=ResizeArray( tags,tagsLength )
							tagType=ResizeArray( tagType,tagsLength )
						End
						tags[tagCount]=i
						tagType[tagCount]=TAG_DOCTYPE
						tagCount += 1
						inDoctype=False
					End
				' less than
				Else If str[i]=ASC_LESS_THAN
					' if we're in a tag,die
					If inTag Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'CacheControlCharacters' : Invalid less than!" )
							
					' check for prolog
					If str[i+1]=ASC_EXCLAMATION
						' comment?
						If str[i+2]=ASC_HYPHEN And str[i+3]=ASC_HYPHEN
							If tagCount+1 >= tagsLength
								tagsLength *= 2
								tags=ResizeArray( tags,tagsLength )
								tagType=ResizeArray( tagType,tagsLength )
							End
							tags[tagCount]=i
							tagType[tagCount]=TAG_COMMENT
							tagCount += 1
							inComment=True
						' cdata?
						Else If str[i+2]=ASC_OPEN_BRACKET And ( str[i+3]=ASC_UPPER_C Or str[i+3]=ASC_LOWER_C ) And ( str[i+4]=ASC_UPPER_D Or str[i+4]=ASC_LOWER_D ) And ( str[i+5]=ASC_UPPER_A Or str[i+5]=ASC_LOWER_A ) And ( str[i+6]=ASC_UPPER_T Or str[i+6]=ASC_LOWER_T ) And ( str[i+7]=ASC_UPPER_A Or str[i+7]=ASC_LOWER_A ) And str[i+8]=ASC_OPEN_BRACKET
							If tagCount+1 >= tagsLength
								tagsLength *= 2
								tags=ResizeArray( tags,tagsLength )
								tagType=ResizeArray( tagType,tagsLength )
							End
							tags[tagCount]=i
							tagType[tagCount]=TAG_CDATA
							tagCount += 1
							inCdata=True
						' doctype?
						Else If ( str[i+2]=ASC_UPPER_D Or str[i+2]=ASC_LOWER_D ) And ( str[i+3]=ASC_UPPER_O Or str[i+3]=ASC_LOWER_O ) And ( str[i+4]=ASC_UPPER_C Or str[i+4]=ASC_LOWER_C ) And ( str[i+5]=ASC_UPPER_T Or str[i+5]=ASC_LOWER_T ) And ( str[i+6]=ASC_UPPER_Y Or str[i+6]=ASC_LOWER_Y ) And ( str[i+7]=ASC_UPPER_P Or str[i+7]=ASC_LOWER_P ) And ( str[i+8]=ASC_UPPER_E Or str[i+8]=ASC_LOWER_E )
							If tagCount+1 >= tagsLength
								tagsLength *= 2
								tags=ResizeArray( tags,tagsLength )
								tagType=ResizeArray( tagType,tagsLength )
							End
							tags[tagCount]=i
							tagType[tagCount]=TAG_DOCTYPE
							tagCount += 1
							inDoctype=True
						Else
	
							Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'CacheControlCharacters' : Invalid prolog!" )
											
						End
					' check for processing instruction
					Else If str[i+1]=ASC_QUESTION
						If piCount+1 >= pisLength
							pisLength *= 2
							pis=ResizeArray( pis,pisLength )
						End
						pis[piCount]=i
						piCount += 1
						inPi=True
					' finally,it must just be opening a tag
					Else
						If tagCount+1 >= tagsLength
							tagsLength *= 2
							tags=ResizeArray( tags,tagsLength )
							tagType=ResizeArray( tagType,tagsLength )
						End
						tags[tagCount]=i
						tagType[tagCount]=TAG_DEFAULT
						tagCount += 1
						inTag=True
					End
				' greater than
				Else If str[i]=ASC_GREATER_THAN
	
					If Not inTag Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'CacheControlCharacters' : Invalid greater than!" )
	
					If tagCount + 1=tagsLength
						tagsLength *= 2
						tags=ResizeArray( tags,tagsLength )
						tagType=ResizeArray( tagType,tagsLength )
					End
					tags[tagCount]=i
					tagType[tagCount]=TAG_DEFAULT
					tagCount += 1
					inTag=False
				End
			Next
	
			If inQuote Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'CacheControlCharacters' : Unclosed quote!" )
			If inTag Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'CacheControlCharacters' : Unclosed tag!" )
			If inComment Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'CacheControlCharacters' : Unclosed comment!" )
			If inCdata Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'CacheControlCharacters' : Unclosed cdata!" )
			If inPi Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'CacheControlCharacters' : Unclosed processing instruction!" )
				
		Catch err:_XMLException
		
			Print err.Message

		End Try				

	End

	Method GetTagContents:XMLElement( startIndex:Int,endIndex:Int )
							
		' our element
		Local e:XMLElement=New XMLElement
		Local a:Int,singleQuoted:Bool,doubleQuoted:Bool,key:String,value:String
		
		' die if empty tag
		Try
			If startIndex=endIndex Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'GetTagContents' : Empty tag detected!" )
			' get the name
			a=startIndex
			While a < endIndex
				If str[a]=ASC_SPACE Or str[a]=ASC_TAB Or str[a]=ASC_LF Or str[a]=ASC_CR
					e.name=str.Slice( startIndex,a)
					a += 1
					Exit
				Else If a=endIndex-1
					e.name=str.Slice( startIndex,endIndex )
				End
				a += 1
			End
			startIndex=a
			
			' TODO: validate tag name is alphanumeric
			' if no name,die
			If e.name="" Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'GetTagContents' : Error reading tag name!" )

			' loop on all tokens
			While startIndex < endIndex
				' trim leading whitespace
				While startIndex < endIndex And ( str[startIndex]=ASC_SPACE Or str[startIndex]=ASC_TAB Or str[startIndex]=ASC_LF Or str[startIndex]=ASC_CR )
					startIndex += 1
				End
				
				' clear check variables
				singleQuoted=False
				doubleQuoted=False
				key=""
				value=""
				
				' find the key
				a=startIndex
				While a < endIndex
					If str[a]=ASC_EQUALS Or str[a]=ASC_SPACE Or str[a]=ASC_TAB Or str[a]=ASC_LF Or str[a]=ASC_CR Or a=endIndex-1
						If a=endIndex-1
							key=str.Slice( startIndex,endIndex )
						Else
							key=str.Slice( startIndex,a )
						End
						a += 1
						Exit
					End
					a += 1
				End
				startIndex=a
				
				' if the key is empty,there was an error ( unless we've hit the end of the string )
				If key=""
					If a < endIndex
	
						Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'GetTagContents' : Error reading attribute key!" )
	
					Else
						Exit
					End
				End
				
				' if it stopped on an equals,get the value
				If str[a-1]=ASC_EQUALS
					singleQuoted=False
					doubleQuoted=False
					While a < endIndex
						' check if it's a single quote
						If str[a]=ASC_SINGLE_QUOTE And Not doubleQuoted
							' if this is the first index,mark it as quoted
							If a=startIndex
								singleQuoted=True
							' otherwise,if we're not quoted at all,die
							Else If Not singleQuoted And Not doubleQuoted
	
								Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'GetTagContents' : Unexpected single quote detected in attribute value!" )
	
							Else
								' we must be ending the quote here,so grab it and break out
								singleQuoted=False
								value=str.Slice( startIndex+1,a )
								a += 1
								Exit
							End
							
						' check if it's a double quote
						Else If str[a]=ASC_DOUBLE_QUOTE And Not singleQuoted
							' if this is the first index,mark it as quoted
							If a=startIndex
								doubleQuoted=True
							' otherwise,if we're not quoted at all,die
							Else If Not singleQuoted And Not doubleQuoted
	
								Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'GetTagContents' : Unexpected double quote detected in attribute value!" )
							
							Else
								' we must be ending the quote here,so break out
								doubleQuoted=False
								value=str.Slice( startIndex+1,a )
								a += 1
								Exit
							End
							
						' should we be ending the attribute?
						Else If a=endIndex-1 Or ( Not singleQuoted And Not doubleQuoted And ( str[a]=ASC_SPACE Or str[a]=ASC_TAB Or str[a]=ASC_LF Or str[a]=ASC_CR ) )
							If a=endIndex-1
								value=str.Slice( startIndex,endIndex )
							Else
								value=str.Slice( startIndex,a )
							End
							a += 1
							Exit
						End
						a += 1
					End
					startIndex=a
					value=_UnescapeXMLString( value )

					If singleQuoted Or doubleQuoted Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLParser',method 'GetTagContents' : Unclosed quote detected!" )
			
			End
			
			' set the attribute
			e.SetAttribute( key,value )

			If a >= endIndex Exit
				
		End
		
		Catch err:_XMLException
		
			Print err.Message

		End Try
		
		Return e
		
	End

	Method TrimString( startIdx:Int,endIdx:Int,trimmed:Int[] )
		Local trimStart:Int=startIdx,trimEnd:Int=endIdx
		While trimEnd > trimStart
			Local ch:Int=str[trimEnd-1]
			If ch=ASC_CR Or ch=ASC_LF Or ch=ASC_SPACE Or ch=ASC_TAB
				trimEnd -= 1
			Else
				Exit
			End
		End
		While trimStart < trimEnd
			Local ch:Int=str[trimStart]
			If ch=ASC_CR Or ch=ASC_LF Or ch=ASC_SPACE Or ch=ASC_TAB
				trimStart += 1
			Else
				Exit
			End
		End
		trimmed[0]=trimStart
		trimmed[1]=trimEnd
	End

End

#rem monkeydoc @hidden
#end
Class XMLDocument

	Method New( rootName:String="" )
		If rootName <> "" root=New XMLElement( rootName )
	End

	Method New( root:XMLElement )
		Self.root=root
	End
	
	Method ExportString:String( formatXML:Bool=True )
		' we start with the xml instruction
		Local output:String="<?xml version=~q"+xmlVersion+"~q encoding=~q"+xmlEncoding+"~q?>"
		If formatXML output += "~n"
		' root node
		output += root.ToString( formatXML ) + "~n"
		' done!
		Return output
	End

	Method SaveFile( filename:String )
#rem
		#If LANG<>"cpp" And LANG<>"java" And LANG<>"cs"

			Print( "WARNING : File streams are not supported on this target" )

			Return
		
		#Else

			If filename="" Return
	
			Local xmlString:String=ExportString()
	
			Local fs:FileStream=FileStream.Open( filename,"w" )
			
			fs.WriteString( xmlString,"utf8" )
			fs.Close()

		#Endif
#end	
	End

	Property Root:XMLElement()
		Return root
	End
	
	Property Prologs:Stack<XMLElement>()
		Return prologs
	End
	
	Property ProcessingInstructions:Stack<XMLElement>()
		Return pi
	End
	
	Property Version:String()
		Return xmlVersion
	End
	
	Property Encoding:String()
		Return xmlEncoding
	End

	Private

	Field xmlVersion:="1.0"
	Field xmlEncoding:="UTF-8"
	Field root:XMLElement
	Field pi:=New Stack<XMLElement>
	Field prologs:=New Stack<XMLElement>

End

#rem monkeydoc @hidden
#end
Class XMLAttribute

	Method New( name:String,value:String )
		Self.name=name
		Self.value=value
	End
	
	Method Matches:Bool( check:String )
		Return check=name + "=" + value
	End

	Private

	Field name:String
	Field value:String

End

#rem monkeydoc @hidden
#end
Class XMLElement

	Method New()
	End
	
	Method New( name:String,parent:XMLElement=Null )
		Self.parent=parent
		Self.name=name
		If parent <> Null parent.children.Push( Self )
	End

	Method IsProcessingInstruction:Bool()
		Return pi
	End
	
	Method IsProlog:Bool()
		Return prolog
	End
	
	Method IsCharacterData:Bool()
		Return cdata
	End
	
	Method AddChild( child:XMLElement )
		If children.Contains( child ) Return
		children.Push( child )
		child.parent=Self
	End
	
	Method HasAttribute:Bool( name:String )
		If Not name Return False ' checking for an empty name,will always return false
		For Local i:=0 Until attributes.Length
			Local att:XMLAttribute=attributes.Get( i )
			If att.name=name Return True
		Next
		Return False
	End
	
	Method GetAttribute:String( name:String,defaultValue:String="" )
		If Not name Return "" ' reading an empty name will always return ""
		For Local i:=0 Until attributes.Length
			Local att:XMLAttribute=attributes.Get( i )
			If att.name=name Return att.value
		Next
		Return defaultValue
	End
	
	Method SetAttribute:String( name:String,value:String )
		' we'll prevent the developer from setting an attribute with an empty name,as it makes no sense
		Try
			If Not name Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLElement',method 'SetAttribute' : No name specified!" )
		Catch err:_XMLException
			Print err.Message
		End Try
							
		For Local i:Int=0 Until attributes.Length
			Local att:XMLAttribute=attributes.Get( i )
			If att.name=name
				Local old:String=att.value
				att.value=value
				Return old
			End
		Next
		attributes.Push( New XMLAttribute( name,value ) )
		Return ""
	End
	
	Method ClearAttribute:String( name:String )
		If Not name Return "" ' clearing an attribute with an empty name just returns ""
		For Local i:=0 Until attributes.Length
			Local att:XMLAttribute=attributes.Get( i )
			If att.name=name
				attributes.RemoveEach( att )
				Return att.value
			End
		Next
		Return ""
	End
	
	Method Dispose( removeSelf:Bool=True )
		' dispose children
		#rem FIXME
		Local en:IEnumerator<XMLElement>=children.Enumerator()
		While en.HasNext()
			Local element:XMLElement=en.NextObject()
			element.Dispose( False )
			en.Remove()
		End
		#end
		' remove self from parent if this is not recursing
		If removeSelf And parent <> Null parent.children.RemoveEach( Self )
		' clear out the parent
		parent=Null
	End
	
	Method ToString:String( formatXML:Bool=True,indentation:Int=0 )
		Local rv:String=""
		If formatXML
			For Local i:=0 Until indentation
				rv += "  "
			Next
		End
		rv += openTagStart + name
		If attributes.Length>0
			For Local i:=0 Until attributes.Length
				Local att:XMLAttribute=attributes.Get( i )
				rv += " " + att.name + "=~q" + _EscapeXMLString( att.value ) + "~q"
			Next
		End
		If children.Length=0
			Local esc:String=_EscapeXMLString( value.Trim() )
			If esc.Length=0
				rv += selfCloseEnd
			Else
				rv += openTagEnd + value.Trim() + closeTagStart + name + closeTagEnd
			End
		Else
			rv += openTagEnd
			If formatXML
				rv += "~n"
			End
			For Local i:=0 Until children.Length
				rv += children.Get( i ).ToString( formatXML,indentation + 1 )
			End
			Local esc:String=_EscapeXMLString( value.Trim() )
			If esc.Length > 0
				If Not formatXML
					rv += esc
				Else
					rv += "~n" + esc + "~n"
				End
			End
			If formatXML
				For Local i:=0 Until indentation
					rv += "  "
				Next
			End
			rv += closeTagStart + name + closeTagEnd
		End
		If formatXML
			rv += "~n"
		End
		Return rv
	End
	
	Method GetMembersByName:Stack<XMLElement>( findName:String,att1:String="",att2:String="",att3:String="",att4:String="",att5:String="",att6:String="",att7:String="",att8:String="",att9:String="",att10:String="" )
	
		Try
			If Not findName Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLElement',method 'GetMembersByName' : Name not specified!" )
		Catch err:_XMLException
			Print err.Message
		End Try
						
		Local rv:=New Stack<XMLElement>
		For Local element:XMLElement=Eachin children
			If element.name=findName
				If att1 And Not element.MatchesAttribute( att1 ) Continue
				If att2 And Not element.MatchesAttribute( att2 ) Continue
				If att3 And Not element.MatchesAttribute( att3 ) Continue
				If att4 And Not element.MatchesAttribute( att4 ) Continue
				If att5 And Not element.MatchesAttribute( att5 ) Continue
				If att6 And Not element.MatchesAttribute( att6 ) Continue
				If att7 And Not element.MatchesAttribute( att7 ) Continue
				If att8 And Not element.MatchesAttribute( att8 ) Continue
				If att9 And Not element.MatchesAttribute( att9 ) Continue
				If att10 And Not element.MatchesAttribute( att10 ) Continue
				rv.Push( element )
			End
		Next
		Return rv
	End
	
	Method GetFirstChildByName:XMLElement( findName:String,att1:String="",att2:String="",att3:String="",att4:String="",att5:String="",att6:String="",att7:String="",att8:String="",att9:String="",att10:String="" )
		
		Try
			If Not findName Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLElement',method 'GetFirstMembersByName' : Name not specified!" )
		Catch err:_XMLException
			Print err.Message
		End Try
					
		For Local element:XMLElement=Eachin children
			If element.name=findName
				If att1 And Not element.MatchesAttribute( att1 ) Continue
				If att2 And Not element.MatchesAttribute( att2 ) Continue
				If att3 And Not element.MatchesAttribute( att3 ) Continue
				If att4 And Not element.MatchesAttribute( att4 ) Continue
				If att5 And Not element.MatchesAttribute( att5 ) Continue
				If att6 And Not element.MatchesAttribute( att6 ) Continue
				If att7 And Not element.MatchesAttribute( att7 ) Continue
				If att8 And Not element.MatchesAttribute( att8 ) Continue
				If att9 And Not element.MatchesAttribute( att9 ) Continue
				If att10 And Not element.MatchesAttribute( att10 ) Continue
				Return element
			End
		Next
		Return Null
	End
	
	Method MatchesAttribute:Bool( check:String )
		For Local attr:XMLAttribute=Eachin attributes
			If attr.Matches( check ) Return True
		Next
		Return False
	End
	
	Property Members:Stack<XMLElement>()
		Return children
	End
	
	Property Parent:XMLElement()
		Return parent
	End
	
	Property Name:String()
		Return name
	Setter( name:String )
		Try
			If Not name Throw New _XMLException( "Exception in module 'xml.monkey',class 'XMLElement',method 'Name' : Name not specified!" )
		Catch err:_XMLException
			Print err.Message
		End Try
							
		Self.name=name
	End
	
	Property Value:String()
		Return value
	Setter ( value:String )
		Self.value=value
	End

	Private

	Field openTagStart:String="<"
	Field openTagEnd:String=">"
	Field selfCloseEnd:String=" />"
	Field closeTagStart:String="</"
	Field closeTagEnd:String=">"
	
	Field name:String
	Field attributes:=New Stack<XMLAttribute>
	Field children:=New Stack<XMLElement>
	Field value:String
	Field parent:XMLElement

	Field pi:Bool
	Field prolog:Bool
	Field cdata:Bool

End

Private

Class _XMLException Extends Throwable
	
    Method New( message:String )
		_message=message
    End

''    Method New( functionMethodName:String,errorMessage:String )
''		_message="Exception in "+functionMethodName+" : "+errorMessage
 ''   End

	Property Message:String()
		Return _message
	End

	Private

	Field _message:String
    
End

Function _EscapeXMLString:String( str:String )
	If Not str Return ""
	Local xmlstring:String
	For Local i:Int=0 Until str.Length
		Select str[i]
			Case ASC_AMPERSAND
				xmlstring += "&amp;"
			Case ASC_LESS_THAN
				xmlstring += "&lt;"
			Case ASC_GREATER_THAN
				xmlstring += "&gt;"
			Case ASC_SINGLE_QUOTE
				xmlstring += "&apos;"
			Case ASC_DOUBLE_QUOTE
				xmlstring += "&quot;"
			Default
				xmlstring += String.FromChar( str[i] )
		End
	End
	Return xmlstring
End

Function _UnescapeXMLString:String( str:String )
	If Not str Return ""
	str=str.Replace( "&quot;","~q" )
	str=str.Replace( "&apos;","'" )
	str=str.Replace( "&gt;",">" )
	str=str.Replace( "&lt;","<" )
	str=str.Replace( "&amp;","&" )
	Return str
End

' Control characters:
Const ASC_NUL:Int=0                          ' Null character
Const ASC_SOH:Int=1                          ' Start of Heading
Const ASC_STX:Int=2                          ' Start of Text
Const ASC_ETX:Int=3                          ' End of Text
Const ASC_EOT:Int=4                          ' End of Transmission
Const ASC_ENQ:Int=5                          ' Enquiry
Const ASC_ACK:Int=6                          ' Acknowledgment
Const ASC_BEL:Int=7                          ' Bell
Const ASC_BACKSPACE:Int=8                    ' Backspace
Const ASC_TAB:Int=9                          ' Horizontal tab
Const ASC_LF:Int=10                          ' Linefeed
Const ASC_VTAB:Int=11                        ' Vertical tab
Const ASC_FF:Int=12                          ' Form feed
Const ASC_CR:Int=13                          ' Carriage return
Const ASC_SO:Int=14                          ' Shift Out
Const ASC_SI:Int=15                          ' Shift In
Const ASC_DLE:Int=16                         ' Data Line Escape
Const ASC_DC1:Int=17                         ' Device Control 1
Const ASC_DC2:Int=18                         ' Device Control 2
Const ASC_DC3:Int=19                         ' Device Control 3
Const ASC_DC4:Int=20                         ' Device Control 4
Const ASC_NAK:Int=21                         ' Negative Acknowledgment
Const ASC_SYN:Int=22                         ' Synchronous Idle
Const ASC_ETB:Int=23                         ' End of Transmit Block
Const ASC_CAN:Int=24                         ' Cancel
Const ASC_EM:Int=25                          ' End of Medium
Const ASC_SUB:Int=26                         ' Substitute
Const ASC_ESCAPE:Int=27                      ' Escape
Const ASC_FS:Int=28                          ' File separator
Const ASC_GS:Int=29                          ' Group separator
Const ASC_RS:Int=30                          ' Record separator
Const ASC_US:Int=31                          ' Unit separator

' Visible characters:
Const ASC_SPACE:Int=32                       ' '
Const ASC_EXCLAMATION:Int=33                 '!'
Const ASC_DOUBLE_QUOTE:Int=34                '"'
Const ASC_HASH:Int=35                        '#'
Const ASC_DOLLAR:Int=36                      '$'
Const ASC_PERCENT:Int=37                     '%'
Const ASC_AMPERSAND:Int=38                   '&'
Const ASC_SINGLE_QUOTE:Int=39                '''
Const ASC_OPEN_PARENTHESIS:Int=40            '( '
Const ASC_CLOSE_PARENTHESIS:Int=41           ' )'
Const ASC_ASTERISK:Int=42                    '*'
Const ASC_PLUS:Int=43                        '+'
Const ASC_COMMA:Int=44                       ','
Const ASC_HYPHEN:Int=45                      '-'
Const ASC_PERIOD:Int=46                      '.'
Const ASC_SLASH:Int=47                       '/'
Const ASC_0:Int=48
Const ASC_1:Int=49
Const ASC_2:Int=50
Const ASC_3:Int=51
Const ASC_4:Int=52
Const ASC_5:Int=53
Const ASC_6:Int=54
Const ASC_7:Int=55
Const ASC_8:Int=56
Const ASC_9:Int=57
Const ASC_COLON:Int=58                      ':'
Const ASC_SEMICOLON:Int=59                  ';'
Const ASC_LESS_THAN:Int=60                  '<'
Const ASC_EQUALS:Int=61                     '='
Const ASC_GREATER_THAN:Int=62               '>'
Const ASC_QUESTION:Int=63                   '?'
Const ASC_AT:Int=64                         '@'
Const ASC_UPPER_A:Int=65
Const ASC_UPPER_B:Int=66
Const ASC_UPPER_C:Int=67
Const ASC_UPPER_D:Int=68
Const ASC_UPPER_E:Int=69
Const ASC_UPPER_F:Int=70
Const ASC_UPPER_G:Int=71
Const ASC_UPPER_H:Int=72
Const ASC_UPPER_I:Int=73
Const ASC_UPPER_J:Int=74
Const ASC_UPPER_K:Int=75
Const ASC_UPPER_L:Int=76
Const ASC_UPPER_M:Int=77
Const ASC_UPPER_N:Int=78
Const ASC_UPPER_O:Int=79
Const ASC_UPPER_P:Int=80
Const ASC_UPPER_Q:Int=81
Const ASC_UPPER_R:Int=82
Const ASC_UPPER_S:Int=83
Const ASC_UPPER_T:Int=84
Const ASC_UPPER_U:Int=85
Const ASC_UPPER_V:Int=86
Const ASC_UPPER_W:Int=87
Const ASC_UPPER_X:Int=88
Const ASC_UPPER_Y:Int=89
Const ASC_UPPER_Z:Int=90
Const ASC_OPEN_BRACKET:Int=91               '['
Const ASC_BACKSLASH:Int=92                  '\'
Const ASC_CLOSE_BRACKET:Int=93              ']'
Const ASC_CIRCUMFLEX:Int=94                 '^'
Const ASC_UNDERSCORE:Int=95                 '_'
Const ASC_BACKTICK:Int=96                   '`'
Const ASC_LOWER_A:Int=97
Const ASC_LOWER_B:Int=98
Const ASC_LOWER_C:Int=99
Const ASC_LOWER_D:Int=100
Const ASC_LOWER_E:Int=101
Const ASC_LOWER_F:Int=102
Const ASC_LOWER_G:Int=103
Const ASC_LOWER_H:Int=104
Const ASC_LOWER_I:Int=105
Const ASC_LOWER_J:Int=106
Const ASC_LOWER_K:Int=107
Const ASC_LOWER_L:Int=108
Const ASC_LOWER_M:Int=109
Const ASC_LOWER_N:Int=110
Const ASC_LOWER_O:Int=111
Const ASC_LOWER_P:Int=112
Const ASC_LOWER_Q:Int=113
Const ASC_LOWER_R:Int=114
Const ASC_LOWER_S:Int=115
Const ASC_LOWER_T:Int=116
Const ASC_LOWER_U:Int=117
Const ASC_LOWER_V:Int=118
Const ASC_LOWER_W:Int=119
Const ASC_LOWER_X:Int=120
Const ASC_LOWER_Y:Int=121
Const ASC_LOWER_Z:Int=122
Const ASC_OPEN_BRACE:Int=123                '{'
Const ASC_PIPE:Int=124                      '|'
Const ASC_CLOSE_BRACE:Int=125               '}'
Const ASC_TILDE:Int=126                     '~'
Const ASC_DELETE:Int=127