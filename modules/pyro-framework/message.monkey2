Namespace pyro.framework.message

#rem monkeydoc The Message class.
#end
Class Message

	#rem monkeydoc Returns the source object of the message.
	#end
	Field Source:Object

	Method New ( data:String,source:Object=Null )
		_data=data
		Source=source
	End

	#rem monkeydoc Creates an message.
	#end
	Function Create:Message( data:String,source:Object=Null )

		If Contains ( data,source ) Return Null

		Local message:=New Message( data,source )

		_messages.Push( message )

		Return message

	End

	#rem monkeydoc Cleara all messages in the message list.
	#end
	Function Clear()
		_messages.Clear()
	End

	#rem monkeydoc Returns True if the message list contains the data and source object.
	#end
	Function Contains:Bool( data:String,source:Object )

		For Local i:=0 Until _messages.Length
			Local e:=_messages.Get( i )
			If e._data=data And e.Source=source Return True
		Next

		Return False

	End

	#rem monkeydoc Return the message object if the message list matches data and source.
	#end
	Function Get:Message( data:String,source:Object )

		data=data.ToLower()

		For Local i:=0 Until _messages.Length
			Local e:=_messages.Get( i )
			If e._data.ToLower().Right( data.Length )=data And e.Source=source
				_messages.RemoveEach( e )
				Return e
			Endif
		Next

		Return Null

	End

	#rem monkeydoc Return the message object if the message list matches data.
	#end
	Function Get:Message( data:String )

		data=data.ToLower()

		For Local i:=0 Until _messages.Length
			Local e:=_messages.Get( i )
			If e._data.ToLower()=data
				_messages.RemoveEach( e )
				Return e
			Endif
		Next

		Return Null

	End

	#rem monkeydoc Shows messages.
	
	Useful for debugging.
	#end
	Function Show()
		For Local i:=0 Until _messages.Length
			Local e:=_messages.Get( i )
			Print e._data.ToLower()
		Next
	End

	Private

	Global _messages:=New Stack <Message>
	
	Field _data:=""

End