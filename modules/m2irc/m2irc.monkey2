' Monkey 2 IRC module
' By @Hezkore 2018
' https://github.com/Hezkore/m2irc

Namespace m2irc

#Import "<std>"
Using std..

'=MODULE=

Class IRCClient
	Field _address:String
	Field _port:Int=6667
	
	Field _receiveBuffer:DataBuffer=New DataBuffer(512)
	
	Field _useFiber:Bool=True
	Field _update:Fiber 'Fiber for updating internet messages
	Field _sleep:Float=0.35 'Lower value gets messages faster but uses more CPU
	
	Field _socket:Socket
	Field _stream:SocketStream
	
	Field _nickname:String
	Field _realname:String
	Field _version:String
	
	Field _floodLimit:Byte=5
	Field _floodCount:Byte
	Field _floodTimer:Int
	
	Field _ctcpResp:=New Map<String,String>
	
	Field _sendQueue:=New Stack<String>
	
	Field _dataReceiver:=New Receiver
	
	Field _localEvents:Bool
	
	Field OnEvent:Void( event:IRCEvent )
	
	Property LocalEvents:Bool()
		Return _localEvents
	Setter( state:Bool )
		_localEvents=state
	End
	
	Property Nickname:String()
		Return _nickname
	Setter( nickname:String )
		_nickname=nickname
	End
	
	Property Version:String()
		Return _version
	Setter( version:String )
		_version=version
	End
	
	Property Port:Int()
		Return _port
	Setter( port:Int )
		_port=port
	End
	
	Property Address:String()
		Return _address
	Setter( address:String )
		_address=address
	End
	
	Property UseFiber:Bool()
		Return _useFiber
	Setter( use:Bool )
		_useFiber=use
	End
	
	Property Connected:Bool()
		If _stream And _socket And Not _socket.Closed Return True
		Return False
	End
	
	Property CTCPReply:Map<String,String>()
		Return _ctcpResp
	End
	
	Function OnIRCEvent( event:IRCEvent )
		Print "IRC::"+event
	End
	
	Method New()
		_realname="KoreIRC 1.5 ("
		
		#If __HOSTOS__="windows"
			_realname+="windows"
		#Else If __HOSTOS__="macos"
			_realname+="macos"
		#Else If __HOSTOS__="linux"
			_realname+="linux"
		#Else If __HOSTOS__="raspbian"
			_realname+="raspbian"
		#Endif
		
		_realname+=")"
		_version=_realname
		
		CTCPReply.Add( "VERSION", "$(version)" )
		CTCPReply.Add( "PING", "" )
		CTCPReply.Add( "TIME", "$(date)" )
		
		OnEvent=OnIRCEvent
	End
	
	Method Disconnect()
		If _socket Then _socket.Close()
		If _stream Then _stream.Close()
	End
	
	'Connect using new nickname, server adress and port
	Method Connect( adress:String, port:Int=6667, nick:String)
		If Connected Then Return
		
		Address=adress
		Port=port
		Nickname=nick
		Connect()
	End
	
	'Connect using existing nickname, server address and port
	Method Connect()
		If Connected Then Return
		
		If Not _nickname Or Not _realname Or Not _address Or Not _port Then
			Print "Not all info set"
			Return
		Endif
		
		'TriggerOnMessage("Connecting to server "+Self.serverAddress+":"+Self.serverPort,Self.name,Self.name,null,Self)
		'Print "Connecting to "+Self._address+":"+Self._port
		
		_socket=Socket.Connect( Self._address, Self._port )
		If Not _socket
			Print "No connect"
			'TriggerOnMessage("Couldn't connect to server",Self.name,Self.name,null,Self)
			Return
		Endif
		
		'No delay pretty please
		_socket.SetOption( "TCP_NODELAY", True )
		
		'Prepare stream
		_stream=New SocketStream( _socket )
		If Not _stream
			'TriggerOnMessage("Couldn't create socket stream",Self.name,Self.name,null,Self)
			Print "Couldn't create socket stream"
			Return
		Endif
		
		'Start our update loop in the background
		Start()
	End
	
	Method UpdateSendQueue()
		Local ms:Int=Millisecs()
		
		If ms<_floodTimer Then _floodTimer=ms
		If ms>=_floodTimer+1000
			If _floodCount>0 Then _floodCount-=(ms/_floodTimer)
			If _floodCount<0 Then _floodCount=0
			_floodTimer=ms
		Endif
		
		If _sendQueue.Length<=0 Then Return
		
		Local sendBuffer:DataBuffer
		
		While _floodCount<_floodLimit And _sendQueue.Length>0
			sendBuffer=New DataBuffer( _sendQueue[0].Length )
			sendBuffer.PokeString( 0, _sendQueue[0] )
			
			_socket.Send( sendBuffer.Data, sendBuffer.Length )
			
			'Loop back local event
			If _localEvents Then
				OnEvent( New IRCEvent( _nickname+" "+_sendQueue[0].Slice( 0, -1 ), True ) )
			Endif
			
			_sendQueue.Erase( 0 )
			
			_floodCount+=1
		Wend
	End
	
	'Send a raw string to the server
	Method SendRaw( raw:String )
		
		raw=Translate(raw)
		
		If Not raw Then Return
		
		If Not raw.EndsWith( "~n" ) Then raw+="~n"
		_sendQueue.Push( raw )
	End
	
	'Send string to server
	Method Send( command:String, p1:String="", p2:String="", p3:String="", p4:String="", p5:String="" )
		Local str:String=command.ToUpper()
		
		'Yes this is ugly, just send me a bomb already!
		p1=Translate(p1)
		p2=Translate(p2)
		p3=Translate(p3)
		p4=Translate(p4)
		p5=Translate(p5)
		
		If p1 str+=" "+p1
		If p2 str+=" "+p2
		If p3 str+=" "+p3
		If p4 str+=" "+p4
		If p5 str+=" "+p5
		
		If Not str Then Return
		
		If Not str.EndsWith( "~n" ) Then str+="~n"
		_sendQueue.Push( str )
	End
	
	'Send CTCP question or reply to user
	Method SendCTCP( toUser:String, ctcp:String, resp:String="" )
		If Not toUser Or Not ctcp Then Return
		
		If resp Then
			ctcp=String.FromChar(1)+ctcp.ToUpper()
			resp=resp+String.FromChar(1)
			Send( "NOTICE", toUser, ":"+ctcp, resp )
		Else
			ctcp=String.FromChar(1)+ctcp.ToUpper()+String.FromChar(1)
			Send( "PRIVMSG", toUser, ":"+ctcp )
		Endif
		
	End
	
	'Send to specific channel or user
	Method SendTo( dest:String, text:String, type:String="PRIVMSG" )
		If Not dest Or Not text Or Not type Then Return
		
		Send( type, dest, ":"+text )
	End
	
	'Strips hostname and returns nickname
	Function GetNickname:String(str:String)
		If str.Contains("!") Then Return str.Split("!")[0]
		Return str
	End
	
	'Strips nickname and return hostname
	Function GetHostname:String(str:String)
		If str.Contains("!") Then Return str.Split("!")[1]
		Return str
	End
	
	Method Translate:String( str:String )
		
		If str.Contains( "$(date)" )
			str=str.Replace( "$(date)", Time.Now().ToString() )
		Endif
		
		str=str.Replace( "$(version)", _version )
		str=str.Replace( "$(me)", _nickname )
		str=str.Replace( "$(realme)", _realname )
		str=str.Replace( "~n", "" )
		str=str.Replace( "~r", "" )
		
		'Strip leading spaces
		While str.StartsWith( " " )
			str=str.Slice( 1 )
		Wend
		
		Return str
	End
	
	Method AutoSeparate:String( str:String, splitter:String="#", separator:String="," )
		Local split:=str.Split( splitter )
		Local result:String
		
		For Local i:Int=1 Until split.Length
			result+=splitter+split[i]+separator
		Next
		result=result.Slice( 0, -separator.Length )
		
		Return result
	End
	
	Class Receiver
		Field lineSplit:String[]
		Field recLine:String
		Field recLength:Int
		Field linePos:Int
		Field line:String
	End
	
	Method Update:Void()
		If _stream.Eof Or Not Self.Connected Then Return
		
		'Do we have anything to receive?
		While _socket.CanReceive
			'How much should we receive, no more than our buffer length
			_dataReceiver.recLength=Min( _socket.CanReceive, _receiveBuffer.Length )
			_socket.Receive( _receiveBuffer.Data, _dataReceiver.recLength )
			
			'Add to our receive string until we've got a line ready
			_dataReceiver.recLine+=_receiveBuffer.PeekString( 0, _dataReceiver.recLength ).Replace( "~r", "" )
			
			'We got a line!
			While _dataReceiver.recLine.Contains( "~n" )
				'Remove processed line from receive string
				_dataReceiver.linePos=_dataReceiver.recLine.Find( "~n" )
				_dataReceiver.line=_dataReceiver.recLine.Left( _dataReceiver.linePos )
				_dataReceiver.recLine=_dataReceiver.recLine.Slice( _dataReceiver.linePos+1 )
				
				'Remove new line
				If _dataReceiver.line.EndsWith( "~n" ) Then
					_dataReceiver.line=_dataReceiver.line.Left( _dataReceiver.line.Length-1 )
				Endif
				
				If _dataReceiver.line.Length<=0 Then Continue
				
				'Return any PING message instantly
				If _dataReceiver.line.ToUpper().StartsWith( "PING " ) Then
					Send( "PONG", _dataReceiver.line.Split( " " )[1] )
					Continue 'We're done with the ping message now
				Endif
				
				Local event:=New IRCEvent( _dataReceiver.line )
				
				'Was this an internal event?
				If Not InternalEvent( event ) Then OnEvent( event )
				
			Wend
		Wend
		
		UpdateSendQueue()
	End
	
	Private
		
		'Background update loop for internet messages
		Method Start()
			_floodTimer=Millisecs()
			
			'Send register stuff at start
			'PASSWORD HERE
			Send( "NICK", _nickname )
			Send( "USER", _nickname, "0", "*", ":"+_realname )
			
			If _useFiber Then
				_update=New Fiber(Lambda()
					
					'Never ending loop of internet data
					While Not _stream.Eof And Self.Connected
						Update()
						
						'Sleep if there's nothing more to receive
						Fiber.Sleep( _sleep )
					Wend
					
					'TriggerOnMessage("Disconnected",Self.name,Self.name,null,Self)
					Print "DISCONNECTED!"
					If _stream Then _stream.Close()
					
				End)
			Endif
		End
		
		Method InternalEvent:Bool( event:IRCEvent )
			Select event.Command
				Case "001","005"
					'A few chances to update nickname
					_nickname=event.Param[0]
					Return False 'Let this event through
				
				Case "PRIVMSG"
					'CTCP questions
					If event.CTCP Then
						'Auto answer?
						
						Local question:String
						Local rest:String
						
						If event.Msg.Contains( " " ) Then
							question=event.Msg.Split( " " )[0].ToUpper()
							rest=event.Msg.Split( " " )[1]
						Else
							question=event.Msg.ToUpper()
						Endif
						
						If _ctcpResp.Contains( question ) Then 
							If _ctcpResp.Get( question ) Then
								SendCTCP( event.Prefix, question, _ctcpResp.Get( question ) +" "+rest )
							Else
								SendCTCP( event.Prefix, question, rest )
							Endif
							
							Return True 'This was internal
						Endif
					Endif
			End
			
			Return False
		End
		
	Public
End

Class IRCEvent
	Field Raw:String
	Field Prefix:String
	Field Host:String
	Field Command:String
	Field Param:String[]
	Field Msg:String
	Field CTCP:Bool
	Field Error:Bool
	Field Locally:Bool
	
	Method New()
	End
	
	Method New( raw:String, locally:Bool=False )
		Raw=raw
		Locally=locally
		
		'Split spaces
		Local split:String[]
		split=raw.Split(" ")
		
		'Does line have a prefix?
		If raw.StartsWith( ":" ) Then
			Prefix=split[0].Slice( 1 )
		Else
			Prefix=split[0]
		Endif
		
		'Is this a critical error?
		If Prefix.ToUpper()="ERROR" Then
			Host=Prefix
			Command=Prefix
			Msg=raw.Slice( 7 )
			Error=True
			Return
		Endif
		
		'Store line data
		Command=split[1].ToUpper()
		
		'Is this an error warning?
		If Int( Command )>=400 And Int( Command )<=599 Then
			Error=True
		Endif
		
		
		'Params and msg
		Param=New String[split.Length-2]
		For Local i:Int=0 Until Param.Length 'param
			
			If split[i+2].StartsWith( ":" ) Then 'message
				For Local i2:Int=i+2 Until split.Length-1
					Msg+=split[i2]+" "
				Next
				Msg+=split[split.Length-1]
				Msg=Msg.Slice( 1 )
				
				Param=Param.Resize( i )
				
				Exit
			Endif
			
			Param[i]=split[i+2]
		Next
		
		'Cleanup Prefix and Host
		Prefix=IRCClient.GetNickname(Prefix)
		Host=IRCClient.GetHostname(Prefix)
		
		'Check for CTCP
		If Msg And Msg[0]=1 And Msg[Msg.Length-1]=1 Then
			CTCP=True
			Msg=Msg.Slice( 1, -1 )
		Else
			CTCP=False
		Endif
	End
	
	Function MakeError:IRCEvent( message:String )
		Local event:=New IRCEvent
		event.Error=True
		event.Msg=message
		Return event
	End
	
	Operator To:String()
		Local str:="Event( "
		
		str+=Raw+"~n"
		If Locally Then str+="Local: "+Locally+"~n"
		If Error Then str+="ERROR: "+Error+"~n"
		If CTCP Then str+="CTCP: "+CTCP+"~n"
		str+="Prefix: "+Prefix+"~n"
		If Host<>Prefix Then str+="Host: "+Host+"~n"
		str+="Command: "+Command+"~n"
		
		'str+="Param: "
		For Local i:Int=0 Until Param.Length
		'	str+="("+i+".)"+Param[i]+" "
			str+="Param"+i+": "+Param[i]+"~n"
		Next
		'str+="~n"
		
		If Msg Then str+="Msg: "+Msg+"~n"
		
		str+=")"
		Return str
	End
End
