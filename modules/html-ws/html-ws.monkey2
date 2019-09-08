Namespace html.ws
#If __TARGET__="emscripten"
#Import "monkeyws.hpp"
#Import "monkeyws.cpp"
Extern Private
' :Returns an address to an inactive web socket wrapper
Function socket_init:Int()="monkeyWS::init_socket"
' :Sets the url(string) of a web socket to your choice.
Function socket_set_url:Void(sock:Int, url:CString)="monkeyWS::set_url"
' :Specific for WebSockets, adds in a protocol of the user's choice.
Function socket_add_protocol:Void(sock:Int, ptc:CString )="monkeyWS::add_protocol"
' :Actually attempts to establish a connection (call at-least after defining a url )
Function socket_connect:Int(sock:Int)="monkeyWS::connect"
' :Returns a WebSocket's "ReadyState" 
Function socket_ready_state:Int(sock:Int)="monkeyWS::ready_state"
' :Sends data(string) through the websocket
Function socket_send:Void(sock:Int, data: CString)="monkeyWS::send"
' :(synchronous) determines if there is data waiting on the queue
Function socket_has_data:Bool(sock:Int)="monkeyWS::has_data"
' :(synchronous) returns the most recent element from the queue
Function socket_get_data:CString(sock:Int)="monkeyWS::get_data"
' :closes a websocket. close code(int) and close reason(string) are optional. Code defaults at 1000.
Function socket_close:Void(sock:Int, code:Int, reason:CString)="monkeyWS::close"
' :Tells if the websocket had an error.
Function socket_has_error:Bool(sock:Int)="monkeyWS::has_error"
' :gets the error message from the error.
Function socket_error_message:CString(sock:Int)="monkeyWS::error_message"
' :WebSocket API for determining how much of the buffered data has been sent.
Function socket_buffered_amount:Int(sock:Int)="monkeyWS::buffered_amount"
' :WebSocket API for detailing what type of data is being sent to the server.
Function socket_binary_type:CString(sock:Int)="monkeyWS::binary_type"

' NOTE, FOR A WEBSOCKET API OVERVIEW VISIT: https://developer.mozilla.org/en-US/docs/Web/API/WebSocket
	
Public

Enum ReadyStates
	CONNECTING = 0	
	OPEN =	1
	CLOSING	= 2	
	CLOSED	= 3
End

Alias SockMonkey:WebSocket
Class WebSocket
	' url: websocket url
	' protocols: see websocket api.
	Method New( url: String, protocols: String[] = Null )
		_sock = socket_init()
		socket_set_url( _sock, url )
		For Local ptcol:=Eachin protocols
			socket_add_protocol( _sock, ptcol )
		Next
		_priorState = ReadyState
	End
	
	'Call when ready to connect.
	Method Connect()
		socket_connect(_sock)
	End
	
	Method Update()
		Local currentState := ReadyState
		If _priorState <> currentState Then 
			' :handle state transitions...
			Select currentState
				Case ReadyStates.OPEN
					OnOpen()
				Case ReadyStates.CLOSED
					OnClose()
			End
		Elseif currentState = ReadyStates.OPEN Then
			' :handle state messages
			While socket_has_data( _sock )
				OnData( socket_get_data(_sock) )
			Wend
		Endif
		_priorState = currentState
	End
	
	' :web socket api. (determines state of the connection)
	Property ReadyState:Int()
		Return socket_ready_state(_sock)
	End
	
	' :web socket api.
	Property BufferedAmount:Int()
		Return socket_buffered_amount(_sock)
	End
	
	' :web socket api.
	Property BinaryType:String()
		Return socket_binary_type(_sock)
	End

	' :Use only when connected. Sends data to host.
	Method Send( data: String) Virtual
		socket_send( _sock, data )
	End

	' :Forces the connection to terminate.
	Method Close( reason: String = "", code:Int = 1000 )
		socket_close(_sock, code, reason)
	End
Protected
	
	' :Called when a connection has succeeded
	Method OnOpen() Virtual
	End
	
	' :Called once the a connection has been terminated.
	Method OnClose() Virtual 
	End
	
	' :Called when data is recieved. 
	' :Passes data in as a string.
	Method OnData(data:String) Virtual 
	End
	
Private
	Field _sock:Int			' reference to our socket
	Field _priorState:Int 	' used in main-loop for mimicking events.
End

#Endif