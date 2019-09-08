Namespace ws_demo
#Import "<std>"
#Import "<mojo>"
#Import "<html-ws>"
Using std..
Using mojo..
Using html.ws..

Class DemoSock Extends WebSocket
	Field Messages:Deque<String>
	Method New()
		Super.New("ws://echo.websocket.org/")
		Messages = New Deque<String>()
	End
	Method Send(data:String) Override
		If Self.ReadyState = ReadyStates.OPEN Then
			Super.Send(data)
		End
	End
	Method AddMessage(msg:String)
		Messages.PushLast(msg)
		If Messages.Length > 15 Then 
			Messages.PopFirst()
		Endif
	End
Protected
	Method OnData(data:String) Override
		AddMessage( "Recieved: " + data )
	End
	
	Method OnOpen() Override
		AddMessage("Socket established connected!")
	End
	
	Method OnClose() Override
		AddMessage("Socket terminated!")
	End
End

Class MyWindow Extends Window
	Field socket:DemoSock
	Field sends:Int = 0

	Method New( title:String="Websocket Demo", width:Int=640,height:Int=480,flags:WindowFlags=Null )
		Super.New( title,width,height,flags )
		ClearColor= Color.Blue
		socket = New DemoSock
	End

	Method OnRender( canvas:Canvas ) Override
		App.RequestRender()
		canvas.DrawText( "Monkey Web Sockets.   Enter*connects* __ Space*sends hello* __ Esc *closes*, ",0,0 )
		socket.Update()
		
		Local I:= 0
		For Local msg:=Eachin socket.Messages
			I += 1
			canvas.DrawText( msg, 20, 40 + I * 20 )
		Next
	End
	
	Method OnKeyEvent( event:KeyEvent ) Override
		If event.Type = EventType.KeyDown Then
			Select event.Key
				Case Key.Space
					socket.Send("Hello # " + sends )
					sends += 1
				Case Key.Enter
					socket.AddMessage("*initiating a connection*")
					socket.Connect()
				Case Key.Escape
					socket.AddMessage("*attempting to close connection*")
					socket.Close()
			End 
		End
	End
	
End

Function Main()

	New AppInstance
	
	New MyWindow
	
	App.Run()
End

