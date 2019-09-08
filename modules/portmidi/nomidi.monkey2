Namespace portmidi


Class PmDeviceInfo
	Field structVersion:Int
	Field interf:Byte Ptr		' underlying MIDI API, e.g. MMSystem or DirectX
	Field name:Byte Ptr		' device name, e.g. USB MidiSport 1x1 
	Field input:Int
	Field output:Int	
	Field opened:Int
End

Alias Handle:Int

Class MidiDriver
	Field deviceCount:Int
	Field error:Int
	Field errorText:Byte Ptr
	Field info:PmDeviceInfo

	Method Sleep(duration:Double)
	End
	
	Method GetInfo(index:Int)	
	End
	
	Method OpenOutput:Handle(index:Int)
		Return 0
	End
	
	Method OutputMessage(index:Int,data:Int)
	End
	
	Method OutputMessages(index:Int,bytes:Int Ptr,length:Int)
	End
	
	Method CloseOutput(handle:Handle)
	End
	
	Method OpenInput:Handle(index:Int)
		Return 0
	End
	
	Method HasInput:Bool(handle:Handle)
		Return False
	End
	
	Method MidiEventData:Int()
		Return 0
	End
	
	Method MidiEventMessage:int(buffer:Void Ptr,length:int)
		Return 0
	End
	
	Method MidiEventTimestamp:Double()	
		Return 0
	End
	
	Method CloseInput(handle:Handle)
	End
	
End
