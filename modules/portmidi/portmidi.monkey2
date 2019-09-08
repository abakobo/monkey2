Namespace portmidi

#Import "<std>"

#If __DESKTOP_TARGET__
#Import "externs.monkey2"
#Else
#Import "nomidi.monkey2"
#Endif


Public

Alias HandleStack:std.collections.Stack<Int>
Alias HandleMap:std.collections.Map<Int,Int>

#rem monkeydoc Desktop MIDI courtesy PortMidi library.
#end

Class PortMidi
	Field version:="0.02"
	Field deviceCount:Int
	
	Field driver:MidiDriver
	
#rem monkeydoc A stack of available MIDI inputs.
#end
	Field inputDevices:=New HandleStack()
#rem monkeydoc A stack of available MIDI outputs.
#end
	Field outputDevices:=New HandleStack()
#rem monkeydoc A map of open MIDI inputs
#end
	Field openInputs:=New HandleMap()
#rem monkeydoc A map of open MIDI outputs
#end
	Field openOutputs:=New HandleMap()
	
#rem monkeydoc High resolution PortMidi sleep command.
#end
	Method Sleep(seconds:Double)
		driver.Sleep(seconds)
	End

#rem monkeydoc scan midi bus
#end
	Method New() 
		Print "PortMidi courtesy github/nitrologic/monkey2/modules"
		driver=New MidiDriver()
		Print "countDevices="+driver.deviceCount
		For Local id:=0 Until driver.deviceCount
			driver.GetInfo(id)
			if driver.info.input inputDevices.Push(id)
			If driver.info.output outputDevices.Push(id)
		Next
	End	
	
#rem monkeydoc close all open MIDI channels
#end
	Method CloseAll()
		Local indexes:=New HandleStack()
		For Local in:=Eachin openInputs.Keys
			indexes.Push(in)
		Next
		For Local index:=Eachin indexes
			CloseInput(index)
		Next
		indexes.Clear()
		For Local out:=Eachin openOutputs.Keys
			indexes.Push(out)
		Next
		For Local index:=Eachin indexes
			CloseOutput(index)
		Next
	end

#rem monkeydoc Name of input channel
#end
	Method InputName:String(index:Int)
		Local id:=inputDevices.Get(index)		
		driver.GetInfo(id)
		Local interf:=String.FromCString(driver.info.interf)
		Local  name:=String.FromCString(driver.info.name)
		Return name
	End

#rem monkeydoc Name of output channel
#end
	Method OutputName:String(index:Int)
		Local id:=inputDevices.Get(index)		
		driver.GetInfo(id)
		Local interf:=String.FromCString(driver.info.interf)
		Local  name:=String.FromCString(driver.info.name)
		Return name
	End
	
#rem monkeydoc Open specified MIDI input channel
#end
	Method OpenInput(index:Int)
		Local id:=inputDevices.Get(index)		
		Local input:=driver.OpenInput(id)
		Print "PortMidi Open input"+index+"@"+id+"="+input
		openInputs[index]=id
	End
		
#rem monkeydoc Open specified MIDI output channel
#end
	Method OpenOutput(index:Int)
		Local id:=outputDevices.Get(index)		
		Local output:=driver.OpenOutput(id)
		Print "PortMidi Open output"+index+"@"+id+"="+output
		openOutputs[index]=id
	End

#rem monkeydoc Send MIDI data to specified MIDI output channel
#end
	Method SendMessage(index:Int,data:Int)
		Local id:=outputDevices.Get(index)		
		driver.OutputMessage(id,data)
	End

#rem monkeydoc Send raw MIDI Sysex data (MSB ignored)
#end
	Method SendMessages(index:Int,data:int[])
		Local id:=outputDevices.Get(index)		
		driver.OutputMessages(id,data.Data,data.Length)
	End

#rem monkeydoc Close specified MIDI input channel
#end
	Method CloseInput(index:Int)
		Local id:=openInputs[index]
		driver.CloseInput(id)
		Print "PortMidi Close input"+index+"@"+id
		openInputs.Remove(index)
	end

#rem monkeydoc Close specified MIDI output channel
#end
	Method CloseOutput(index:Int)
		Local id:=openOutputs[index]
		driver.CloseOutput(id)
		Print "PortMidi Close output"+index+"@"+id
		openOutputs.Remove(index)
	end

#rem monkeydoc Returns number of MIDI events in queue
#end
	Method HasEvent:Int()
		For Local index:=Eachin openInputs.Keys
			Local id:=openInputs[index]
			if driver.HasInput(id) Return true
		Next
		Return False
	End
	
#rem monkeydoc Return packed 4 bytes of current MIDI event
#end
	Method EventData:Int()
		Return driver.MidiEventData()
	End

#rem monkeydoc Return separate 4 bytes of current MIDI event
#end
	Method EventDataBytes:UByte[]()
		Local i:=EventData()
		Return new UByte[](i&$ff,(i Shr 8)&$ff,(i Shr 16)&$ff,(i Shr 24)&$ff)
	End

#rem monkeydoc Return timestamp of event in seconds
#end
	Method EventTime:Double()
		Return driver.MidiEventTimestamp()
	End

	Field sysexBuffer:=New Byte[65536]

#rem monkeydoc Return contents of Sysex message
#end
	Method EventContent:Byte[]()		
		Local b:=Varptr sysexBuffer[0]
		Local n:=driver.MidiEventMessage(b,65536)
		Return sysexBuffer.Slice(0,n)
	End

End
