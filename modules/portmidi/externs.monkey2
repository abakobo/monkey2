Namespace portmidi

#Import "pm_common/*.h"
#Import "porttime/*.h"

#Import "porttime/porttime.h"
#Import "porttime/porttime.c"

#Import "pm_common/portmidi.h"
#Import "pm_common/portmidi.c"
#Import "pm_common/pmutil.c"

#If __HOSTOS__="windows"

#Import "pm_win/pmwin.c"
#Import "pm_win/pmwinmm.c"
#Import "porttime/ptwinmm.c"

#import "<libadvapi32.a>"
#import "<libwinmm.a>"

#Endif

#If __HOSTOS__="macos"

#Import "<CoreMIDI.framework>"
#Import "<CoreAudio.framework>"
#Import "<CoreFoundation.framework>"
#Import "<Carbon.framework>"

#Import "pm_mac/finddefault.c"
#Import "pm_mac/pmmac.c"
#Import "pm_mac/pmmacosxcm.c"
#Import "pm_mac/readbinaryplist.c"
#Import "porttime/ptmac.c"

#Endif

#If __HOSTOS__="linux"

#Import "<libpthread.a>"
#import "pm_linux/finddefault.c"
#import "pm_linux/pmlinux.c"
#import "pm_linux/pmlinuxalsa.c"
#import "porttime/ptlinux.c"

#Endif

#If __HOSTOS__="pi"

#Import "<libasound.a>"
#Import "<libpthread.a>"
#import "pm_linux/finddefault.c"
#import "pm_linux/pmlinux.c"
#import "pm_linux/pmlinuxalsa.c"
#import "porttime/ptlinux.c"

#Endif

#Import "mididriver.h"

Extern

Class PmDeviceInfo Extends Void
	Field structVersion:Int
	Field interf:Byte Ptr		' underlying MIDI API, e.g. MMSystem or DirectX
	Field name:Byte Ptr		' device name, e.g. USB MidiSport 1x1 
	Field input:Int
	Field output:Int	
	Field opened:Int
End

Class MidiDriver Extends Void 
	Field deviceCount:Int
	Field error:Int
	Field errorText:Byte Ptr
	Field info:PmDeviceInfo

	Method Sleep(duration:Double)
	Method GetInfo(index:Int)	

	Method OpenOutput:int(index:Int)
	Method OutputMessage(index:Int,data:Int)
	Method OutputMessages(index:Int,bytes:Int Ptr,length:Int)
	Method CloseOutput(handle:int)

	Method OpenInput:int(index:Int)
	Method HasInput:Bool(handle:int)
	Method MidiEventData:Int()
	Method MidiEventMessage:int(buffer:Void Ptr,length:int)
	Method MidiEventTimestamp:Double()	
	Method CloseInput(handle:int)
End
