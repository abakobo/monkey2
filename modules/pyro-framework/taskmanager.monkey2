Namespace pyro.framework.taskmanager

#rem monkeydoc @hidden
#end
Function CopyTask:Task( source:Task,copy:Task=New Task )

	If source=Null Return Null
	If copy=Null Return Null

	copy._enabled=source._enabled
	copy.Name=source.Name
	copy.Order=source.Order
	copy.Pausable=source.Pausable
	copy.StartTime=source.StartTime
	copy._taskManager=source._taskManager
	copy._updateCounter=source._updateCounter

	Return copy

End

#rem monkeydoc Whether tasks are paused.
#end
Function DebugPause:Bool()
	Return _debugPause
End

#rem monkeydoc Whether tasks are paused.

Pauses all tasks even the ones that are not pause sensitive.

#end
Function DebugPause( debugPause:Bool )

	If debugPause=True _pauseTime=Millisecs() Else _timeShift+=Millisecs()-_pauseTime
	
	_debugPause=debugPause

End

Function Frozen:Bool()
	If DebugPause()=True Return True
	If Pause()=True Return True
	Return False
End

#rem monkeydoc @hidden
#end
Function Milliseconds:Int()

	If _pause=True Or _debugPause=True Return _pauseTime-_timeShift

	Return Millisecs()-_timeShift

End

#rem monkeydoc Whether tasks are paused.
#end
Function Pause:Bool()
	Return _pause
End

#rem monkeydoc Whether tasks are paused.

Pauses tasks that are pause sensitive.

#end
Function Pause( pause:Bool )

	If pause=True _pauseTime=Millisecs() Else _timeShift+=Millisecs()-_pauseTime

	_pause=pause

End

#rem monkeydoc Toggle Pause state.
#end
Function TogglePause()
	Pause( Not Pause() )
End

#rem monkeydoc Toggle DebugPause state.
#end
Function ToggleDebugPause()
	DebugPause( Not DebugPause() )
End

#rem monkeydoc The Task class.
#end
Class Task

	Field Identifier:=""
	#rem monkeydoc Name.
	#end
	Field Name:=""
	#rem monkeydoc @hidden
	#end
	Field Order:=0
	#rem monkeydoc Whether the task is pause sensitive.
	#end
	Field Pausable:=True
	#rem monkeydoc @hidden
	#end
	Field StartTime:=Milliseconds()

	#rem monkeydoc Whether the task is enabled.
	#end
	Property Enabled:Bool() Virtual
		Return _enabled
	Setter( enabled:Bool ) Virtual
		_enabled=enabled
	End

	#rem monkeydoc Task manager.
	#end
	Property TaskManager:TaskManager()
		Return _taskManager
	Setter( taskManager:TaskManager )
		If _taskManager _taskManager.Tasks.RemoveEach( Self )
		_taskManager=taskManager
		If _taskManager _taskManager.Tasks.Push( Self )
	End	

	#rem monkeydoc Called by the task manager.
	#end
	Method OnUpdate() Virtual
	End

	#rem monkeydoc @hidden
	#end
	Field _enabled:=True
	#rem monkeydoc @hidden
	#end
	Field _taskManager:TaskManager
	#rem monkeydoc @hidden
	#end
	Field _updateCounter:=0

End

#rem monkeydoc The TaskManager class.
#end
Class TaskManager

	#rem monkeydoc Name.
	#end
	Field Name:=""
	#rem monkeydoc Tasks.
	#end
	Field Tasks:=New Stack<Task>

	#rem monkeydoc Whether tasks are paused.

	Pauses all tasks even the ones that are not pause sensitive.

	#end
	Property DebugPause:Bool()

		Return _debugPause

	Setter( debugPause:Bool )

		If debugPause=True _pauseTime=Millisecs() Else _timeShift+=Millisecs()-_pauseTime

		_debugPause=debugPause

	End

	#rem monkeydoc Whether tasks are paused.

	Pauses tasks that are pause sensitive.

	#end
	Property Pause:Bool()

		Return _pause

	Setter( pause:Bool )

		If pause=True _pauseTime=Millisecs() Else _timeShift+=Millisecs()-_pauseTime

		_pause=pause

	End

	Method Update()

		Update( _updateCounter )

		_updateCounter+=1

	End

	Method Update( updateCounter:Int )

		Local i:=0
	
		While i<Tasks.Length
		
			Local task:=Tasks.Get( i )

			If _debugPause=True i+=1 ; Continue
			If task._updateCounter=updateCounter i+=1 ; Continue
			If task._enabled=False i+=1 ; Continue
			If task.Pausable=True And _pause=True i+=1 ; Continue

			Local tasks:=Tasks.Length

			task.OnUpdate()

			If Tasks.Length<tasks i+=( Tasks.Length-tasks )

			If i<0 Exit

			i+=1

		Wend

	End

	Private
	
	Field _updateCounter:=0

End

Private

Global _debugPause:=False
Global _pause:=False
Global _pauseTime:=0
Global _timeShift:=0
