Namespace pyro.framework.objectpool

#rem monkeydoc The ObjectPool class.
#end
Class ObjectPool<T>

#rem monkeydoc Adds an oject to the pool.
#end
	Method Add:Int( obj:T )

		If _length=_data.Length
			_data=ResizeArray( _data,_length*2+10 )
			_inUse=ResizeArray( _inUse,_length*2+10 )
		Endif

		_data[_length]=obj

		_length+=1

		Return _length-1

	End

#rem monkeydoc Empties the pool.
#end
	Method Clear()
		For Local i:=0 Until _length
			_data[i]=Null
			_inUse[i]=False
		Next
		_length=0
	End

#rem monkeydoc Return the number of objects in the pool.
#end
	Method Count:Int()
		Return _length
	End

#rem monkeydoc Changes the state of the object to 'available'.
#end
	Method Free:Bool( obj:T )
		For Local i:=0 Until _length
			If _data[i]=obj
				_inUse[i]=False
				Return True
			Endif
		Next
		Return False
	End

#rem monkeydoc Changes the state of the object to 'available'.
#end
	Method Free( index:Int )
		_inUse[index]=False
	End

#rem monkeydoc Changes the state of all the objects to 'available'.
#end
	Method FreeAll()
		For Local i:=0 Until _length
			_inUse[i]=False
		Next
	End

#rem monkeydoc Returns the object at the specified index.
#end
	Method Get:T( index:Int )
		Return _data[index]
	End

#rem monkeydoc Returns the first available object in the pool.
#end
	Method GetAvailable:T()

		Local count:=_length

		While count>0

			Local i:=_index

			If _inUse[i]=False

				_inUse[i]=True

				_index+=1
				If _index>_length-1 _index=0

				Return _data[i]

			Endif

			_index+=1
			If _index>_length-1 _index=0

			count-=1

		Wend

		Return Null

	End

	Private

	Field _data:T[]
	Field _index:=0
	Field _inUse:Bool[]
	Field _length:=0

End