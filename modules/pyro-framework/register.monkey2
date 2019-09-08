Namespace pyro.framework.register

Using pyro.framework.arrays

#rem monkeydoc @hidden
#end
Class Register<T>

	Field Data:T[]
	Field Length:=0

	Method New()
	End

	Method Add:Int( obj:T,name:String,group:String="" )

		If Length=Data.Length
			Data=ResizeArray( Data,Length*2+10 )
			_group=ResizeArray( _group,Length*2+10 )
			_name=ResizeArray( _name,Length*2+10 )
		Endif

		Data[Length]=obj
		_group[Length]=group
		_name[Length]=name

		Length+=1

		Return Length-1

	End

	Method Clear()
		For Local i:=0 Until Length
			Data[i]=Null
'			_group[i]=""
'			_name[i]=""
		Next
		Length=0
	End

	Method Get:T( name:String,group:String="" )
		For Local i:=0 Until Length
			If _name[i]=name And _group[i]=group Return Data[i]
		Next
		Return Null
	End

	Method ToString:String()
		Local out:=""
		For Local i:=0 Until Length
			If out out+="~n"
			out+="Name="+_name[i]
			If _group[i]<>"" out+=", Group="+_group[i]
		Next
		Return out
	End

	Field _group:String[]
	Field _index:=0
	Field _name:String[]

End