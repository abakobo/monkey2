Namespace pyro.framework.pathfinder

#rem monkeydoc The Pathfinder class.
#end
Class Pathfinder

	Method New()
	End

#rem monkeydoc Returns the height.
#end
	Property Height:Int()
		Return _size.Y
	End

#rem monkeydoc Length of the prepared path.
#end
	Property Length:Int()
		Return _path.Length-1
	End

#rem monkeydoc Returns the size ( width/height ).
#end
	Property Size:Vec2i()
		Return _size
	End

#rem monkeydoc Returns the width.
#end
	Property Width:Int()
		Return _size.X
	End

#rem monkeydoc The prepared path as an array.
#end
	Method GetPathArray:String[]()
		Return _path
	End
	
#rem monkeydoc The prepared path as a string.
#end
	Method GetPathString:String()
		Local path:=""
		For Local i:=0 Until _path.Length-1
			path+=_path[i]
		Next
		Return path
	End

#rem monkeydoc Search for path.

Must first be prepared with SetMap!

If a path is found Search will return True and GetPathArray or GetPathString can be used to get the path.
#end
	Method Search:Bool()

		Const CHECK:=1
		Const CHECKED:=2

		Local location:=0
		Local nothingToCheck:=False
		Local finishFound:=False
		
		Repeat

			location+=1

			nothingToCheck=True

			For Local x:=0 Until _size.X
				For Local y:=0 Until _size.Y
				 	If _data[x,y]._location=location
				 		_data[x,y]._state=CHECK
				 		nothingToCheck=False
				 	Endif
				Next
			Next

			If nothingToCheck=True finishFound=False ; Exit

			If _data[_finish.X,_finish.Y]._state=CHECK
				finishFound=True
				Exit
			Endif

			For Local x:=0 Until _size.X
				For Local y:=0 Until _size.Y
				 	If _data[x,y]._state=CHECK
				 		_CheckNeighbours( x,y,location+1 )
				 		_data[x,y]._state=CHECKED
				 	Endif
				Next
			Next
		Forever	

		If finishFound=False
			_Reset()
			Return False
		Endif

		_TracePath()

		Return True
		
	End

#rem monkeydoc Sets the data needed for a search.
#end
	Method SetMap( data:Int[,],start:Vec2i,finish:Vec2i )

		_start=start
		_finish=finish

		_size.X=data.GetSize( 0 )
		_size.Y=data.GetSize( 1 )

		_data=_Init( _size.X,_size.Y )

		For Local x:=0 Until _size.X

			For Local y:=0 Until _size.Y

				If _data[x,y]=Null _data[x,y]=New Data()

				If data[x,y]=0
					_data[x,y]._location=PASSABLE
				Else
					_data[x,y]._location=UNPASSABLE
				Endif

			Next

		Next

		_data[_start.X,_start.Y]._location=1

	End

	Private

	Method _CheckNeighbours( x:Int,y:Int,value:Int )

		' Above
		If y-1>=0
			If _data[x,y-1]._location=PASSABLE
				_data[x,y-1]._location=value
			Endif
		End If

		' Right
		If x+1<_size.X
			If _data[x+1,y]._location=PASSABLE
				_data[x+1,y]._location=value
			Endif	
		End If

		' Below
		If y+1<_size.Y
			If _data[x,y+1]._location=PASSABLE
				_data[x,y+1]._location=value
			Endif
		End If

		' Left
		If x-1>=0
			If _data[x-1,y]._location=PASSABLE
				_data[x-1,y]._location=value
			Endif
		End If

	End

	Method _Reset()

		For Local x:=0 Until _size.X
			For Local y:=0 Until _size.Y
				_data[x,y]._location=PASSABLE
				_data[x,y]._state=0
			Next
		Next

		_path=ResizeArray( _path,0 )

	End
	
	Method _TracePath()

		Local location:=_data[_finish.X,_finish.Y]._location

		Local x:=_finish.X
		Local y:=_finish.Y

		_path=ResizeArray( _path,location )

		Repeat

			location-=1

			If location=0 Exit
			If y-1>=0
				If _data[x,y-1]._location=location
					_path[location-1]="D"
					y-=1
					Continue
				Endif
			Endif

			If x+1<_size.X
				If _data[x+1,y]._location=location
					_path[location-1]="L"
					x+=1
					Continue
				Endif
			Endif

			If y+1<_size.Y
				If _data[x,y+1]._location=location
					_path[location-1]="U"
					y+=1
					Continue
				Endif
			Endif

			If x-1>=0
				If _data[x-1,y]._location=location
					_path[location-1]="R"
					x-=1
					Continue
				Endif
			Endif

		Forever

	End

	Function _Init:Data[,]( width:Int,height:Int )

	    Local data:=New Data[width,height]

	    For Local x:=0 Until data.GetSize( 0 )
		    For Local y:=0 Until data.GetSize( 1 )
		        data[x,y]=New Data
		    Next
	    Next

    	Return data

	End Function

	Const PASSABLE:=0
	Const UNPASSABLE:=-1

	Field _data:Data[,]
	Field _finish:Vec2i
	Field _path:String[]
	Field _size:Vec2i
	Field _start:Vec2i

End

Private

Class Data
	Field _location:=0
	Field _state:=0
End