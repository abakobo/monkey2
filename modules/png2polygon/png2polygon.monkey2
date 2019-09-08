
Namespace png2polygon

#Import "<std>"

Using std..


Class PNG2Polygon
	Field outline:Stack<Point>
	Field simplyfied_outline:Stack<Point>
	Field pixmap:Pixmap
	Field data:Int[]
	
	Method Trace:Stack<Point>(path:String,tolerance:Float=1,_simplifyDouglasPeucker:Bool=True,
_simplifyRadialDistance:Bool=False,_toPoly:Bool=True,startX:Int=0, startY:Int=0, w:Int=-1, h:Int=-1)
		Local startPixelX:Int = -1
		Local startPixelY:Int = -1
		Local start:Point
		
		pixmap = Pixmap.Load(path)
		If w=-1 Then w=pixmap.Width
		If h=-1 Then h=pixmap.Height

		data = New Int[w*h]
  		Local first:Point
        Local firstPrev:Point
		For Local y:Int = h-1 To 0 Step -1
			If first = Null Then
				firstPrev = New Point(0,y-1)
			End
			For Local x:Int = 0 Until w-1
				Local j:Int = pixmap.GetPixelARGB(x,y)
				data[y * w + x] = (j & $ff000000) | ( (j & $00ff0000) Shr 16) | (j & $0000ff00) | ( (j & $000000ff) Shl 16)
				If first = Null Then
					If isPixelSolid(x,y,w) Then
						first = New Point(x,y)
					End
					If first = Null Then 
						firstPrev = New Point(x,y)
					End
				End
			Next
		Next

		If first <> Null And firstPrev<>Null Then
			outline = moorNeighbor(first,firstPrev,w,h)

			If outline.Length>2 Then
				If _simplifyRadialDistance Then
					simplifyRadialDistance(outline,tolerance)
				ElseIf _simplifyDouglasPeucker Then
					simplifyDouglasPeucker(outline,tolerance)
				End

				If _toPoly Then
					toPolyData(outline)
				End
			End
		Else
			Print "can't find the start pixel"
		End

		data = null
		Return outline
	End

	Method isPixelSolid:Bool(x:int, y:int,w:Int)
		Return (data[y * w + x] shr 24) & $FF > 0
	End
	
	Method moorNeighbor:Stack<Point>(first:Point,firstPrev:Point,w:Int,h:Int)
		Local clockwiseOffset:StringMap<Point> = New StringMap<Point>()
		clockwiseOffset.Set("1,0",New Point(1,-1))
		clockwiseOffset.Set("1,-1",New Point(0,-1))
		clockwiseOffset.Set("0,-1",New Point(-1,-1))
		clockwiseOffset.Set("-1,-1",New Point(-1,0))
		clockwiseOffset.Set("-1,0",New Point(-1,1))
		clockwiseOffset.Set("-1,1",New Point(0,1))
		clockwiseOffset.Set("0,1",New Point(1,1))
		clockwiseOffset.Set("1,1",New Point(1,0))

		Local out:Stack<Point> = New Stack<Point>()
		Local prev:Point
        Local curr:Point
        Local boundary:Point

		prev = firstPrev
		out.Push(first)
		boundary = first

		curr = Clockwise(clockwiseOffset,boundary,prev)
		Local stopLoop:Bool = False
		Local startCorner:Bool = False
 		While stopLoop = False

            If curr.y >= 0 And curr.x >= 0 And curr.y < h And curr.x < w And isPixelSolid(curr.x,curr.y,w) Then
            	
				If startCorner Then
					If (boundary.x-curr.x)=-1 And (boundary.y-curr.y)=0 Then
						out.Insert(out.Length-1,New Point(boundary.x-1,boundary.y))
						startCorner=False
					ElseIf (boundary.x-curr.x)=0 And (boundary.y-curr.y)=-1 Then
						out.Insert(out.Length-1,New Point(boundary.x,boundary.y-1))
						startCorner=False
					ElseIf (boundary.x-curr.x)=1 And (boundary.y-curr.y)=0 Then
						out.Insert(out.Length-1,New Point(boundary.x+1,boundary.y))
						startCorner=False
					ElseIf (boundary.x-curr.x)=0 And (boundary.y-curr.y)=1 Then
						out.Insert(out.Length-1,New Point(boundary.x,boundary.y+1))
						startCorner=False
					End
            	End

				out.Push(curr)

				' create innercorners
				If startCorner=False And (((curr.x-boundary.x)=1 And (curr.y-boundary.y)=-1) Or ((curr.x-boundary.x)=1 And (curr.y-boundary.y)=1) Or ((curr.x-boundary.x)=-1 And (curr.y-boundary.y)=1) Or ((curr.x-boundary.x)=-1 And (curr.y-boundary.y)=-1)) Then
            		startCorner = True
            	End

            	prev = boundary
            	boundary = curr
            	curr = Clockwise(clockwiseOffset,boundary, prev)
            Else
            	prev = curr
            	curr = Clockwise(clockwiseOffset,boundary, prev)
           	End
           	stopLoop = ((curr.x = first.x And curr.y = first.y) OR (prev.x = firstPrev.x And prev.y = firstPrev.y))
        Wend

	
        Return out
	End

	Method Clockwise:Point(_clockwiseOffset:StringMap<Point>,target:Point,prev:Point)
		Local clock:Point = _clockwiseOffset.Get((prev.x - target.x)+","+(prev.y - target.y))
		Return New Point(clock.x+target.x,clock.y+target.y) 
	End

	Method getSquareDistance:Float(p1:Point, p2:Point)
    	Local dx:Float = p1.x - p2.x
    	Local dy:Float = p1.y - p2.y
    	Return dx * dx + dy * dy
    End
	Method simplifyRadialDistance:Stack<Point>(points:Stack<Point>, tolerance:Float=1)
		Local length:Int = points.Length
		Local prev_point:Point = points.Get(0)
		Local new_points:Stack<Point> = New Stack<Point>()
		new_points.Push(prev_point)

		Local point:Point

		For Local i:Int = 0 Until length
			point = points.Get(i)
			If getSquareDistance(point, prev_point) > tolerance Then
            	new_points.Push(point)
            	prev_point = point
            End
		Next

		If prev_point <> point Then
        	new_points.Push(point)
        End

        simplyfied_outline = new_points
        Return new_points
	End

	Method getSquareSegmentDistance:Float(p:Point, p1:Point, p2:Point)
	    Local x:Float = p1.x
	    Local y:Float = p1.y

	    Local dx:Float = p2.x - x
	    Local dy:Float = p2.y - y

	    If dx <> 0 or dy <> 0 Then
	        Local t:Float = ((p.x - x) * dx + (p.y - y) * dy) / (dx * dx + dy * dy)

	        If t > 1 Then
	            x = p2.x
	            y = p2.y
	        Elseif t > 0 Then
	            x += dx * t
	            y += dy * t
	        End
	    End

	    dx = p.x - x
	    dy = p.y - y

	    Return dx * dx + dy * dy
    End

	Method simplifyDouglasPeucker:Stack<Point>(points:Stack<Point>, tolerance:Float=1)
		Local length:Int = points.Length
		Local markers:Int[] = New Int[length]

		Local f:Int = 0
		Local l:Int = length-1

		Local first_stack:IntStack = New IntStack()
		Local last_stack:IntStack = New IntStack()
		Local new_points:Stack<Point> = New Stack<Point>()

		markers[f] = 1
		markers[l] = 1

		Local index:Int = 0
		Local max_sqdist:Float = 0
		Local stop:Bool = False

		While stop=False
			max_sqdist = 0

			For Local i:Int = f Until l
				Local sqdist:Float = getSquareSegmentDistance(points.Get(i), points.Get(f), points.Get(l))
				If sqdist > max_sqdist Then
                	index = i
                	max_sqdist = sqdist
                End
			Next

			If max_sqdist > tolerance Then
	            markers[index] = 1

	            first_stack.Push(f)
	            last_stack.Push(index)

	            first_stack.Push(index)
	            last_stack.Push(l)
	        End

	        If first_stack.Empty Then
	        	f = 0
	        Else
	        	f = first_stack.Pop()
	        End

	        If last_stack.Empty Then
	        	l = 0
	        	stop = True
	        Else
	        	l = last_stack.Pop()
	        End
		End

		For Local i:Int = 0 Until length
        	If markers[i] Then
            	new_points.Push(points.Get(i))
           	End
        Next

       	simplyfied_outline = new_points
       	Return new_points
	End


	Method toPolyData:Float[](points:Stack<Point>)
		Local verts:Float[] = New Float[points.Length*2]
		Local tmpI:Int = 0

		For Local point:Point = Eachin points
			verts[tmpI] = point.x
			tmpI=tmpI+1
			verts[tmpI] = point.y
			tmpI=tmpI+1
		Next

		Return verts
	End
End


Class Point
	Field x:Int
	Field y:Int

	Method New(_x:Int,_y:Int)
		x=_x
		y=_y
	End
End