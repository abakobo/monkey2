Namespace box2dxt.extsandfuncs

#Import "<std>"
#Import "<mojo>"

Using std..
Using mojo..
Using box2d..
Using box2dxt..



Class Canvas Extension
	Method SetCameraByCenter(point:Vec2f,zoom:Float=1.0,rotation:Float=0.0)
		Translate(Viewport.Width/2.0,Viewport.Height/2.0)
		Scale(zoom,zoom)
		Rotate(rotation)
		Translate(-point)
	End
	Method SetCameraByVisibleBottomAndCenter(bottom:Vec2f,center:Vec2f,height:Int ,rotation:Float=0)
		Local dheight:=bottom.y-center.y
		Local zoom:=(height/2.0)/dheight
		SetCameraByCenter(center,zoom,rotation)	
	End
	
	Method SetCameraByBottomAndCenterXAndZoom(bottom:Float,centerx:Float,zoom:Float , screenHeight :Int ,rotation:Float=0)
		Local center:=New Vec2f(centerx,0)
		Local canvasHeight:=screenHeight/zoom
		center.y=bottom-canvasHeight/2.0
		SetCameraByCenter(center,zoom,rotation)	
	End
	
	Method GetCenterViewPoint:Vec2f()
		Return -Self.Matrix*New Vec2f(Viewport.Width/2,Viewport.Height/2)
	End
	
	Method GetRect:Rectf()
		Local c1:=-Self.Matrix*New Vec2f(0,0)
		Local c2:=-Self.Matrix*New Vec2f(Viewport.Width,Viewport.Height)
		Return New Rectf(c1,c2)
	End
	
End



Struct AffineMat3<T> Extension

	Method GetScale:Vec2f()
	
		Local sxsq:=Pow(Self.i.x,2)+Pow(Self.i.y,2)
		Local sysq:=Pow(Self.j.x,2)+Pow(Self.j.y,2)
		
		If sxsq=sysq 'to gain calculation time because I generaly use 1:1 scales
			Local sc:=Sqrt(sxsq)
			Return New Vec2f (sc,sc)
		Else
			Return New Vec2f (Sqrt(sxsq),Sqrt(sysq))
		Endif
	
	End
	
	Method GetRotation:Float()
		Local f:Float=Self.j.y 'to prevent integer division giving integer
		If Self.j.y <>0
			Return ATan(-Self.i.y/f)	
		Else
			Print "Matrix is not a valid transform, could not get Rotation" 'should throw instead of print ..?
			Return 0.0
		Endif
	End
	
	Method GetTranslation:Vec2<T>()
		Return Self.t
	End


End

'-------------------------------------------
'
'   vec2d angle ext
'
'-------------------------------------

Alias Vec2d:Vec2<Double>

Struct Vec2<T> Extension
	
	'- Pi to Pi
	Method SignedAngleWith:Double(v:Vec2<T>)
		If (Self.Length=0) Or (v.Length=0)
			'#If __DEBUG__
			'	Print"Warning: null Vec2<T> for angle, returning zero"
			'#End
			Return 0
		End
		
		Local dot := Self.x*v.x + Self.y*v.y    '  # dot product between [x1, y1] and [x2, y2]
		Local det := Self.x*v.y - Self.y*v.x     ' # determinant
		Return ATan2(det, dot) ' # atan2(y, x) or atan2(sin, cos)
	End
	
	'0 to 2Pi
	Method PositiveAngleWith:Double(v:Vec2<T>)
		If (Self.Length=0) Or (v.Length=0)
			'#If __DEBUG__
			'	Print"Warning: null Vec2<T> for angle, returning zero"
			'#End
			Return 0
		End
		
		Local dot := Self.x*v.x + Self.y*v.y    '  # dot product between [x1, y1] and [x2, y2]
		Local det := Self.x*v.y - Self.y*v.x     ' # determinant
		Local a:= ATan2(det, dot) ' # atan2(y, x) or atan2(sin, cos)
		If a<0 Then a=2*Pi+a
		Return a
	End
	
	
	Method Cross:Double(v:Vec2<T>)
		Return x * v.y - y * v.x
	End
	
	Property SqLength:Double()
		
		Return (x*x)+(y*y)
		
	End
	
	Method SqDistance:Double(v:Vec2<T>)
		
		Local tv:=v-Self
		Return (tv.x*tv.x)+(tv.y*tv.y)
		
	End
		
End


'--------------------------------------
'
'  Stack ext shuffle
'
'--------------------------------------

Class Stack<T> Extension
	
	Method Shuffle()
		For Local i:=0 Until Self.Length
			Self.Swap(i,Int(Rnd(0,Self.Length-0.00000001)))
		Next
	End
	Method Reverse()
		For Local i:=0 Until Self.Length/2
			Self.Swap(i,Self.Length-1-i)
		Next
	End
	Method Copy:Stack<T>()
		Local ret:=New Stack<T>
		For Local e:=Eachin Self
			ret.Add(e)
		Next
		Return ret
	End

End

'---------------------------------------
'
'    Structs with points/vects/Line  (geoms)
'
'---------------------------------------

Struct Line2D
	
	Field o:Vec2<Double>
	Field d:Vec2<Double>
	
	Method New(origin:Vec2<Double>,directionSegment:Vec2<Double>)
		o=origin
		d=directionSegment
	End
	
	Method IsParallel:Bool(line:Line2D)
		If Self.d.Normalize()<>line.d.Normalize() And Self.d.Normalize()<>-line.d.Normalize()
			Return False
		Else
			Return True
		End
	End
	
	Method IsOkToIntersect:Bool(line:Line2D)

		Local divisor:Double=1.0*(1.0*line.d.y*Self.d.x*1.0)-1.0*(1.0*Self.d.y*1.0*line.d.x)
		
		If divisor<1.0e-35 And divisor>-1.0e-35
			Return False

		End
		
		Return True

		
	End
	
	Method Intersection:Vec2<Double>(line:Line2D)

		Local divisor:Double=1.0*(1.0*line.d.y*Self.d.x*1.0)-1.0*(1.0*Self.d.y*1.0*line.d.x)

		If divisor=0.0
			#If __DEBUG__
				Print"Error: divisor=0 returning Vec2 Null(0,0)! You should use Line2D.Parallel before calling Line2D.intersection!"
			#End
			Return Null
		End
		
		Local multiplor:Double=(Self.d.x*Self.o.y)+Self.d.y*(line.o.x-Self.o.x)-Self.d.x*line.o.y
		Local tline:=multiplor/divisor
		Local intersectPoint:=line.o+(line.d*tline)
		
		Return intersectPoint

	End
	
	#rem FAUT corriger et utiliser même métode que dans intersectPAB
	'donne false pour ligne égales même si en vrai elles on infinité d'intersections et false si touche le bord
	Method SegmentIntersectsLimitsExcluded:Bool(line:Line2D)
		
		If Not Self.IsOkToIntersect(line) Then Return False
		
		Local inter:=Self.Intersection(line)
		Local intx:=inter.x
		Local inty:=inter.y
		Local o2:=o+d
		Local selfMinx:=Min(o.x,o2.x)
		Local selfMaxx:=Max(o.x,o2.x)
		Local selfMiny:=Min(o.y,o2.y)
		Local selfMaxy:=Max(o.y,o2.y)
		
		Local insideSelf:=False
		
		If ((selfMinx<inter.x) And (inter.x<selfMaxx)) Or ((selfMinx=selfMaxx) And (selfMinx=inter.x))
			If ((selfMiny<inter.y) And (inter.y<selfMaxy)) Or ((selfMiny=selfMaxy) And (selfMiny=inter.y))
				insideSelf=True
			Else
				Return False 'pour zapper y
			End
		End
	
		Local lineo2:=line.o+line.d
		Local lineMinx:=Min(line.o.x,lineo2.x)
		Local lineMaxx:=Max(line.o.x,lineo2.x)
		Local lineMiny:=Min(line.o.y,lineo2.y)
		Local lineMaxy:=Max(line.o.y,lineo2.y)
		Local insideLine:=False
		If ((lineMinx<inter.x) And (inter.x<lineMaxx)) Or ((lineMinx=lineMaxx) And (lineMinx=inter.x))
			If ((lineMiny<inter.y) And (inter.y<lineMaxy)) Or ((lineMiny=lineMaxy) And (lineMiny=inter.y))
				insideLine=True
			End
		End
		
		If insideSelf And insideLine Then Return True
		
		Return False
		
	End
	#end
	
	Method IsCollinearAndInsideSegment:Bool(p:Vec2d)
		
			Local dp:=p-Self.o
			
			Local insideSelf:=False
			
			Local epsilon:Double=0.2e-52
			
			If Abs(dp.SignedAngleWith(Self.d))<0.001
				If dp.SqLength<=Self.d.SqLength+epsilon
					insideSelf=True
				End
			End
			
			Return insideSelf
			
	End
	
	Method SegmentIntersectsPAB:PointAndBool(line:Line2D)
			
			If Not Self.IsOkToIntersect(line)
				Return New PointAndBool(New Vec2d(0,0),False)
			End
			
			Local inter:=Self.Intersection(line)
			Local dSelf:=inter-Self.o
			Local dLine:=inter-line.o
			
			
			Local epsilon:Double=0.2e-20
			Local insideSelf:=False
			If Abs(dSelf.SignedAngleWith(Self.d))<0.001 Or Self.o.SqDistance(inter)<1e-5
				'Print "TinangleA°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°"
				If dSelf.SqLength<=Self.d.SqLength+epsilon
					insideSelf=True
				'Else
				'	Print "pasinsideSelf: "+dSelf.SqLength+" <= "+Self.d.SqLength+" ?"
				End
			'Else
				'Print "RonangleA°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°"
				'Print "d: "+Self.o.Distance(inter)+" : "+line.o.Distance(inter)
			End
			
			Local insideLine:=False
			If Abs(dLine.SignedAngleWith(line.d))<0.001 Or line.o.SqDistance(inter)<1e-5
				'Print "TinangleB°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°"
				'Print "insideLine: "+dLine.SqLength+" <= "+line.d.SqLength+" ?"
				If dLine.SqLength<=line.d.SqLength+epsilon
					insideLine=True
				'Else
				'	Print "NotinsideLine: "
				End
			'Else
			'	Print "RonangleB°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°"
			'	Print "d: "+Self.o.Distance(inter)+" : "+line.o.Distance(inter)
			End
			
			If insideSelf And insideLine
				Return New PointAndBool(inter,True)
			End
			
			Return New PointAndBool(inter,False)
			
		End
		
		Method LineSegmentIntersectsPAB:PointAndBool(segmentLine:Line2D)
			
			Local line:=segmentLine
			
			If Not Self.IsOkToIntersect(line)
				Return New PointAndBool(New Vec2d(0,0),False)
			End
			
			Local inter:=Self.Intersection(line)
			Local dSelf:=inter-Self.o
			Local dLine:=inter-line.o
			Local epsilon:Double=0.2e-52
			
			Local insideLine:=False
			If Abs(dLine.SignedAngleWith(line.d))<0.001
			'	Print "Tinangle°°°°°°°°°°°°°°°°°°°°°°°°°°"
				If dLine.SqLength<=line.d.SqLength+epsilon
					insideLine=True
				End
			'Else
			'	Print "GrosNangle°°°°°°°°°°°°°°°°°°°°°°°°°°°°"
			End
			
			If insideLine=True
				Return New PointAndBool(inter,True)
			End
			
			Return New PointAndBool(inter,False)
			
		End
		
		Method IsCollinear:Bool(l:Line2D) 
			
			If Self.IsParallel(l)
				Local p2:=Self.o+Self.d
				If ((l.o.y - p2.y) * (p2.x - Self.o.x) = (p2.y - Self.o.y) * (l.o.x - p2.x))
					Return True
				End
			End
			
			Return False
			
		End

End

Struct PointAndBool
	
	Field p:Vec2d
	Field b:Bool
	
	Method New(point:Vec2d,bol:Bool)
		p=point
		b=bol
	End
End

Struct PointAndInt
	
	Field p:Vec2d
	Field i:Int
	
	Method New(point:Vec2d,in:Int)
		p=point
		i=in
	End
End

Struct SegmentPWI
	
	Field pp1:PointsPair
	Field pp2:PointsPair
	Field hasIntersection:Bool
	Field intersection:Vec2d
	
	Method New(paa:Vec2d,pab:Vec2d,pba:Vec2d,pbb:Vec2d)
		pp1=New PointsPair(paa,pab)
		pp2=New PointsPair(pba,pbb)
		
		Local line1:=pp1.ToLine2D()
		Local line2:=pp2.ToLine2D()
		
		Local pabool:PointAndBool=line1.SegmentIntersectsPAB(line2)
		
		hasIntersection=pabool.b
		intersection=pabool.p
				
	End
	
	Method Contains:Bool(pp:PointsPair)
		
		If pp1=pp Or pp2=pp Then Return True
		
		Return False
	
	End
	
	Operator =:Bool(s:SegmentPWI)
	
		If pp1=s.pp1 And pp2=s.pp2 Then Return True
		If pp1=s.pp2 And pp2=s.pp1 Then Return True 
		Return False
	End
	
End

Struct PointsPair
	
	Field p1:Vec2d
	Field p2:Vec2d
	
	Method New(pa:Vec2d,pb:Vec2d)
		p1=pa
		p2=pb
		End
	
	Method Contains:Bool(p:Vec2d)
		
		If p1=p Or p2=p Then Return True
		
		Return False
	
	End
	
	Method ToLine2D:Line2D()
		Return New Line2D(p1,p2-p1)
	End
	
	Operator =:Bool(pp:PointsPair)
	
		If p1=pp.p1 And p2=pp.p2 Then Return True
		If p1=pp.p2 And p2=pp.p1 Then Return True 
		Return False
	End
	
End

Function MakeCCW:Stack<b2Vec2>(pol:Stack<b2Vec2>)
	Return V2dStackTob2vStack(MakeCCW(b2vStackToV2dStack(pol)))
End

Function MakeCCW<T>:Stack<Vec2<T>>(poly:Stack<Vec2<T>>)
	
	If poly.Top=poly[0] Then poly.Pop()
	
	Local maxLeftPoint:=poly[0]
	Local maxLeftPointIndex:=0
	For Local i:=0 Until poly.Length
		If poly[i].x<maxLeftPoint.x
			maxLeftPoint=poly[i]
			maxLeftPointIndex=i
		End
	Next
	Local pa:Vec2<T>
	Local pb:Vec2<T>
	Local pc:Vec2<T>
	
	If maxLeftPointIndex=0
		pa=poly[poly.Length-1]
		pb=poly[0]
		pc=poly[1]
	Elseif maxLeftPointIndex=poly.Length-1
		pa=poly[poly.Length-2]
		pb=poly[poly.Length-1]
		pc=poly[0]
	Else
		pa=poly[maxLeftPointIndex-1]
		pb=poly[maxLeftPointIndex]
		pc=poly[maxLeftPointIndex+1]
	End
	
	Local vBegin:=pa-pb
	vBegin=vBegin.Normalize() 
	Local vEndin:=pc-pb
	vEndin=vEndin.Normalize()
	
	If vBegin.y=vEndin.y
		#If __DEBUG__
			Print "ERROR: exterior zero-angle: extreme left vertices are parallel and same direction (in MakeCCW) returning NULL"
		#End
		Return Null
	End
	If vBegin.y>vEndin.y
	 ' Print "CCW"
	  
	Else
	'	Print "not CCW"
		poly.Reverse()
	End
	
	Return poly
	
End

Function IsPolyCollinearOrLessThan3:Bool(p:Stack<Vec2d>)
	
	If p.Length>2
		If p.Top<>p[0] Then p.Add(p[0])
	End
	
	If p.Length>3
		
		For Local i:=0 Until p.Length-2
		
			Local line1:=New Line2D(p[i],p[i+1]-p[i])
			Local line2:=New Line2D(p[i+1],p[i+2]-p[i+1])
		
			If line1.IsCollinear(line2)=False Then Return False
					
		Next
		
	End
	
	Return True
	
End

Function IsPolyCollinearOrLessThan3:Bool(p:Stack<b2Vec2>)
	
	If p.Length>2
		If p.Top<>p[0] Then p.Add(p[0])
	End
	
	If p.Length>3
		
		For Local i:=0 Until p.Length-2
		
			Local line1:=New Line2D(p[i],p[i+1]-p[i])
			Local line2:=New Line2D(p[i+1],p[i+2]-p[i+1])
		
			If line1.IsCollinear(line2)=False Then Return False
					
		Next
		
	End
	
	Return True
	
End

'-----------------------
'
' convert funcs
'
'---------------------


Function b2vStackToV2dStack:Stack<Vec2d>(in:Stack<b2Vec2>)
	Local out:=New Stack<Vec2d>
	For Local i:=0 Until in.Length
		out.Add(New Vec2d(in[i].x,in[i].y))
	Next
	Return out
End

Function b2vStackToV2fStack:Stack<Vec2f>(in:Stack<b2Vec2>)
	Local out:=New Stack<Vec2f>
	For Local i:=0 Until in.Length
		out.Add(New Vec2d(in[i].x,in[i].y))
	Next
	Return out
End

Function b2vStastackToV2dStastack:Stack<Stack<Vec2d>>(in:Stack<Stack<b2Vec2>>)
	Local out:=New Stack<Stack<Vec2d>>
	For Local i:=0 Until in.Length
		out.Add(b2vStackToV2dStack(in[i]))
	Next
	Return out
End

Function b2vStastackToV2fStastack:Stack<Stack<Vec2f>>(in:Stack<Stack<b2Vec2>>)
	Local out:=New Stack<Stack<Vec2f>>
	For Local i:=0 Until in.Length
		out.Add(b2vStackToV2fStack(in[i]))
	Next
	Return out
End
' -----------
Function V2dStackTob2vStack:Stack<b2Vec2>(in:Stack<Vec2d>)
	Local out:=New Stack<b2Vec2>
	For Local i:=0 Until in.Length
		out.Add(New b2Vec2(in[i].x,in[i].y))
	Next
	Return out
End

Function V2fStackTob2vStack:Stack<b2Vec2>(in:Stack<Vec2f>)
	Local out:=New Stack<b2Vec2>
	For Local i:=0 Until in.Length
		out.Add(New b2Vec2(in[i].x,in[i].y))
	Next
	Return out
End

Function V2dStastackTob2vStastack:Stack<Stack<b2Vec2>>(in:Stack<Stack<Vec2d>>)
	Local out:=New Stack<Stack<b2Vec2>>
	For Local i:=0 Until in.Length
		out.Add(V2dStackTob2vStack(in[i]))
	Next
	Return out
End

Function V2dStastastackTob2vStastastack:Stack<Stack<Stack<b2Vec2>>>(in:Stack<Stack<Stack<Vec2d>>>)
	Local out:=New Stack<Stack<Stack<b2Vec2>>>
	For Local i:=0 Until in.Length
		out.Add(V2dStastackTob2vStastack(in[i]))
	Next
	Return out
End

Function V2fStastackTob2vStastack:Stack<Stack<b2Vec2>>(in:Stack<Stack<Vec2f>>)
	Local out:=New Stack<Stack<b2Vec2>>
	For Local i:=0 Until in.Length
		out.Add(V2fStackTob2vStack(in[i]))
	Next
	Return out
End

Class b2Body Extension
	Method copyParamsFrom(body:b2Body,copyTransform:Bool=False,copyUserData:Bool=False)
		If copyTransform=True 'position?
			Self.SetTransform(body.GetPosition(),body.GetAngle())
		End
		If copyUserData=True
			#If __DEBUG__
				Print "copy body User_Data not implemented yet!"
			#End
		End
		Self.SetActive(body.IsActive()) 	
		Self.SetAngularDamping(body.GetAngularDamping())
		Self.SetAngularVelocity(body.GetAngularVelocity())
		Self.SetAwake(body.IsAwake())
		Self.SetBullet(body.IsBullet())
		Self.SetFixedRotation(body.IsFixedRotation())
		Self.SetGravityScale(body.GetGravityScale())
		Self.SetLinearDamping(body.GetLinearDamping())
		Self.SetLinearVelocity(body.GetLinearVelocity())
		Self.SetSleepingAllowed(body.IsSleepingAllowed())
		Self.SetType(body.GetType())
	End
End

Class b2Fixture Extension
	Method copyParamsFrom(fixture:b2Fixture,copyIsSensor:Bool=False,copyUserData:Bool=False)
		
		Self.SetDensity(fixture.GetDensity())	
		Self.SetFilterData(fixture.GetFilterData())
		Self.SetFriction(fixture.GetFriction())	
		Self.SetRestitution(fixture.GetRestitution())
		If copyIsSensor Then Self.SetSensor(fixture.IsSensor())
		If copyUserData
				#If __DEBUG__
				Print "copy fixture User_Data not implemented yet!"
			#End
		'Self.SetUserData(fixture.Get)
		End
	End
End

Function PrintPoly(poly:Stack<b2Vec2>)
	For Local pt:=Eachin poly
		Print pt
	Next		
End
Function PrintPoly(poly:Stack<Vec2d>)
	For Local pt:=Eachin poly
		Print pt
	Next		
End

Function PrintPolyStack(polyStack:Stack<Stack<b2Vec2>>)
	For Local poly:=Eachin polyStack
	For Local pt:=Eachin poly
		Print pt
	Next
	Print "-endpoly"
	Next	
End

Function PrintPolyStack(polyStack:Stack<Stack<Vec2d>>)
	For Local poly:=Eachin polyStack
	For Local pt:=Eachin poly
		Print pt
	Next
	Print "-endpoly"
	Next	
End





