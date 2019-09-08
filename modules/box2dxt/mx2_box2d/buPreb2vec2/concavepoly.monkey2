Namespace box2dxt.polytools

#Import "<std>"
#Import "extsandfuncs.monkey2"
#Import "bayazit_src/bayazit.h"
#Import "hmconvex_src/polypartition.h"
#Import "hmconvex_src/polypartition.cpp"

Using std..
Using box2d..
Using box2dxt..

Extern

Function OptConvexor:BayazitPoint[][](BayazitPoint[])="p2bayazit::BayazitConvexor"
	
Struct BayazitPoint="p2bayazit::Point"
	Field x:Double
	Field y:Double
End

Function NOptConvexor:TPPLPoint[][](TPPLPoint[])
	
Struct TPPLPoint
	Field x:Double
	Field y:Double
End
	
Public

Function BayazitConvexor:Stack<Stack<Vec2d>>(poly:Stack<Vec2d>)
	
		If poly.Top=poly[0] Then poly.Pop()
		Local tpoly:=New BayazitPoint[poly.Length]
		For Local i:=0 Until poly.Length
			tpoly[i].x=poly[i].x
			tpoly[i].y=poly[i].y
		Next
		Local convexResult:=OptConvexor(tpoly)

		Local convertedResult:=New Stack<Stack<Vec2d>>
		
		For Local i:=0 Until convexResult.Length
			convertedResult.Add(BayaPArrToV2dStack(convexResult[i]))
		Next

		
		Return convertedResult
End

Function HMConvexor:Stack<Stack<Vec2d>>(poly:Stack<Vec2d>)
	
		If poly.Top=poly[0] Then poly.Pop()
		Local tpoly:=New TPPLPoint[poly.Length]
		For Local i:=0 Until poly.Length
			tpoly[i].x=poly[i].x
			tpoly[i].y=poly[i].y
		Next
		Local convexResult:=NOptConvexor(tpoly)

		Local convertedResult:=New Stack<Stack<Vec2d>>
		
		For Local i:=0 Until convexResult.Length
			convertedResult.Add(TPPLPArrToV2dStack(convexResult[i]))
		Next

		
		Return convertedResult
End

Function ConvexPartitionOpt:Stack<Stack<b2Vec2>>(poly:Stack<b2Vec2>)
	Return V2dStastackTob2vStastack(ConvexPartitionOpt(b2vStackToV2dStack(poly)))
End

Function ConvexPartitionOpt:Stack<Stack<Vec2d>>(poly:Stack<Vec2d>)

	Local convexResult:=BayazitConvexor(poly)
	Print "convResult size: "+convexResult.Length
	PrintPolyStack(convexResult)
	
	Local retStack:=New Stack<Stack<Vec2d>>
	
	For Local convPoly:=Eachin convexResult
		
		If convPoly=Null
			Continue
		End
		If convPoly.Length<3
			Continue
		End
		convPoly=CleanMinSlopes(convPoly)
		If IsPolyCollinearOrLessThan3Bayazit(convPoly)
			Continue
		End
		Local tStack:=New Stack<Vec2d>
		For Local pt:=Eachin convPoly
			Local v2d:=New Vec2d(pt.x,pt.y)
			tStack.Add(v2d)
		Next
		retStack.Add(tStack)
	Next
	
	Return retStack
	
End

Function ConvexPartitionNOpt:Stack<Stack<Vec2d>>(poly:Stack<Vec2d>)

	Local convexResult:=HMConvexor(poly)
	
	Local tStastack:=New Stack<Stack<Vec2d>>
	
	For Local convPoly:=Eachin convexResult
		
		If convPoly=Null Then Continue
		'Print "l: "+convPoly.Length
		If convPoly.Length<3 Then Continue
		'convPoly=CleanMinSlopes(convPoly)
		'If IsPolyCollinearOrLessThan3Bayazit(convPoly) Then Continue
		Local tStack:=New Stack<Vec2d>
		For Local pt:=Eachin convPoly
			Local v2d:=New Vec2d(pt.x,pt.y)
			tStack.Add(v2d)
		Next
		tStastack.Add(tStack)
	Next
	
	tStastack=Max8Polys(tStastack)
	Local retStack:=New Stack<Stack<Vec2d>>
	
	For Local convPoly:=Eachin tStastack
		If convPoly.Length<3 Then Continue
		convPoly=CleanMinSlopes(convPoly)
		If IsPolyCollinearOrLessThan3Bayazit(convPoly) Then Continue
		retStack.Add(convPoly)
	Next
	
	Return retStack
	
End


Function CleanMinSlopes:Stack<Vec2d>(p:Stack<Vec2d>)
	
	If p.Top<>p[0] Then p.Add(p[0])
	Local tp:=New Stack<Vec2d>
	For Local i:=0 Until p.Length-1
		If (Abs(p[i].x-p[i+1].x)<=0.005) And (Abs(p[i].y-p[i+1].y)<=0.005)
			'trop près
		Else
			tp.Add(p[i])
		End
	Next
	
	p.Pop()
	If tp.Top=p[0] Then tp.Pop()
	
	Return tp
	
End

Private

Function IsPolyCollinearOrLessThan3Bayazit:Bool(p:Stack<Vec2d>)
	

	If p.Length>2
		If p.Top<>p[0] Then p.Add(p[0])
	End
		
	If p.Length>3
		
		For Local i:=0 Until p.Length-2
		
			Local pa:=New Vec2d(p[i].x,p[i].y)
			Local pb:=New Vec2d(p[i+1].x,p[i+1].y)
			Local pc:=New Vec2d(p[i+2].x,p[i+2].y)
			
			Local v1:=pa-pb
			Local v2:=pb-pc
			
			Local angle:=Abs(v1.SignedAngleWith(v2))
			'Print "angle: "+angle
			#rem
			v1=v1.Normalize()
			v2=v2.Normalize()
			If v1.x<0 Then v1=-v1
			If v2.x<0 Then v2=-v2
			If v1.x=0
				v1.y=Abs(v1.y)
			End
			If v2.x=0
				v2.y=Abs(v2.y)
			End

			
			If Abs(v1.x-v2.x)>0.001 Or Abs(v1.y-v2.y)>0.001
				p.Pop()
				Return False
			End
			#End
			If angle<=Pi/2.0
				If angle>0.002
					p.Pop()
					Return False
				End
			Elseif angle>Pi/2.0
				If Abs(Pi-angle)>0.002
					p.Pop()
					Return False
				End
			End	
		Next
		
	End
	
	If p.Length>1 Then p.Pop()
	
	Return True
	
End

Function BayaPArrToV2dStack:Stack<Vec2d>(in:BayazitPoint[])
	Local out:=New Stack<Vec2d>
	For Local i:=0 Until in.Length
		out.Add(New Vec2d(in[i].x,in[i].y))
	Next
	Return out
End

Function TPPLPArrToV2dStack:Stack<Vec2d>(in:TPPLPoint[])
	Local out:=New Stack<Vec2d>
	For Local i:=0 Until in.Length
		out.Add(New Vec2d(in[i].x,in[i].y))
	Next
	Return out
End

Public

Function Max8Poly:Stack<Stack<Vec2d>>(poly:Stack<Vec2d>)
	
	Local tPoly:=poly.Copy()
	If tPoly.Top=tPoly[0] Then tPoly.Pop()
	Local retStastack:=New Stack<Stack<Vec2d>>	
	
	While tPoly.Length>8
		Local splitPoly:=New Stack<Vec2d>
		For Local i:=0 To 7
			splitPoly.Add(tPoly[i])
		Next
		retStastack.Add(splitPoly)
		
		Local ntPoly:=New Stack<Vec2d>
		ntPoly.Add(tPoly[0])
		ntPoly.Add(tPoly[7])
		For Local i:=8 Until tPoly.Length
			ntPoly.Add(tPoly[i])
		Next
		tPoly=ntPoly
	Wend
	retStastack.Add(tPoly)
	
	Return retStastack
	
End

Function Max8Polys:Stack<Stack<Vec2d>>(polys:Stack<Stack<Vec2d>>)
	
	Local retStastack:=New Stack<Stack<Vec2d>>
	
	For Local p:=Eachin polys
		Local splitPolys:=Max8Poly(p)
		retStastack.AddAll(splitPolys)	
	Next
	
	Return retStastack
	
End

Private

