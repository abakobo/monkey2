Namespace box2dxt.polytools

#Import "<std>"
#Import "extsandfuncs.monkey2"

Using std..
Using box2d..
Using box2dxt..

Function PolyCutSided:Stack<Stack<Stack<Vec2d>>>(poly:Stack<Vec2d>,cutEdges:Stack<Vec2d>)
	
	Local polail:=New PolyAIL(poly,True)
	Local LRStack:=PolyCutRecSided(polail,cutEdges)
	
	Local left:=New Stack<Stack<Vec2d>>
	Local right:=New Stack<Stack<Vec2d>>
	Local ret:=New Stack<Stack<Stack<Vec2d>>>
	
	For Local pail:=Eachin LRStack
		If pail.isLeft=True
			Local tleft:=cleanStraights(pail.poly)
			If tleft<>Null
				left.Add(tleft)
			End
		Else
			Local tright:=cleanStraights(pail.poly)
			If tright<>Null
				right.Add(tright)
			End
		End
	Next
	
	ret.Add(left)
	ret.Add(right)
	
	Return ret

End

'Function PolyCutSided:Stack<Stack<Stack<b2Vec2>>>(poly:Stack<b2Vec2>,cutEdges:Stack<b2Vec2>)
'	Return V2dStastastackTob2vStastastack(PolyCutSided(b2vStackToV2dStack(poly),b2vStackToV2dStack(cutEdges)))
'End

'Function PolyCut:Stack<Stack<b2Vec2>>(poly:Stack<b2Vec2>,cutEdges:Stack<b2Vec2>)
'	Return V2dStastackTob2vStastack(PolyCut(b2vStackToV2dStack(poly),b2vStackToV2dStack(cutEdges)))
'End

Function PolyCut:Stack<Stack<Vec2d>>(poly:Stack<Vec2d>,cutEdges:Stack<Vec2d>)
	
	Local polail:=New PolyAIL(poly,True)
	Local LRStack:=PolyCutRecSided(polail,cutEdges)
	
	Local ret:=New Stack<Stack<Vec2d>>
	
	For Local pail:=Eachin LRStack
			Local tleft:=cleanStraights(pail.poly)
			If tleft<>Null
				ret.Add(tleft)
			End
	Next
	
	Return ret

End

Private

Function PolyCutRecSided:Stack<PolyAIL>(polygon:PolyAIL,cutEdges:Stack<Vec2d>)
	'poly must be convex
	'poly must be CCW
	'poly may not contain straigths (consecutive parallel edges)
	'knife may not self intersect
	If polygon=Null Then Return Null 'Print "Null poly!!!!!!!!!!!!!!!!!!!"
	Local knife:=New Stack<Vec2d>
	'Print "preknif: "+cutEdges.Length
				'For Local p:=Eachin cutEdges
				'	Print p
				'Next
	For Local p:=Eachin cutEdges
		If InPolyExclLim(p,polygon.poly) And knife.Length>0
			knife.Add(p)
		Elseif Not InPolyExclLim(p,polygon.poly)
			knife.Add(p)
		End
	End
		
	While knife.Length>1 And InPolyExclLim(knife.Top,polygon.poly)
		knife.Pop()
	End

	'Print "knifeé"+knife.Length
	'For Local p:=Eachin knife
	'	Print p
	'Next
	
	Local retPolys:=New Stack<PolyAIL>
	If knife.Length<2
		'Print "miniknifeInput"
		retPolys.Add(polygon)
		Return retPolys
	End
	
	
	Local poly:=ExtremeLeftise(polygon.poly)
	poly.Add(poly[0])
	'Print "preInter:Polylength "+poly.Length
	Local interKnife:=New Stack<PIP>
	Local totInts:=0
	Local tangeantI:=0
	
	Local isEdgeExtreme:=False
	
	
	For Local i:=0 Until knife.Length-1
		Local kline:=New Line2D(knife[i],knife[i+1]-knife[i])
		'Print "kline:"+knife[i]+" "+knife[i+1]
		For Local j:=0 Until poly.Length-1
			Local pline:=New Line2D(poly[j],poly[j+1]-poly[j])
			'Print "pline:"+poly[j]+" "+poly[j+1]
			Local pab:=pline.SegmentIntersectsPAB(kline)
			If pab.b=True
				'Print "interrrr: "+i+" "+j
				'Print pab.p
				If totInts=1
					'Print "TotInts=1 "+interKnife.Length
					If pab.p<>interKnife[0].intPoint 'si lintersection est juste la jonction entre deux edges
						'Print "same???: "+pab.p+" "+interKnife[0].intPoint
						totInts+=1
						interKnife.Add(New PIP(pab.p,i,j))
						'Print "interknifeAdd: "+interKnife.Length
					Else
						'Print "intersection is edge extreme, skipping one"
						isEdgeExtreme=True
					End
				Else
					'Print "TotInts- "+totInts+" interkL"+interKnife.Length
					totInts+=1
					interKnife.Add(New PIP(pab.p,i,j))
					'Print "interknifeAdd: "+interKnife.Length
				End
			Else
				'Print "No Intersection"
			End

			If totInts=2 Then Exit
			
		Next
		Local kIn:=InPolyExclLim(knife[i],poly)
		Local kpIn:=InPolyExclLim(knife[i+1],poly)
		If (Not kIn) And (Not kpIn)
			If totInts=1 
				'Print "PasRESSET intTOTDS"
				totInts=0
				interKnife.Pop()
			End
			
		End
		If totInts=2 Then Exit
	Next
	
	'Print "preknifeChain"
	'Print "interL: "+interKnife.Length
	'Print "knifeL: "+knife.Length
	Local polyA:=New Stack<Vec2d>
	Local polyB:=New Stack<Vec2d>
	
	If interKnife.Length>2
		#If __DEBUG__
			Print "Error: more than 2 intersection for poly cutting. returning empty Stack"
		#End
		Return retPolys
	End
	
	Local ptBaseIntLR:Vec2d
	Local ptDirLR:Vec2d
	Local ptNormLR:Vec2d
	
	If interKnife.Length<2
		
		'Print "noCutHere----------------------"
		retPolys.Add(polygon)
		Return retPolys
		
	Elseif interKnife.Length=2
		Local knifeChain:=New Stack<Vec2d>

		If interKnife[0].polyi=interKnife[1].polyi
			'Print "CCCUUUUUUTING ON THE SAME LINE !!!!!!!!!!!!!!!"
			If poly[interKnife[0].polyi].SqDistance(interKnife[0].intPoint)>poly[interKnife[0].polyi].SqDistance(interKnife[1].intPoint)
				interKnife.Reverse()
			End
		Elseif interKnife[0].polyi>interKnife[1].polyi
			interKnife.Reverse()
		End
		knifeChain.Add(interKnife[0].intPoint)

		Local baseLRBegins:Bool

		
		Local diff:=interKnife[1].knifi-interKnife[0].knifi
		If diff=0
			Print "diff=0"
			knifeChain.Add(interKnife[1].intPoint)
			
			baseLRBegins=True
			ptBaseIntLR=interKnife[0].intPoint
			ptDirLR=knife[interKnife[0].knifi+1]
				Print "Equal?"
				Print ptBaseIntLR
				Print ptDirLR
			If ptBaseIntLR=ptDirLR
				ptDirLR=ptBaseIntLR+(knife[interKnife[0].knifi+1]-knife[interKnife[0].knifi])
			End
		Else If diff>0
			'Print "diff>0"
			baseLRBegins=True
			ptBaseIntLR=interKnife[0].intPoint
			ptDirLR=knife[interKnife[0].knifi+1]
			For Local i:=interKnife[0].knifi Until interKnife[1].knifi
				knifeChain.Add(knife[i+1])
			Next
			knifeChain.Add(interKnife[1].intPoint)
		Else If diff<0
			'Print "diff<0$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
			baseLRBegins=False
			ptBaseIntLR=interKnife[1].intPoint
			ptDirLR=knife[interKnife[1].knifi+1]
			For Local i:=interKnife[0].knifi-1 To interKnife[1].knifi Step-1
				knifeChain.Add(knife[i+1])
			Next
			knifeChain.Add(interKnife[1].intPoint)
		End

		'Print "preLR"
		'faire poly starting from 0
		For Local i:=0 To interKnife[0].polyi
			polyA.Add(poly[i])
		Next
		For Local i:=0 Until knifeChain.Length
			polyA.Add(knifeChain[i])
		Next
		For Local i:=interKnife[1].polyi+1 Until poly.Length
			polyA.Add(poly[i])
		Next
		
		'faire autre poly 
		For Local i:=interKnife[0].polyi+1 To interKnife[1].polyi
			polyB.Add(poly[i])
		Next
		For Local i:=knifeChain.Length-1 To 0 Step -1
			polyB.Add(knifeChain[i])
		Next
		
		'If polyA=Null Then Print "here it's NullAAAAAAAAAAAAAA"
		'If polyB=Null Then Print "here it's NullBBBBBBBBBBBBBB"
		'LR
		If baseLRBegins=True
			
			ptNormLR=poly[interKnife[0].polyi+1]
			If ptNormLR=ptBaseIntLR
				Print "=In baseTrue$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
				ptNormLR=ptNormLR+(poly[interKnife[0].polyi+1]-poly[interKnife[0].polyi])
			End
		Else
			
			ptNormLR=poly[interKnife[1].polyi]
			If ptNormLR=ptBaseIntLR
				Print "=In baseFalse$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
				ptNormLR=ptNormLR+(poly[interKnife[1].polyi+1]-poly[interKnife[1].polyi])
			End
		End
	End
	
	'déterminer gauche/droite
	Local lRVD:=ptDirLR-ptBaseIntLR
	Local lRVN:=ptNormLR-ptBaseIntLR
	'Print "anglus"
	'Print lRVD
	'Print lRVN
	Local pailA:PolyAIL
	Local pailB:PolyAIL



	If lRVD.SignedAngleWith(lRVN)=0
	'	If polyA=Null Then Print "here it's NullAAAAAAAAAAAAAA"
	'	If polyB=Null Then Print "here it's NullBBBBBBBBBBBBBB"
		'Print "ANANANANANANANANGLE 0"
		
	Elseif lRVD.SignedAngleWith(lRVN)>0
		'Print lRVD.SignedAngleWith(lRVN)
		'Print "posAngle"
		pailA=New PolyAIL (polyA,True)
		pailB=New PolyAIL (polyB,False)
	Else
		'Print lRVD.SignedAngleWith(lRVN)
		'Print "NegAngle?"
		pailA=New PolyAIL (polyA,False)
		pailB=New PolyAIL (polyB,True)
	End	
	
	'Virer premier "knife"
	Local kIndex:=interKnife[0].knifi
	If kIndex<interKnife[1].knifi Then kIndex=interKnife[1].knifi
	kIndex+=1
	Local tknife:=New Stack<Vec2d>
	For Local i:=kIndex Until knife.Length
		tknife.Add(knife[i])
	Next
	knife=tknife
	Local retSta:=New Stack<PolyAIL>
	If knife.Length<2 
		retSta.Add(pailA)
		retSta.Add(pailB)
	Else
		'Print "rFirestRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR"+knife.Length
		retSta.AddAll(PolyCutRecSided(pailA,knife))
		'Print "rSecondRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR"+knife.Length
		retSta.AddAll(PolyCutRecSided(pailB,knife))
	End
	
	Return retSta
	
End

Public

Function PolyHole:Stack<Stack<Stack<Vec2d>>>(polygon:Stack<Vec2d>,cutPoly:Stack<Vec2d>)
	
	'cutPoly must be simple (non-self intersecting)
	'polygon must be convex
	
	cutPoly=MakeCCW(cutPoly)
	If cutPoly.Top<>cutPoly[0] Then cutPoly.Add(cutPoly[0])
	If cutPoly.Length<4
		#If __DEBUG__
				Print"Warning: poly Length<3 for PolyHole returning empty Stack"
		#End
		Return New Stack<Stack<Stack<Vec2d>>>
	End
	
	If polygon.Top<>polygon[0] Then polygon.Add(polygon[0])
	If polygon.Length<4
		#If __DEBUG__
				Print"Warning: poly Length<3 for PolyHole returning empty Stack"
		#End
		Return New Stack<Stack<Stack<Vec2d>>>
	End
	
	
	Local time:=Millisecs()
	Local outPointIndex:=-1 'si un point est strictement à l'extérieur
	For Local i:=0 Until cutPoly.Length-1
		If InPolyInclLim(cutPoly[i],polygon)=False
				outPointIndex=i
		End
	Next
	Print "point ext: "+(Millisecs()-time)

	If outPointIndex<>-1
	'	Print "outpoint"
		Local newCutPoly:=New Stack<Vec2d>
		time=Millisecs()
		For Local i:=outPointIndex Until cutPoly.Length-1
			newCutPoly.Add(cutPoly[i])
		Next
		For Local i:=0 Until outPointIndex
			newCutPoly.Add(cutPoly[i])
		Next
		Print "replacement polyext: "+(Millisecs()-time)
		newCutPoly.Add(newCutPoly[0])
		time=Millisecs()
		Local returnPoly:=PolyCutSided(polygon,newCutPoly)
		Print "Polycut: "+(Millisecs()-time)
		Return returnPoly
	Else 'Si completement dans poly limit incl alors split le poly
		'Print "Inpoints"
		Local pa:Vec2d
		Local pb:Vec2d
		
		Local indoxMilieu:Int=(cutPoly.Length+1)/2
		Local vda:=cutPoly[indoxMilieu]-cutPoly[indoxMilieu-1]
		Local vdb:=cutPoly[1]-cutPoly[0]
		vda=vda*(Pi/7.0) '(pour éviter que ça tombe pile sur les vertices de polygones réguliers)
		vdb=vdb*(Pi/6.0)
		pa=cutPoly[indoxMilieu-1]+vda
		pb=cutPoly[0]+vdb
	

		
		Local cutLine:=New Line2D(pa,pb-pa)
		Local cutLineIntersections:=New Stack<PointAndInt>
		For Local i:=0 Until polygon.Length-1
			Local tLine:=New Line2D(polygon[i],polygon[i+1]-polygon[i])
			Local cutPAB:=cutLine.LineSegmentIntersectsPAB(tLine)
			If cutPAB.b=True
			'	Print "intersect"+cutPAB.p
				If cutLineIntersections.Length=0
					cutLineIntersections.Add(New PointAndInt(cutPAB.p,i))
				Else
					If cutPAB.p<>cutLineIntersections[0].p
					'	Print "meme point"
						cutLineIntersections.Add(New PointAndInt(cutPAB.p,i))
					End
					If cutLineIntersections.Length=2 Then Exit
				End	
			End
		Next
		If cutLineIntersections.Length<2
			#If __DEBUG__
				Print"ROPROBLEM cutLineIntersection<2 returning empty Stack"
			#End
			Return New Stack<Stack<Stack<Vec2d>>>
		End
		
		Local polygonA:=New Stack<Vec2d>
		Local polygonB:=New Stack<Vec2d>

		
		If cutLineIntersections[0].i>cutLineIntersections[1].i Then cutLineIntersections.Reverse()
		
		For Local i:=0 To cutLineIntersections[0].i
			polygonA.Add(polygon[i])
		Next
		polygonA.Add(cutLineIntersections[0].p)
		polygonA.Add(cutLineIntersections[1].p)
		For Local i:=cutLineIntersections[1].i+1 Until polygon.Length-1
			polygonA.Add(polygon[i])
		Next
		
		For Local i:=cutLineIntersections[0].i+1 To cutLineIntersections[1].i
			polygonB.Add(polygon[i])
		Next
		polygonB.Add(cutLineIntersections[1].p)
		polygonB.Add(cutLineIntersections[0].p)	
		
		Local polygons:=New Stack<Stack<Vec2d>>
		polygons.AddAll(ConvexPolyValidator(polygonA))
		polygons.AddAll(ConvexPolyValidator(polygonB))
		'Maintennant le 'gros' poly est coupé en deux avec un pt exterieur à cutPoly de chaque côté
		
		
		
		Local retStack:=New Stack<Stack<Stack<Vec2d>>>
		retStack.Add(New Stack<Stack<Vec2d>>)
		retStack.Add(New Stack<Stack<Vec2d>>)
		
		
		'mettage de point extérieur en premier avce d'appeler polycutsided
		For Local i:=0 Until polygons.Length
		'	Print "CCCCCUUUUUUUUTING i: "+i
			Local newCutPoly:Stack<Vec2d>
			Local outPointI:=-1 'si un point est strictement à l'extérieur
			For Local j:=0 Until cutPoly.Length-1
				If InPolyInclLim(cutPoly[j],polygons[i])=False
					outPointI=j
				End
			Next
			If outPointI<>-1
			'	Print "outpoint"
				newCutPoly=New Stack<Vec2d>
				
				For Local j:=outPointI Until cutPoly.Length-1
					newCutPoly.Add(cutPoly[j])
				Next
				For Local j:=0 Until outPointI
					newCutPoly.Add(cutPoly[j])
				Next
				
				newCutPoly.Add(newCutPoly[0])
			Else
				#If __DEBUG__
					Print"ROPROBLEM split hole has no outside point: returning empty Stack"
				#End
				Return New Stack<Stack<Stack<Vec2d>>>
			End
			
			Local stacky:=PolyCutSided(polygons[i],newCutPoly)

			retStack[0].AddAll(stacky[0])
			retStack[1].AddAll(stacky[1])
			
		Next
	Return retStack
		
	End
	#If __DEBUG__
		Print"ROPROBLEM cutHole à pas marché! returning empty Stack"
	#End
	Return New Stack<Stack<Stack<Vec2d>>>
	
End

Function ConvexPolyValidator:Stack<Stack<Vec2d>>(polygon:Stack<Vec2d>)
	
	Local tPoly:=CleanMinSlopes(polygon)
	Local ret:=Max8Poly(tPoly)
	For Local i:=0 Until ret.Length
		ret[i]=MakeCCW(ret[i])
	End
	Return ret
	
End




Private

Struct PIP
	
	Field intPoint:Vec2d
	Field knifi:Int
	Field polyi:Int
	
	Method New(iPoint:Vec2d,knifeIndex:Int,polySegmentIndex:Int)
		intPoint=iPoint
		knifi=knifeIndex
		polyi=polySegmentIndex
	End
	
	
End

Struct PolyAIL
	Field poly:Stack<Vec2d>
	Field isLeft:Bool
	Method New(p:Stack<Vec2d>,l:Bool)
		poly=p
		isLeft=l
	End
End

Function ExtremeLeftise:Stack<Vec2d>(poly:Stack<Vec2d>)
	
	If poly.Top=poly[0] Then poly.Pop()
	
	Local extrmIndex:=0
	For Local i:=0 Until poly.Length
		If poly[i].x<poly[extrmIndex].x
			extrmIndex=i
		End
	Next
	
	Local retStack:=New Stack<Vec2d>
	
	For Local i:=extrmIndex Until poly.Length
		retStack.Add(poly[i])
	Next
	If extrmIndex<>0
		For Local i:=0 Until extrmIndex
			retStack.Add(poly[i])
		Next
	End
	
	Return retStack
	
End

Function IsLeft:Float( P0:Vec2d, P1:Vec2d, P2:Vec2d )
	return ( (P1.x - P0.x) * (P2.y - P0.y) - (P2.x - P0.x) * (P1.y - P0.y) )
End

Public

Function InPoly:Bool( P:Vec2d, V:Stack<Vec2d> )
	
	'	Copyright 2000 softSurfer, 2012 Dan Sunday
	'// This code may be freely used and modified for any purpose
	'// providing that this copyright notice is included with it.
	'// SoftSurfer makes no warranty for this code, and cannot be held
	'// liable for any real or imagined damage resulting from its use.
	'// Users of this code must verify correctness for their application.
	
	If V[0]<>V.Top Then V.Add(V[0])

	Local wn := 0
	For Local i:=0 To V.Length-2
		If V[i].y <= P.y		
			If V[i+1].y	> P.y		
					If IsLeft( V[i], V[i+1], P) > 0	
						wn+=1		
					End
			End
		Else							
			If V[i+1].y <= P.y
					If IsLeft( V[i], V[i+1], P) < 0	
						wn-=1		
					End
			End
		End
	Next
	If wn<>0 Then Return True
	Return False
End

Function InPolyExclLim:Bool(point:Vec2d , poly:Stack<Vec2d>)
	
	If poly.Length<3 Then Return False
	
	Local Win:=InPoly(point,poly)
	If poly[0]<>poly.Top Then poly.Add(poly[0])
	If Win=True
		For Local i:=0 Until poly.Length-1
			
			Local lin:=New Line2D(poly[i],poly[i+1]-poly[i])
			
			If lin.IsCollinearAndInsideSegment(point) Then Return False
			
		Next
	End
	
	Return Win
	
End

Function InPolyInclLim:Bool (point:Vec2d, poly:Stack<Vec2d>)

	If poly.Length<3 Then Return False
	
	Local Win:=InPoly(point,poly)
	If poly[0]<>poly.Top Then poly.Add(poly[0])
	If Win=False
		For Local i:=0 Until poly.Length-1
			
			Local lin:=New Line2D(poly[i],poly[i+1]-poly[i])
			
			If lin.IsCollinearAndInsideSegment(point) Then Return True
			
		Next
	End
	
	Return Win
	
End

Private

Function PrintPoly(p:Stack<Vec2d>)
	
	Print "Polyprint: "
	For Local pt:=Eachin p
		Print pt
	Next
	Print "Endpolyprint"
	
End


