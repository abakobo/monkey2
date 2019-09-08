Namespace box2dxt

#Import "<std>"
#Import "<mojo>"

#Import "<box2d>"
#Import "b2dJsonBodyImage.monkey2"
#Import "b2dJsonJoint.monkey2"
#Import "../iforce2d-b2djson/mx2b2djson.monkey2"

#Import "UserDataExtensions.monkey2"

#Import "complexpoly.monkey2"
#Import "concavepoly.monkey2"
#Import "polycutter.monkey2"
#Import "extsandfuncs.monkey2"

Using std..
Using mojo..
Using box2d..
Using mx2b2dJson..

Class b2Manager Extends Resource
	
	Field world:b2World
	
	Field physScale:Float
	Field yAxisInversion:=True
	
	Field timeStep:= 0.01666666667
	Field velocityIterations := 6
	Field positionIterations := 2
	
	Field bodyInfos:b2BodyImageInfo[]
	Field bodyImageMap:IntMap<Image>
	Field fixtureInfos:Stack<b2FixtureInfo>
	Field jointInfos:Stack<b2JointInfo>
	
	Field debugDrawer:b2DebugDraw
	
	Field b2dJsons:=New b2dJson[1]
	Field b2dJsonsCount:=0
	
	'Private
	Field sortedBodyImageInfos:=New Stack<b2BodyImageInfo>
	
	'Public
	
	'
	'
	' Manual Body Creation
	'
	'
	
	Method New(gravity:b2Vec2=New b2Vec2(0,-9.81),pScale:Float=15,yAxisInvert:Bool=True)
		
		physScale=pScale
		yAxisInversion=yAxisInvert
		
		world=New b2World(gravity)
		
		debugDrawer=New b2DebugDraw(physScale,yAxisInvert)
		
		world.SetDebugDraw( debugDrawer  )
		debugDrawer.SetFlags( e_shapeBit|e_jointBit )
		
		bodyInfos=New b2BodyImageInfo[0]
		bodyImageMap=New IntMap<Image>
		fixtureInfos=New Stack<b2FixtureInfo>
		jointInfos=New Stack<b2JointInfo>
			
	End
	
	
	
	Method CreateBody:b2Body(name:String="nonamebody",bd:b2BodyDef)
		
		Local l:=Self.bodyInfos.Length
		
		Self.bodyInfos=Self.bodyInfos.Resize(l+1)
		Self.bodyInfos[l]=New b2BodyImageInfo()
		Local bii:=Self.bodyInfos[l]
		bii.index=l
		bii.bodyName=name
		bii.body=Self.world.CreateBody(bd)
		bii.bodyUserData=New StringMap<Variant>
		bii.bodyUserData["b2ManagerBodyInfo"]=bii
		bii.body.SetUserData(Cast<Void Ptr>(bii.bodyUserData))
		
		Return bii.body
		
	End
	
	Method CreateStaticBox:b2Body( name:String="UnnamedBox" , width:Float,height:Float,initialPosition:b2Vec2,initialAngle:Float=0 ,  density:Float=1.0 , friction:Float=1.0 , restitution:Float=0.3 ,image:Image=Null , imageHeigth:Float=0.0 , imageStrech:Float=1.0 , imageHandle:Vec2f=New Vec2f(0.5,0.5) )
		
		Return Self.CreateBox(name,width,height,initialPosition,initialAngle,density,friction,restitution,image,imageHeigth,imageStrech,imageHandle,b2BodyType.b2_staticBody)
		
	End
	
	Method CreateBox:b2Body( name:String="UnnamedBox" , width:Float,height:Float,initialPosition:b2Vec2,initialAngle:Float=0 ,  density:Float=1.0 , friction:Float=1.0 , restitution:Float=0.3 ,image:Image=Null , imageHeigth:Float=0.0 , imageStrech:Float=1.0 , imageHandle:Vec2f=New Vec2f(0.5,0.5) , type:b2BodyType=b2BodyType.b2_dynamicBody)
	
			Local bd:b2BodyDef
			bd.type = type
			bd.position=initialPosition
			bd.angle = initialAngle
			Local body:=Self.CreateBody(name,bd)
			
			Local fd:b2FixtureDef
			fd.friction = friction
			fd.restitution = restitution
			fd.density = density
			
			Local pshape:=New b2PolygonShape()
			Local vs:=New b2Vec2[4]
			
			vs[0].Set(0.5*width, 0.5*height)
			vs[1].Set(-0.5*width, 0.5*height)
			vs[2].Set(-0.5*width, -0.5*height)
			vs[3].Set(0.5*width, -0.5*height)
			pshape.Set(vs.Data, 4)
			
			fd.shape = pshape
			
			Self.CreateFixture(name+"_Fixture",body,fd)
			
			If image<>Null
				If imageHeigth=0.0 Then imageHeigth=height
				image.Handle=imageHandle
				'ici add Image
			End
			
			Return body
		
	End
	
	Method CreateBall:b2Body( name:String="UnnamedBall" , radius:Float,initialPosition:b2Vec2,initialAngle:Float=0 ,  density:Float=1.0 , friction:Float=1.0 , restitution:Float=0.3 , type:b2BodyType=b2BodyType.b2_dynamicBody ,image:Image=Null , imageHeigth:Float=0.0 , imageStrech:Float=1.0 , imageHandle:Vec2f=New Vec2f(0.5,0.5) )
	
			Local bd:b2BodyDef
			bd.type = type
			bd.position=initialPosition
			bd.angle = initialAngle
			Local body:=Self.CreateBody(name,bd)
			
			Local fd:b2FixtureDef
			fd.friction = friction
			fd.restitution = restitution
			fd.density = density
			
			Local cshape:=New b2CircleShape()
			cshape.m_radius = 0.5
			cshape.m_p.Set(0.0, 0.0)
			
			fd.shape = cshape
			
			Self.CreateFixture(name+"_Fixture",body,fd)
			
			If image<>Null
				If imageHeigth=0.0 Then imageHeigth=2*radius
				image.Handle=imageHandle
				'ici add Image
			End
			
			Return body
		
	End
	
	Method CreatePolyBody:b2Body( name:String="UnnamedPolyBody" , poly:b2Vec2[] ,initialPosition:b2Vec2,initialAngle:Float=0 ,  density:Float=1.0 , friction:Float=1.0 , restitution:Float=0.3 , type:b2BodyType=b2BodyType.b2_dynamicBody ,image:Image=Null , imageHeigth:Float=0.0 , imageStrech:Float=1.0 , imageHandle:Vec2f=New Vec2f(0.5,0.5) )
	
			Local tStack:=New Stack<b2Vec2>(poly)
			Print "polyInCreate"
			PrintPoly(tStack)
			Local Stastack:=FullPartition(tStack)
			Local fixtureCreated:=False
			If Stastack=Null
				Return Null
			End
			
			Local numFixtures:=Stastack.Length
			Print "numFixt in createPolyBody: "+numFixtures
			If numFixtures=0
				#If __DEBUG__
					Print "e1: no Valid poly//fixtures for CreatePolyBody returning Null"
					PrintPoly(tStack)
					Print "-------"
				#End
				Return Null
			End
	
			Local bd:b2BodyDef
			bd.type = type
			bd.position=initialPosition
			bd.angle = initialAngle
			Local body:=Self.CreateBody(name,bd)
			
			
			Local fd:=New b2FixtureDef[numFixtures]
			For Local i:=0 Until numFixtures
				fd[i].friction = friction
				fd[i].restitution = restitution
				fd[i].density = density
				Local pshape:=New b2PolygonShape()
				Local vs:=Stastack[i].ToArray()
				'Print "vsl:"+vs.Length
				pshape.Set(vs.Data, vs.Length)
				fd[i].shape = pshape
				
				Self.CreateFixture(name+"Fixture_"+i,body,fd[i])
				fixtureCreated=True
			Next
			
			If fixtureCreated=False
				Self.DestroyBodyClean(body)
				#If __DEBUG__
					Print "e2: no Valid poly/fixtures for CreatePolyBody returning Null"
				#End
				Return Null
			End
			
			Local minHeight:=poly[0].y
			Local maxHeight:=poly[0].y
			
			For Local i:=0 Until poly.Length
				If poly[i].y>maxHeight Then maxHeight=poly[i].y
				If poly[i].y<minHeight Then minHeight=poly[i].y
			Next
			
			Local height:=maxHeight-minHeight
			
			If image<>Null
				If imageHeigth=0.0 Then imageHeigth=height
				image.Handle=imageHandle
				'ici add Image
			End
			
			Return body
		
	End
	
	
	'
	'
	'
	'
	' DESTRUCTION
	'
	'
	'
	
	
	Method DestroyBodyNotClean:Bool(body:b2Body)
		'trouver les joints associés et les virer du stack de b2Manager
		If body<>Null
			Local jointEdge:b2JointEdge Ptr=body.GetJointList()
			
			Local getOut:=False
			Repeat
				If jointEdge<>Null
					If jointEdge->joint<>Null
						Self.jointInfos.Remove(Self.GetJointInfo(jointEdge->joint))'on peut enlever pendant l'iteration car on destroy pas les joints
						jointEdge=jointEdge->nextt
					Else
						getOut=True 'ceci ne devrait pas arriver..
					End
				Else
					getOut=True
				End
			Until getOut=True
			
			'trouver les fixtures et les virer du stack b2Manager
			
			Local fixtureInfoStack:=New Stack<b2FixtureInfo>
			Local currentFixture:b2Fixture=body.GetFixtureList()
			
			getOut=False
			Repeat
				If currentFixture<>Null
					Self.fixtureInfos.Remove(Self.GetFixtureInfo(currentFixture))
					currentFixture=currentFixture.GetNext()
				Else
					getOut=True
				End
			Until getOut=True
			
			'virer les bodies et leurs imges
			
			Local bi:=Self.GetBodyInfo(body)
			bi.deleted=True 'flag comme effacé mais faut cleaner avec Self.CleanBodyInfos()
			Self.bodyImageMap.Remove(bi.index)
			
			Self.world.DestroyBody(body) 'ce qui vire les fixtures et joints associés dans b2world
			'Self.CleanBodyInfos()
			Self.SortRenderOrderToBodyDrawStack()
			
			Return True
		Else
			
			#If __DEBUG__
				Print "body is Null for destroyBody()"
			#End
			Return False
		End
		
	End
	
	Method DestroyBodyClean(body:b2Body)
		DestroyBodyNotClean(body)
		CleanBodyInfos()
	End
	
	Method CleanBodyInfos()
		
		Local count:=0
		For Local bi:=Eachin bodyInfos
			If bi.deleted=False Then count+=1
		Next
		Local newBodyInfos:=New b2BodyImageInfo[count]
		Local newBodyImageMap:=New IntMap<Image>
		count=0
		For Local i:=0 Until bodyInfos.Length
			If bodyInfos[i].deleted=False
				newBodyInfos[count]=bodyInfos[i]
				newBodyInfos[count].index=count
				If bodyImageMap.Contains(i) Then newBodyImageMap[count]=bodyImageMap[i]
				count+=1
			End
		Next
		bodyInfos=newBodyInfos
		bodyImageMap=newBodyImageMap
		'est-ce qu'il ne faut pas cleaner le UserData et Images (resource), je pense que non, le GC devrait s'en occuper vu que c'est un objet mx2
	End
	
	'
	'
	'
	'
	' CUTTINGS
	'
	'
	'
	'
	
	Method CutBody(body:b2Body,cutKnife:Stack<b2Vec2>,knifeIsInWorldCoord:Bool=True)
		
		Local knife:=New Stack<b2Vec2>
		If knifeIsInWorldCoord
			For Local pt:=Eachin cutKnife
				knife.Add(body.GetLocalPoint(pt))
			Next
		Else
			knife=cutKnife
		End
		Print "theWknife:"
		PrintPoly(knife)
		Local polyStack:=New Stack<Stack<b2Vec2>>
		Local currentFixt:=body.GetFixtureList()
		While currentFixt<>Null
			
			Local cShape:=currentFixt.GetShape()
			Local poly:=New Stack<b2Vec2>
			If cShape<>Null And cShape.m_type=2
				Local ps:=Cast <b2PolygonShape>(currentFixt.GetShape())
				For Local i:=0 Until ps.GetVertexCount()
					poly.Add(ps.GetVertex(i))
				Next
			End
			polyStack.Add(poly)
			currentFixt=currentFixt.GetNext()
		Wend
		
		Print "lvlone Cutting polys:"
		PrintPolyStack(polyStack)
		
		Local cutPolyStack:=New Stack<Stack<Stack<b2Vec2>>>
		cutPolyStack.Add(New Stack<Stack<b2Vec2>>)
		cutPolyStack.Add(New Stack<Stack<b2Vec2>>)
		
		For Local poly:=Eachin polyStack
			'Print "precut-poly"
			'PrintPoly(poly)
			'Print "--knif-"
			'PrintPoly(knife)
			'Print "--"
			Local cutSided:=PolyCutSided(poly,knife)
			'Print cutSided.Length
			'Print cutSided[0].Length
			'PrintPolyStack(cutSided[0])
			'Print cutSided[1].Length
			'PrintPolyStack(cutSided[1])
			'Print "postcut"
			cutPolyStack[0].AddAll(cutSided[0])
			cutPolyStack[1].AddAll(cutSided[1])
		Next
		Local bodyName:=body.GetName()
		'Print "anglin: "+body.GetAngle()
		'Print "CuttBody: "+bodyName+" l: "+cutPolyStack.Length
		Print "Creating new BodiesWith------------------------------"
		PrintPolyStack(cutPolyStack[0])
		Print "stckB-----"
		PrintPolyStack(cutPolyStack[1])
		Print "fin stcks"
				
		For Local npoly:=Eachin cutPolyStack[0]
			'MakeCCW(npoly)	
			'Local tconvPolys:=ConvexPartitionOpt(npoly)
			Local tBody:=Self.CreatePolyBody(bodyName+"_cut",npoly.ToArray(),body.GetPosition(),body.GetAngle())'faut passer les vitesses aussi!
			'tBody.copyParamsFrom(body)
		Next
		For Local npoly:=Eachin cutPolyStack[1]
			Local tBody:=Self.CreatePolyBody(bodyName+"_cut",npoly.ToArray(),body.GetPosition(),body.GetAngle())'faut passer les vitesses aussi!
		'	tBody.copyParamsFrom(body)
		Next
		Self.DestroyBodyClean(body)
	End
	
	
	
	Method SetBodyImage (bi:b2BodyImageInfo,img:Image , worldHeight:Float=1.0,renderOrder:Int=1,opacity:Float=1.0,flip:Int=1,locPos:Vec2f=New Vec2f(0,0),locAngle:Float=0.0,aspectScale:Float=1.0)

		If img<>Null
			bi.image=img
			bi.imageRubeName="manually_Added_from_mojo.graphics.Image"
			bi.imageFileName="manually_Added_from_mojo.graphics.Image"
			bi.imageLocalPosition=locPos
			bi.imageLocalAngle=locAngle
			bi.imageAspectScale=aspectScale
			bi.imageWorldHeight=worldHeight
			Local fact:=bi.imageWorldHeight/bi.image.Height
			bi.imageRenderScale=New Vec2f(fact*bi.imageAspectScale,fact)
			bi.imageRenderOrder=renderOrder
			bi.imageOpacity=opacity
			bi.imageFlip=flip
			Self.bodyImageMap[bi.index]=img
			Self.SortRenderOrderToBodyDrawStack()
		Else
			#If __DEBUG__
					Print "Image is Null for b2Manager.AddImage()"
			#End
		End
			
	End
	
	Method SetBodyImage (body:b2Body,img:Image , worldHeight:Float=1.0,renderOrder:Int=1,opacity:Float=1.0,flip:Int=1,locPos:Vec2f=New Vec2f(0,0),locAngle:Float=0.0,aspectScale:Float=1.0)

		Local bi:=Self.GetBodyInfo(body)
		If bi<>Null
			Self.SetBodyImage (bi,img , worldHeight,renderOrder,opacity,flip,locPos,locAngle,aspectScale)
		Else
			#If __DEBUG__
				Print "body is not defined in b2Manager for b2Manager.AddImageToBody()"
			#End	
		End
			
	End
	
	Method SetBodyImage (name:String,img:Image , worldHeight:Float=1.0,renderOrder:Int=1,opacity:Float=1.0,flip:Int=1,locPos:Vec2f=New Vec2f(0,0),locAngle:Float=0.0,aspectScale:Float=1.0)

		Local bi:=Self.GetBodyInfo(Self.GetBody(name))
		If bi<>Null
			Self.SetBodyImage (bi,img , worldHeight,renderOrder,opacity,flip,locPos,locAngle,aspectScale)
		Else
			#If __DEBUG__
				Print "no body found with name or body is not defined in b2Manager for b2Manager.AddImage()"
			#End	
		End
			
	End
	
	'
	'
	'
	' 	Manual Fixture creation
	'
	'
	
	
	Method CreateFixture:b2Fixture(name:String="",b:b2Body,fd:b2FixtureDef)
		
		If name="" Then name=Self.GetBodyName(b)+"_Fixture"
		
		Local fi:=New b2FixtureInfo()
		fi.fixture=b.CreateFixture(fd)
		fi.fixtureName=name
		fi.fixtureUserData=New StringMap<Variant>
		fi.fixtureUserData["b2ManagerFixtureInfo"]=fi
		fi.fixture.SetUserData(Cast<Void Ptr>(fi.fixtureUserData))
		
		Self.fixtureInfos.Add(fi)
		
		Return fi.fixture
		
	End
	
	
	'----------------------------
	'
	' Manual Shapes
	'
	'--------------------------
	
	
	Method CreatePolygonShape:b2PolygonShape(vertices:b2Vec2[])
		
		If vertices.Length<3
			#If __DEBUG__
				Print "ERROR less than 3 vertices for CreatePolygonShape . "
			#End
			Return Null
		End
		If vertices.Length>8
			#If __DEBUG__
				Print "ERROR: too much vertices(v>8) for b2Manager.CreatePolygonShape(verts[]). Use CreatePolygonShapes(verts[]) instead."
			#End
			Return Null
		End
		 
		Local pshape:=New b2PolygonShape()
		pshape.Set(vertices.Data, vertices.Length)
		
		If Not pshape.Validate()
			#If __DEBUG__
				Print "ERROR vertices create a concave poly for b2Manager.CreatePolygonShape(verts[]) . Use CreatePolygonShapes(verts[]) instead."
			#End
			Return Null
		End
		
		Return pshape
		
	End
	
	Method CreatePolygonShapes:b2PolygonShape[](vertices:b2Vec2[])
		Return Null	
	End
	
	
	
	
	
	'
	'------------------------
	'
	' Manual Joints creation
	'
	'-------------------------
	'
		
	
	Method CreateRevoluteJoint:b2Joint(name:String="",bodyA:b2Body,bodyB:b2Body,localAnchorA:b2Vec2=New b2Vec2(0,0) , localAnchorB:b2Vec2=New b2Vec2(0,0) , collideConnected:Bool=False , enableMotor:Bool=False, motorSpeed:Float=0.0 , maxMotorTorque:Float=10.0 , enableLimit:Bool=False , referenceAngle:Float=0.0 , lowerAngle:Float=-0.7 , upperAngle:Float=0.7 )
			

		Local jdRe:b2RevoluteJointDef
		jdRe.bodyA = bodyA
		jdRe.bodyB = bodyB
		jdRe.collideConnected = collideConnected
		jdRe.localAnchorA = localAnchorA
		jdRe.localAnchorB = localAnchorB
		jdRe.referenceAngle = referenceAngle
		jdRe.enableLimit = enableLimit
		jdRe.lowerAngle = lowerAngle
		jdRe.upperAngle = upperAngle
		jdRe.enableMotor = enableMotor
		jdRe.motorSpeed = motorSpeed
		jdRe.maxMotorTorque = maxMotorTorque
		
		Return Self.CreateRevoluteJoint(name,jdRe)
			
	End
	

	
	Method CreateRevoluteJoint:b2Joint(name:String="",revoluteJointDef:b2RevoluteJointDef)
		
		If name="" Then name=revoluteJointDef.bodyA.GetName()+"-"+revoluteJointDef.bodyB.GetName()+"_revoluteJoint"

		Local ji:=New b2JointInfo()
		ji.theb2Joint=world.CreateJoint( revoluteJointDef )
		ji.jointName=name
		ji.jointType="revolute"
		ji.jointUserData=New StringMap<Variant>
		ji.jointUserData["b2ManagerJointInfo"]=ji
		ji.theb2Joint.SetUserData(Cast<Void Ptr>(ji.jointUserData))
		
		Self.jointInfos.Add(ji)
		
		Return ji.theb2Joint
			
	End
	
	
	
	'
	'
	' Methodes 'Classiques'
	'
	'
	
	Method New (jsonPath:String,pScale:Float=15,yAxisInvert:Bool=True,offset:b2Vec2=New b2Vec2(0,0))
		
		physScale=pScale
		yAxisInversion=yAxisInvert
		
		b2dJsons[0]=New mx2b2dJson.b2dJson()
		
		
		world=Loadb2dJsonWithb2dJsonRef(b2dJsons[0] , jsonPath, Null, offset) 'offset TODO!
		
		debugDrawer=New b2DebugDraw(physScale,yAxisInversion)
		
		world.SetDebugDraw( debugDrawer  )
		debugDrawer.SetFlags( e_shapeBit|e_jointBit )
		
		bodyInfos=Createb2BodyImageInfoArray(world,jsonPath )
		bodyImageMap=Createb2BodyImageMap(bodyInfos)
		
		fixtureInfos=Createb2FixtureInfoStack(world,jsonPath)
		
		jointInfos=Createb2JointInfoStack(world,jsonPath)',b2dJsons[0])
		
		SortRenderOrderToBodyDrawStack()
		
	End
	
	Method AddJson(jsonPath:String,offset:b2Vec2=New b2Vec2(0,0))
		
		Local firstWSize:=world.GetBodyCount()
		
		Local l:=b2dJsons.Length
		b2dJsons=b2dJsons.Resize(l+1)
		b2dJsons[l]=New mx2b2dJson.b2dJson()
		
		Loadb2dJsonWithb2dJsonRef(b2dJsons[l] , jsonPath , world , offset)
		
		Local tempBodyInfos:=Createb2BodyImageInfoArray(world,jsonPath,firstWSize )
		bodyInfos=bodyInfos.Resize(world.GetBodyCount())
		For Local n:=Eachin tempBodyInfos
			bodyInfos[n.index]=n
		Next
		bodyImageMap=Createb2BodyImageMap(bodyInfos)
		
		Local tempFixtureInfos:=Createb2FixtureInfoStack(world,jsonPath)
		If tempFixtureInfos.Length>0
			For Local inf:=Eachin tempFixtureInfos
				fixtureInfos.Add(inf)
			Next
		Endif
		Local tempJointInfos:=Createb2JointInfoStack(world,jsonPath)',b2dJsons[0])
		If tempJointInfos.Length>0
			For Local inf:=Eachin tempJointInfos
				jointInfos.Add(inf)
			Next
		Endif
		
		SortRenderOrderToBodyDrawStack()
		
		
	End
	
	Method StepWorld()
		world.Stepp(timeStep, velocityIterations, positionIterations)
	End
	
	Method DrawDebug(canvas:Canvas)
		Local col:=canvas.Color
		debugDrawer.SetCanvas(canvas)
		world.DrawDebugData()
		canvas.Color=col
	End
	
	Method DrawBodies(canvas:Canvas)
		
		Local sign:=-1
		If yAxisInversion=False Then sign=1
		#rem
		For Local bodyImageNode:=Eachin bodyImageMap
			
			Local location:=b2Vec2ToVec2f(bodyInfos[bodyImageNode.Key].imageWorldPosition)*(New Vec2f(physScale,sign*physScale)) 'sign for y axis inversion RUBE using standart coordinates system
			Local rotation:=-sign*bodyInfos[bodyImageNode.Key].imageWorldAngle' sign for y axis inversion RUBE using standart coordinates system -sign for trig vs canvas rotation direction????????
			Local scale:=bodyInfos[bodyImageNode.Key].imageRenderScale*New Vec2f(physScale,physScale) 'No yaxis inversion here! because it's an image in left handed coord anyway!
			
			canvas.DrawImage (bodyInfos[bodyImageNode.Key].image , location , rotation , scale)
			
		Next
		#end
		'#rem
		For Local bodyInf:=Eachin sortedBodyImageInfos
			
			Local location:=b2Vec2ToVec2f(bodyInf.imageWorldPosition)*(New Vec2f(physScale,sign*physScale)) 'sign for y axis inversion RUBE using standart coordinates system
			Local rotation:=-sign*bodyInf.imageWorldAngle' sign for y axis inversion RUBE using standart coordinates system -sign for trig vs canvas rotation direction????????
			Local scale:=bodyInf.imageRenderScale*New Vec2f(bodyInf.imageFlip*physScale,physScale) 'No yaxis inversion here! because it's an image in left handed coord anyway!
		
			canvas.DrawImage (bodyInf.image , location , rotation , scale)
			
		Next
		'#end
				'For Local bImgInfo:=Eachin sortedBodyDrawStack
				'	Print bImgInfo.imageRenderOrder
				'Next
		
	End
	
	Method SortRenderOrderToBodyDrawStack()
		
		'copy To an array
		sortedBodyImageInfos=New Stack<b2BodyImageInfo>

		For Local bodyImageNode:=Eachin bodyImageMap
			sortedBodyImageInfos.Add(bodyInfos[bodyImageNode.Key])
		Next
		
		sortedBodyImageInfos.Sort(Lambda:Int(a:b2BodyImageInfo,b:b2BodyImageInfo) 
									Return  a.imageRenderOrder - b.imageRenderOrder
								End )
	End

	
	#rem	
	Method UpdateInfos()
		'usefull?
	End
	#end
	
	Method GetBodies:b2Body[](name:String)
		
		Local retArray:b2Body[]
		Local bodyStack:=New Stack<b2Body>

		For Local i:=0 Until bodyInfos.Length
			If bodyInfos[i].bodyName=name
				bodyStack.Add(bodyInfos[i].body)
			End
		End
		If bodyStack.Length>0
			retArray=bodyStack.ToArray()
		Else
		#If __DEBUG__
			Print "No body with name "+name+" !!!!!!!!!!!!!!!"
		#End
			Return Null
		End
		Return retArray
		
	End
	
	Method GetBody:b2Body(name:String)
		Local i:=0
		While i<bodyInfos.Length
			If bodyInfos[i].bodyName=name Then Return bodyInfos[i].body
			i+=1
		Wend
		
		#If __DEBUG__
			Print "No body with name "+name+" !!!!!!!!!!!!!!!"
		#End
		
		Return Null

	End
	
	Method GetBodiesInfo:b2BodyImageInfo[](name:String)
		
		Local retArray:b2BodyImageInfo[]
		Local bodyStack:=New Stack<b2BodyImageInfo>

		For Local i:=0 Until bodyInfos.Length
			If bodyInfos[i].bodyName=name
				bodyStack.Add(bodyInfos[i])
			End
		End
		If bodyStack.Length>0
			retArray=bodyStack.ToArray()
		Else
		#If __DEBUG__
			Print "No body with name "+name+" !!!!!!!!!!!!!!!"
		#End
			Return Null
		End
		Return retArray
		
	End
	
	Method GetBodyInfo:b2BodyImageInfo(name:String)
		Local i:=0
		While i<bodyInfos.Length
			If bodyInfos[i].bodyName=name Then Return bodyInfos[i]
			i+=1
		Wend
		
		#If __DEBUG__
			Print "No body with name "+name+" !!!!!!!!!!!!!!!"
		#End
		
		Return Null

	End
	
'----------------------------BodiesData

	Method GetBodiesUserData:StringMap<Variant>[](name:String)
		
		Local retArray:StringMap<Variant>[]
		Local bodyStack:=New Stack<StringMap<Variant>>

		For Local i:=0 Until bodyInfos.Length
			If bodyInfos[i].bodyName=name
				bodyStack.Add(bodyInfos[i].bodyUserData)
			End
		End
		If bodyStack.Length>0
			retArray=bodyStack.ToArray()
		Else
		#If __DEBUG__
			Print "No body with name "+name+" !!!!!!!!!!!!!!!"
		#End
			Return Null
		End
		Return retArray
		
	End

	Method GetBodiesUserData:Stack<Variant>(name:String,dataName:String)
	
		Local variantMapArray:=GetBodiesUserData(name)
		Local retStack:= New Stack<Variant>
		Local retArray:Variant[]
	
		For Local map:=Eachin variantMapArray
			If map.Contains(dataName)
	
				retStack.Add(map[dataName])
	
			End
		End
	
		If retStack.Length=0
	
			#If __DEBUG__
				Print "No body-data "+dataName+" for body with name "+name+" !!!!!!!!!!!!!!!"
			#End
	
			Return Null
		End
		Return retStack
	
	End
	
	Method GetBodiesUserDataToS:Stack<String>(name:String,dataName:String)
	
		Local variantMapArray:=GetBodiesUserData(name)
		Local retStack:= New Stack<String>
		For Local map:=Eachin variantMapArray
			If map.Contains(dataName)
				If map[dataName].Type.Name="String"
					retStack.Add(Cast<String>(map[dataName]))
				Else
					#If __DEBUG__
						Print "body-data "+dataName+" for body "+name+" Is Not a String !!!!!!!!!!!!!!!"
					#End
				End	
			End
		Next
		If retStack.Length=0
			#If __DEBUG__
				Print "No with body-data "+dataName+" for body with name "+name+" !!!!!!!!!!!!!!!"
			#End	
		End
		Return retStack
	End
	
	Method GetBodiesUserDataToB:Stack<Bool>(name:String,dataName:String)
	
		Local variantMapArray:=GetBodiesUserData(name)
		Local retStack:= New Stack<Bool>
		For Local map:=Eachin variantMapArray
			If map.Contains(dataName)
				If map[dataName].Type.Name="Bool"
					Local myVariant:Variant=map[dataName]
					Local myBool:=Cast<Bool>(myVariant)
					retStack.Add(myBool)
				Else
					#If __DEBUG__
						Print "body-data "+dataName+" for body "+name+" Is Not a Bool !!!!!!!!!!!!!!!"
					#End
				End
			Else
				retStack.Add(False)	
			End
		Next
		If retStack.Length=0
			#If __DEBUG__
				Print "No with body-data "+dataName+" for body with name "+name+" !!!!!!!!!!!!!!!"
			#End
		End
		Return retStack
	
	End
	
	Method GetBodiesUserDataToI:Stack<Int>(name:String,dataName:String)
	
		Local variantMapArray:=GetBodiesUserData(name)
		Local retStack:= New Stack<Int>
		For Local map:=Eachin variantMapArray
			If map.Contains(dataName)
				If map[dataName].Type.Name="Int"
					retStack.Add(Cast<Int>(map[dataName]))
				Else
					#If __DEBUG__
						Print "body-data "+dataName+" for body "+name+" Is Not an Int !!!!!!!!!!!!!!!"
					#End
				End	
			End
		End
		If retStack.Length=0
			#If __DEBUG__
				Print "No with body-data "+dataName+" for body with name "+name+" !!!!!!!!!!!!!!!"
			#End	
		End
		Return retStack
	End
	
	Method GetBodiesUserDataToF:Stack<Float>(name:String,dataName:String)
	
		Local variantMapArray:=GetBodiesUserData(name)
		Local retStack:= New Stack<Float>
		For Local map:=Eachin variantMapArray
			If map.Contains(dataName)
				If map[dataName].Type.Name="Float"
					retStack.Add(Cast<Float>(map[dataName]))
				Else
					#If __DEBUG__
						Print "body-data "+dataName+" for body "+name+" Is Not a Float !!!!!!!!!!!!!!!"
					#End
				End	
			End
		End
		If retStack.Length=0
			#If __DEBUG__
				Print "No with body-data "+dataName+" for body with name "+name+" !!!!!!!!!!!!!!!"
			#End	
		End
		Return retStack
	End
	
	Method GetBodiesUserDataToN:Stack<Float>(name:String,dataName:String)
	
		Local variantMapArray:=GetBodiesUserData(name)
		Local retStack:= New Stack<Float>
		For Local map:=Eachin variantMapArray
			If map.Contains(dataName)
				If map[dataName].Type.Name="Float"
					retStack.Add(Cast<Float>(map[dataName]))
				Elseif map[dataName].Type.Name="Int"
					retStack.Add(Float(Cast<Int>(map[dataName])))
				Else
					#If __DEBUG__
						Print "body-data "+dataName+" for body "+name+" Is Not a Float or Int !!!!!!!!!!!!!!!"
					#End
				End	
			End
		End
		If retStack.Length=0
			#If __DEBUG__
				Print "No body-data "+dataName+" for body with name "+name+" !!!!!!!!!!!!!!!"
			#End	
		End
		Return retStack
	End



	'--------------------Fixtures
	
	Method GetFixtures:b2Fixture[](name:String)
	
		Local retArray:b2Fixture[]
		Local fixtureStack:=New Stack<b2Fixture>
		For Local fixt:=Eachin fixtureInfos
			If fixt.fixtureName=name
				fixtureStack.Add(fixt.fixture)
			End
		End
		If fixtureStack.Length>0
			retArray=fixtureStack.ToArray()
		Else
		#If __DEBUG__
			Print "No fixture with name "+name+" !!!!!!!!!!!!!!!"
		#End
			Return Null
		End
		Return retArray
	
	End
	
	Method GetFixture:b2Fixture(name:String)

		For Local fixt:=Eachin fixtureInfos
			If fixt.fixtureName=name Then Return fixt.fixture
		Next
	
		#If __DEBUG__
			Print "No fixture with name "+name+" !!!!!!!!!!!!!!!"
		#End
	
		Return Null
	
	End
	
	Method GetJointsInfo:b2JointInfo[](name:String)
	
		Local retArray:b2JointInfo[]
		Local jointStack:=New Stack<b2JointInfo>
		For Local jo:=Eachin jointInfos
			If jo.jointName=name
				jointStack.Add(jo)
				'Print "Added"
			End
		End
		If jointStack.Length>0
			retArray=jointStack.ToArray()
		Else
		#If __DEBUG__
			Print "No joint with name "+name+" !!!!!!!!!!!!!!!"
		#End
			Return Null
		End
		Return retArray
	
	End
	
	Method GetJoints:b2Joint[](name:String)
		
		Local infArr:=GetJointsInfo(name)
		If infArr<>Null
			If infArr.Length>0
				Local ret:=New b2Joint[infArr.Length]
				For Local i:=0 Until infArr.Length
					ret[i]=infArr[i].theb2Joint
				Next
				Return ret
			End
		End
		
		Return Null
		
	End
	
	Method GetJointInfo:b2JointInfo(name:String)

		For Local jo:=Eachin jointInfos
			If jo.jointName=name
				Return jo
			End
			
		Next
	
		#If __DEBUG__
			Print "No joint with name "+name+" !!!!!!!!!!!!!!!"
		#End
		
		Return Null
	
	End
	
	Method GetJointInfo:b2JointInfo(joint:b2Joint)

		For Local jo:=Eachin jointInfos
			If jo.theb2Joint=joint
				Return jo
			End
			
		Next
	
		#If __DEBUG__
			Print "No jointInfo found with given joint  for GetJointInfo !!!!!!!!!!!!!!!"
		#End
		
		Return Null
	
	End
	
	Method GetJoint:b2Joint(name:String)
		
		Local info:=GetJointInfo(name)
		
		Return info?.theb2Joint
		
	End
	
	
	

	
	Method ToPhysics:b2Vec2(Location:Vec2f)

		Return debugDrawer.ToPhysicsLocation(Location)
		
	End
	
	Method FromPhysics:Vec2f(physLocation:b2Vec2)
		
		Return debugDrawer.FromPhysicsLocation(physLocation)
		
	End
	
	Method Save(path:String,humanReadable:Bool=False)
		'getting the type of vec2f for variant check
		Local someVector:=New Vec2f (0,0)
		Local variantVector:Variant=someVector
		
		
		Local json:b2dJson=New b2dJson(humanReadable)
		
		For Local i:=0 Until bodyInfos.Length
			If bodyInfos[i].bodyName<>"" Then json.setBodyName(bodyInfos[i].body, bodyInfos[i].bodyName)
			If bodyInfos[i].bodyUserData<>Null
				For Local node:=Eachin bodyInfos[i].bodyUserData
					If node.Key<>"b2ManagerBodyInfo"
						If node.Value.Type.Name="Int"
							json.addCustomInt(bodyInfos[i].body,node.Key,Cast <Int>(node.Value))
						Elseif node.Value.Type.Name="Float"
							json.addCustomFloat(bodyInfos[i].body,node.Key,Cast <Float>(node.Value))
						Elseif node.Value.Type.Name="Bool"
							json.addCustomBool(bodyInfos[i].body,node.Key,Cast <Bool>(node.Value))
						Elseif node.Value.Type.Name="String"
							json.addCustomString(bodyInfos[i].body,node.Key,Cast <String>(node.Value))
						Elseif node.Value.Type=variantVector.Type
							json.addCustomVector(bodyInfos[i].body,node.Key,Cast <Vec2f>(node.Value))
						Else
							#If __DEBUG__
								Print "The custom value "+node.Key+" of type: "+node.Value.Type.Name+" has an unsupported type for Saving (and loading)!!!!!"
							#End
						End	
					End
				Next
			End
		Next
		For Local fixInfo:=Eachin fixtureInfos
			json.setFixtureName(fixInfo.fixture,fixInfo.fixtureName)
			If fixInfo.fixtureUserData<>Null
				For Local node:=Eachin fixInfo.fixtureUserData
					If node.Key<>"b2ManagerFixtureInfo"
						If node.Value.Type.Name="Int"
							json.addCustomInt(fixInfo.fixture,node.Key,Cast <Int>(node.Value))
						Elseif node.Value.Type.Name="Float"
							json.addCustomFloat(fixInfo.fixture,node.Key,Cast <Float>(node.Value))
						Elseif node.Value.Type.Name="Bool"
							json.addCustomBool(fixInfo.fixture,node.Key,Cast <Bool>(node.Value))
						Elseif node.Value.Type.Name="String"
							json.addCustomString(fixInfo.fixture,node.Key,Cast <String>(node.Value))
						Elseif node.Value.Type=variantVector.Type
							json.addCustomVector(fixInfo.fixture,node.Key,Cast <Vec2f>(node.Value))
						Else
							#If __DEBUG__
								Print "The custom value "+node.Key+" of type: "+node.Value.Type.Name+" has an unsupported type for Saving (and loading)!!!!!"
							#End
						End	
					End
				Next
			End
		Next
		
		For Local joInfo:=Eachin jointInfos
			json.setJointName(joInfo.theb2Joint,joInfo.jointName)
			If joInfo.jointUserData<>Null
				For Local node:=Eachin joInfo.jointUserData
					If node.Key<>"b2ManagerJointInfo"
						If node.Value.Type.Name="Int"
							json.addCustomInt(joInfo.theb2Joint,node.Key,Cast <Int>(node.Value))
						Elseif node.Value.Type.Name="Float"
							json.addCustomFloat(joInfo.theb2Joint,node.Key,Cast <Float>(node.Value))
						Elseif node.Value.Type.Name="Bool"
							json.addCustomBool(joInfo.theb2Joint,node.Key,Cast <Bool>(node.Value))
						Elseif node.Value.Type.Name="String"
							json.addCustomString(joInfo.theb2Joint,node.Key,Cast <String>(node.Value))
						Elseif node.Value.Type=variantVector.Type
							json.addCustomVector(joInfo.theb2Joint,node.Key,Cast <Vec2f>(node.Value))
						Else
							#If __DEBUG__
								Print "The custom value "+node.Key+" of type: "+node.Value.Type.Name+" has an unsupported type for Saving (and loading)!!!!!"
							#End
						End	
					End
				Next
			End
		Next		
		
		
		'getting the size of the string and creating it with iforce2d-b2dJson
		Local strSize:=Getb2dJsonStringSize(world,json)
		Local jsonCStr:=New char_t[strSize+1]
		b2dJsonWriteToString_ext(jsonCStr.Data,world,json)
		
		'converting the b2djson to string then to mx2JsonObject
		Local tempJsonFullString:=String.FromCString(jsonCStr.Data)
		Local mainJsonObj:=JsonObject.Parse(tempJsonFullString)

		'create an object for images info
		Local imageJsonArray:=New JsonArray
		
		
		Local i:=0
		For Local bodyImageNode:=Eachin bodyImageMap
			
			Local ptiJsonObj:=New JsonObject
			
			ptiJsonObj["name"]=New JsonString(bodyInfos[bodyImageNode.Key].imageRubeName)':String
			
			ptiJsonObj["file"]=New JsonString(bodyInfos[bodyImageNode.Key].imageFileName.Replace("asset::",""))':String
			
			ptiJsonObj["angle"]=New JsonNumber(bodyInfos[bodyImageNode.Key].imageLocalAngle)':Float
			ptiJsonObj["aspectScale"]=New JsonNumber(bodyInfos[bodyImageNode.Key].imageAspectScale)':Float
			ptiJsonObj["scale"]=New JsonNumber(bodyInfos[bodyImageNode.Key].imageWorldHeight)':Float
			ptiJsonObj["renderOrder"]=New JsonNumber(bodyInfos[bodyImageNode.Key].imageRenderOrder)':Int

			ptiJsonObj["body"]=New JsonNumber(bodyInfos.Length-1-bodyInfos[bodyImageNode.Key].index)':Int Body reference has to be processed backwards compared to .json order (reliable?)
			ptiJsonObj["opacity"]=New JsonNumber(bodyInfos[bodyImageNode.Key].imageOpacity)

			If bodyInfos[bodyImageNode.Key].imageFlip=-1
				ptiJsonObj["flip"]=New JsonBool(True)
			End
			
			Local miniJsonObj:=New JsonObject
			miniJsonObj["x"]=New JsonNumber(bodyInfos[bodyImageNode.Key].imageLocalPosition.x)
			miniJsonObj["y"]=New JsonNumber(bodyInfos[bodyImageNode.Key].imageLocalPosition.y)
			ptiJsonObj["center"]=miniJsonObj
			
			imageJsonArray.Add(ptiJsonObj)
				
			i+=1
		Next
		
		'Print "-------------Print array"
		'Print imageJsonArray.ToJson()
		
		mainJsonObj["image"]=imageJsonArray
		
		SaveString(mainJsonObj.ToJson(),path,True)
		json.Destroy()
		
	End
	
	'
	'
	'
	' USER DATAS
	'
	'
	'
	

	
'---------------------------- Body user data singleton by Name
	
	Method GetBodyUserDataToM:StringMap<Variant>(name:String)
		Local i:=0
		While i<bodyInfos.Length
			If bodyInfos[i].bodyName=name Then Return bodyInfos[i].bodyUserData
			i+=1
		Wend
		
		#If __DEBUG__
			Print "No body with name "+name+" !!!!!!!!!!!!!!!"
		#End
		
		Return Null

	End
	
	Method GetBodyUserData:Variant(name:String,dataName:String)
		
		Local data:=GetBodyUserDataToM(name)
		
		Local ret:=data[dataName]
		
		#If __DEBUG__
			If ret=False Then Print "No body-data "+dataName+" for body "+name+" !!!!!!!!!!!!!!!"
		#End
		
		Return ret

	End
	Method GetBodyUserDataToS:String(name:String,dataName:String)
		
		Local dataVariant:=GetBodyUserDataToM(name)[dataName]
		
		If dataVariant.Type.Name="String"
			
			Return Cast<String>(dataVariant)
			
		Else
		
			#If __DEBUG__
				Print "body-data "+dataName+" for body "+name+" Is Not a String !!!!!!!!!!!!!!!"
			#End
			
		End
		
		Return Null

	End
	
	Method GetBodyInfo:b2BodyImageInfo(b:b2Body)
	
		Local biiVariant:=GetBodyUserDataToM(b)["b2ManagerBodyInfo"]
		Local bii:=Cast<b2BodyImageInfo>(biiVariant)
	
		Return bii
	
	End
	
	Method GetBodyName:String(b:b2Body)
		If b<>Null
			Return GetBodyInfo(b).bodyName
		Else
			Return ""
		End
	End
	
	Method GetFixtureInfo:b2FixtureInfo(f:b2Fixture)
	
		Local fiVariant:=GetFixtureUserDataToM(f)["b2ManagerFixtureInfo"]
		Local fi:=Cast<b2FixtureInfo>(fiVariant)
	
		Return fi
	
	End
	
	Method GetFixtureName:String(f:b2Fixture)
		
		Return GetFixtureInfo(f).fixtureName
		
	End
	
	Method GetFixtureUserDataToM:StringMap<Variant>(fixt:b2Fixture)
		
		
		Local ret:=Cast<StringMap<Variant>>(fixt.GetUserData())
		#If __DEBUG__
			If ret=Null Then Print "Fixture GetUserData returns Null !!!!!!!!!!!!!!!"
		#End
		Return ret

	End
		
	
	Method GetBodyUserDataToB:Bool(name:String,dataName:String)
	
		Local dataVariant:=GetBodyUserDataToM(name)[dataName]
	
		If dataVariant.Type.Name="Bool"
	
			Return Cast<Bool>(dataVariant)
	
		Else
	
			#If __DEBUG__
				If dataVariant=False Then Print "body-data "+dataName+" for body "+name+" Is Not a Bool !!!!!!!!!!!!!!!"
			#End
	
		End
	
		Return Null
	
	End
	
	Method GetBodyUserDataToI:Int(name:String,dataName:String)
	
		Local dataVariant:=GetBodyUserDataToM(name)[dataName]
	
		If dataVariant.Type.Name="Int"
	
			Return Cast<Int>(dataVariant)
	
		Else
	
			#If __DEBUG__
				If dataVariant=False Then Print "body-data "+dataName+" for body "+name+" Is Not an Int !!!!!!!!!!!!!!!"
			#End
	
		End
	
		Return Null
	
	End
	
	Method GetBodyUserDataToF:Float(name:String,dataName:String)
	
		Local dataVariant:=GetBodyUserDataToM(name)[dataName]
	
		If dataVariant.Type.Name="Float"
	
			Return Cast<Float>(dataVariant)
	
		Else
	
			#If __DEBUG__
				If dataVariant=False Then Print "body-data "+dataName+" for body "+name+" Is Not a Float !!!!!!!!!!!!!!!"
			#End
	
		End
	
		Return Null
	
	End
	
	Method GetBodyUserDataToN:Float(name:String,dataName:String)
	
		Local dataVariant:=GetBodyUserDataToM(name)[dataName]
	
		If dataVariant.Type.Name="Float"
	
			Return Cast<Float>(dataVariant)
			
		Elseif dataVariant.Type.Name="Int"
		
				Return Cast<Int>(dataVariant)
				
		Else
	
			#If __DEBUG__
				If dataVariant=False Then Print "body-data "+dataName+" for body "+name+" Is Not a 32 bit Numeric (Int,Float) !!!!!!!!!!!!!!!"
			#End
	
		End
	
		Return Null
	
	End
	

	'---------------getbody user data by body--------------------------------------------------------
	
	Method GetBodyUserDataToM:StringMap<Variant>(body:b2Body)
		
		
		Local ret:=Cast<StringMap<Variant>>(body.GetUserData())
		#If __DEBUG__
			If ret=Null Then Print "Body GetUserData returns Null !!!!!!!!!!!!!!!"
		#End
		Return ret

	End
	
	Method GetBodyUserData:Variant(body:b2Body,dataName:String)
		
		Local data:=GetBodyUserDataToM(body)
		
		
		If data<>Null
			If data.Contains(dataName)
				Return data[dataName]
			Else
				#If __DEBUG__
					Print "body has no data called "+dataName+" !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "body user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Local v:Variant
		Return v

	End
	
	Method GetBodyUserDataToS:String(body:b2Body,dataName:String)
		
		Local data:=GetBodyUserDataToM(body)
		
		
		If data<>Null
			If data.Contains(dataName)
				If data[dataName].Type.Name="String"
					Return Cast<String>(data[dataName])
				Else
					#If __DEBUG__
						Print "body data called "+dataName+" is Not a string !!!!!!!!!!!!!!!"
					#End
				End
			Else
				#If __DEBUG__
					Print "body has no data called "+dataName+" !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "body user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Return Null

	End
	
	Method GetBodyUserDataToB:Bool(body:b2Body,dataName:String)
		
		Local data:=GetBodyUserDataToM(body)
		
		
		If data<>Null
			If data.Contains(dataName)
				If data[dataName].Type.Name="Bool"
					Local v:=data[dataName]
					Return Cast<Bool>(v)
				Else
					#If __DEBUG__
						Print "body data called "+dataName+" is Not a Bool !!!!!!!!!!!!!!!"
					#End
				End
			Else
				#If __DEBUG__
					Print "body has no data called "+dataName+" !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "body user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Return Null

	End
	
	Method GetBodyUserDataToF:Float(body:b2Body,dataName:String)
		
		Local data:=GetBodyUserDataToM(body)
		
		
		If data<>Null
			If data.Contains(dataName)
				If data[dataName].Type.Name="Float"
					Return Cast<Float>(data[dataName])
				Else
					#If __DEBUG__
						Print "body data called "+dataName+" is Not a Float !!!!!!!!!!!!!!!"
					#End
				End
			Else
				#If __DEBUG__
					Print "body has no data called "+dataName+" !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "body user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Return Null

	End
	Method GetBodyUserDataToI:Int(body:b2Body,dataName:String)
		
		Local data:=GetBodyUserDataToM(body)
		
		
		If data<>Null
			If data.Contains(dataName)
				If data[dataName].Type.Name="Int"
					Return Cast<Int>(data[dataName])
				Else
					#If __DEBUG__
						Print "body data called "+dataName+" is Not an int !!!!!!!!!!!!!!!"
					#End
				End
			Else
				#If __DEBUG__
					Print "body has no data called "+dataName+" !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "body user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Return Null

	End
	
	Method GetBodyUserDataToN:Float(body:b2Body,dataName:String)
		
		Local data:=GetBodyUserDataToM(body)
		
		If data<>Null
			If data.Contains(dataName)
				If data[dataName].Type.Name="Int"
					Local i:= Cast<Int>(data[dataName])
					Return i
				Elseif data[dataName].Type.Name="Float"
					Return Cast<Float>(data[dataName])
				Else
					#If __DEBUG__
						Print "body data called "+dataName+" is Not a Number (int or float) !!!!!!!!!!!!!!!"
					#End
				End
			Else
				#If __DEBUG__
					Print "body has no data called "+dataName+" !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "body user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Return Null

	End
		
	
	'
	'
	' RESOURCE MEM FREE 
	'
	'
	'
	Method OnDiscard() Override
		If b2dJsonsCount>0
			For Local i:=0 Until b2dJsonsCount
				b2dJsons[i].Destroy()
			Next
		Endif
		world.Destroy()
		
	End
	
	Method OnFinalize() Override
		'Print "finalaïzing b2Manager"
	End
	
End


