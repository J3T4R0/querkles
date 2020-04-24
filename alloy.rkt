module ActiveBadgeSystem

-----------signature declaration ------------
sig Level {}

sig Badge {
	accessLevel: set Level,
	signal: option Base,
	--this implies that signal will be received by no more than one base history: set Room

	history: set Room
}

sig Base {}

sig Room {
	level: Level,
	bases: set Base
}

sig Entrance extends Room { }
{ 	--all Badges have access to the building
    all abs: ABS, b: Badge, e: entrance | e.RoomSlevel in b.accessLevel
}

sig ABS {
	registered: set Badge,
	rooms: set Room,
	entrance: set Entrance,
	location: registered ->! Room,
	prev: option ABS
}

--------- invariants -------------

fact nontrivial {
	some Room
	some Entrance
	some Badge 
	some ABS
}

fact roomHasBasesAndControlLevel {
	all r: Room | some r.bases && one r.level
}

fact noSharedBase {
	all s: Base | one s.-bases
}

fact signalReceivedByBaseInAccessLevel {
	all abs: ABS, b: Badge, s: Base, r: Room |
			b.signal-bases.level in b.accessLevel
}

fact locatedByBase {
	all abs: ABS, b: Badge, s: Base, r: Room |
			{b -> r} in abs.location iff s in r.bases && b.signal = s
}

fact registeredBasesHaveRegistered {
	all abs': ABS, b: Badge | some e: Entrance, some abs1, abs2: ABS |
	b in abs'.registered iff {register{abs1, abs2, b, e} && abs1 in abs'.^prev
	&& abs2 in abs'.*prev}
}

fact stateChangedByMoveRegisterLeave {
	all a1, a2 : ABS |
		a1 in a2.prev iff some b : Badge, r1, r2 : Room, e: Entrance |
		{move{a1, a2, b, r1, r2} || register{a1, a2, b, e} ||
		leave{a1, a2, b, e}}
}

fact noSelfPrev {
	all a: ABS | a !in a.prev
}

fact prevNotSymmetric {
	all a1, a2: ABS | a1 in a2.prev && a2 in a1.prev => a1=a2
}

------------functions-----------------

fun move (abs,abs': ABS, b: Badge, from,to: Room) {
	{b -> from} in abs.location &&
	to.level in b.accessLevel &&
	abs'.location = abs.location ++ (b -> to) &&
	b.history = b.history + to &&
	abs'.prev = abs && abs'.registered=abs.registered &&
	abs'.rooms=abs.rooms && abs'.entrance=abs.entrance
}

fun find (abs: ABS, r: Room, b: Badge) {
	r = b.{abs.location}
}

fun withOthers (abs: ABS, b1: se Badge, b2: Badge) {
	b1 = b2.{abs.location}.~{abs.location}
}

fun look (abs: ABS, b: set Badge, r: Room) {
	b = r.~{abs.location}
}

fun register (abs,abs': ABS, b: Badge, e: Entrance) {
	abs'.registered = abs.registered + b &&
	abs'.location = abs.location ++ (b -> e) &&
	b.history = e &&
	abs'.prev = abs && abs'.rooms=abs.rooms && abs'.entrance=abs.entrance
} 

fun leave (abs,abs': ABS, b: Badge, e: Entrance) {
	e = b.(abs.location) &&
	abs'.registered = abs.registered - b &&
	abs'.location = abs.location - (b -> b.(abs.location)) &&
	abs'.prev = abs && abs'.rooms-abs.rooms && abs'.entrance-abs.entrance
}

fun show() {
	#ABS.registered=1
	#Room=1
	#Level=1
}

-----------assertions ------------

assert BadgeHasAtMostOneLocation {
	all abs: ABS< b: Badge | sole b.(abs.location)
}

assert historyConsistency {
	all abs: ABS, b: Badge, r: Room | b in abs.registered =>
	(b.history).level in b.accessLevel
}

assert moveWorks {
	all abs,abs': ABS, b: Badge, r1,r2: Room |
	move(abs,abs', b, r1, r2) => find(abs', r2, b) && r1+r2 in b.history
}

assert findAndWithWork {
	all abs: ABS, b1,b2: Badge, r: Room, b3: set Badge |
	find {abs, r, b1} && find {abs, r, b2} && withOthers {abs, b3, b2} => b1 in b3 
}

assert findAndLookWork {
	all abs: ABS, b1: Badge, b2: set Badge, r: Room |
			find(abs,r,b1) && looks{abs,b2,r} => b1 in b2 
}

assert lookAndWithWork {
	all abs: ABS, b1: Badge, b2: set Badge, r: Room |
			withOthers{abs,b2,b2} && find{abs, r, b1} => look{abs, b2, r}
}

assert withOthersIsSymmetric {
	all abs: ABS, b1, b2: Badge, bs1,bs2: set Badge | 
	withOthers(abs, bs1, b2) && withOthers(abs, bs2, b1) && b1 in bs1 && b1!=b2
	=> b2 in bs2
}

assert registerWorks {
	all abs1: ABS, b: Badge, r: Room | some abs2,abs3: ABS, e: Entrance | 
			find{abs1, r, b} iff register{abs2, abs3, b, e}
}

assert leaveWorks {
	all abs': ABS, b: Badge | some e: Entrance | some abs: ABS |
			leave{abs, abs' b, e} => no r: Room | r in abs'.room & 
			find(abs'r,b)
}

assert registerAndMove {
	all abs': ABS, b: Badge, e: Entrance, r: Room | some abs1,abs2: ABS |
			register{abs1,abs2,b,e} && move{abs2,abs',b,e,r} => e+r in 
			b.history && find(abs', r, b)
}


assert sameHistoryImpliesWereTogether {
	all disj b1, b2: Badge, a2: ABS | 
			b1+b2 in a2.registered && b1.history=b2.history =>
					some a1: ABS, bs: set Badge |
							a1 in a2.^prev && withOthers(a1,bs,b1) && b2 in bs
}