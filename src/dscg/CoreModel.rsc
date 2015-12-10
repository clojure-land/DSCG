/**
 * Copyright (c) 2015 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 */
module dscg::CoreModel

import dscg::Common;
import dscg::Common_ContentType;

import IO;
import List;
import Map;
import Relation;
import Set;
import String;
import ValueIO;
//import lang::rascal::\syntax::Rascal;
import util::Math;
import util::Maybe;



// TODO: infer common collection type for generics 



test bool pushDownTest() {
	Type payloadType = eitherTypeSequence({ s.itemType | /Partition s := simplify(pscene_typedPayload_typedRarePayload_typedNodes(), psStripIfReferenceType()), s is slice, s.contentType is ctPayloadTuple }); 

	return pushDownEitherType(payloadType) == typeSequence([
		eitherTypeSequence({ primitive("int"), generic("K") }),
		eitherTypeSequence({ primitive("int"), generic("V") })
	]);
}

Type transformEitherType(Type t:eitherTypeSequence({ t1, t2 })) 
	= typeSequence([
		eitherTypeSequence(toSet(zipped[0])), 
		eitherTypeSequence(toSet(zipped[1]))])
when t1 is typeSequence, t2 is typeSequence, zipped := zip(t1.typeArgumentList, t2.typeArgumentList); 

default Type transformEitherType(Type t) = t;

Type pushDownEitherType(Type topType) {
	return top-down visit(topType) {
		case Type \type => transformEitherType(\type)
	}
}



//Type generalizeEitherType(Type t:eitherTypeSequence(set[Type] typeArgumentSet)) = object();

CoreModel filterByPartitionTypeSlice(CoreModel cm)
	= [ s | /Partition s := cm, s is slice ];

CoreModel filterByPartitionTypeSlice(CoreModel cm, set[str] idSet)
	= [ s | /Partition s := cm, s is slice, s.id in idSet ];

CoreModel filterByContentTypePayloadTuple(CoreModel cm)
	= [ s | /Partition s := cm, s is slice, s.contentType is ctPayloadTuple ];

CoreModel filterByContentTypePayloadTuple(CoreModel cm, set[str] idSet)
	= [ s | s <- filterByContentTypePayloadTuple(cm), s.id in idSet ];

default Type generalizeEitherType(Type t) {
	return bottom-up visit(t) {
		case eitherTypeSequence(set[Type] typeArgumentSet) => object()
	}
}
 

int anExperiment() {
	list[Partition] partitionList = pscene_typedPayload_typedNodes_bounded();	

	map[str, int] substitutionMap = ( "payload": 2, "rarePayload": 3, "node": 4 );	

	JvmMemoryLayoutOption mlOption = compressedOops(true);
	
	// TODO: consider amount of bitmaps and their sizes 
	return sizeOfObjectHeader(mlOption) + partitionListFootprint(partitionList, substitutionMap, mlOption);
}

int partitionListFootprint(list[Partition] partitionList, map[str, int] substitutionMap, JvmMemoryLayoutOption mlOption) {
	sizeTypeRelation 
		= { <s.id, sizeOf(mlOption, s.itemType)> | /Partition s := partitionList, s is slice };
	
	return ( 0 | it + size * count | <size, count> <- sizeTypeRelation<1,0> o toRel(substitutionMap) ); 
}

bool is8ByteAligned(int sizeInBytes) = (sizeInBytes % 8 == 0); 

bool is8ByteAligned(map[str, int] substitutionMap) {
	list[Partition] partitionList = pscene_typedPayload_typedNodes_bounded();	
	JvmMemoryLayoutOption mlOption = compressedOops(true);
	 
	int sizeInBytes = sizeOfObjectHeader(mlOption) + partitionListFootprint(partitionList, substitutionMap, mlOption);
	
	return is8ByteAligned(sizeInBytes);
}



/*
 * Simplified model of JVM settings that influence memory layout of objects. 
 */
data JvmMemoryLayoutOption
	= jvmMemoryLayoutNoop() 
	| compressedOops(bool isEnabled);


int sizeOf(JvmMemoryLayoutOption _, Type::___primitive(str \type, isArray = false)) = 1 when \type == "byte";
int sizeOf(JvmMemoryLayoutOption _, Type::___primitive(str \type, isArray = false)) = 2 when \type == "short";
int sizeOf(JvmMemoryLayoutOption _, Type::___primitive(str \type, isArray = false)) = 4 when \type == "int";
int sizeOf(JvmMemoryLayoutOption _, Type::___primitive(str \type, isArray = false)) = 8 when \type == "long";

// does not consider padding
int sizeOf(JvmMemoryLayoutOption mlOption, Type::typeSequence(list[Type] typeArguments)) 
	= ( 0 | it + sizeOf(mlOption, t) | t <- typeArguments );

int sizeOf(JvmMemoryLayoutOption::compressedOops(false), Type t) = 8 when t has isArray, t.isArray == false;
int sizeOf(JvmMemoryLayoutOption::compressedOops(true),  Type t) = 4 when t has isArray, t.isArray == false;
int sizeOf(JvmMemoryLayoutOption _,  Type t) = 4 when t has isArray, t.isArray == false;

default int sizeOf(JvmMemoryLayoutOption mlOption, Type t) { throw "<mlOption> \n <t>"; }


int sizeOfObjectHeader(JvmMemoryLayoutOption::compressedOops(true )) = 12;
int sizeOfObjectHeader(JvmMemoryLayoutOption::compressedOops(false)) = 16;
default int sizeOfObjectHeader(JvmMemoryLayoutOption _) = 12;


int maxUntypedSlotArityCount(TrieSpecifics ts)
	= tupleLength(ts.ds) * ts.nMax 
when isOptionEnabled(ts, useHeterogeneousEncoding());

Maybe[int] maxUntypedSlotArityCount(list[Partition] partitionList) 
	= nothing()
when /Partition x := partitionList, x.range == unbounded();

Maybe[int] maxUntypedSlotArityCount(list[Partition] partitionList) { 
	for([ *_, Partition x, *_ ] := partitionList) {
		println(x.range);
		println(uniqueStates(x));
	}

	return just(1);	
}




data RangeStrategy 
	= rsShare()
	| rsConcat();

int size(Range r) 
	= size([r.lower, r.lower + r.stride .. r.upper + extension])
when extension := (r.isUpperIncluded ? 1 : 0);

// TODO: traverse recursively and perform different calculations based on function (with / without scaling) 
// TODO: distinct base case (slice) and calculated attribute (strip)
int logicalSize(Partition p) 
	= size(p.range)
when p is slice;

int logicalSize(Partition p) 
	= size(( emptyRange() | widenLowerAndUpper(it, np.range) | np <- p.sliceList ))
when p is strip;
	
int physicalSize(Partition p) 
	= size(scaleLowerAndUpper(p.range, arity(p.itemType)))
when p is slice;

int physicalSize(Partition p) 
	= size(( emptyRange() | widenLowerAndUpper(it, scaleLowerAndUpper(np.range, arity(np.itemType))) | np <- p.sliceList ))
when p is strip;

Range scaleLowerAndUpper(Range r:range(lower, upper, isUpperIncluded, stride), int factor) 
	= range(lower * factor, upper * factor, isUpperIncluded, stride);
	
Range widenLowerAndUpper(Range r1:range(lower1, upper1, bool isUpperIncluded, int stride), Range r2:range(lower2, upper2, isUpperIncluded, stride)) 
	= range(min(lower1, lower2), max(upper1, upper2), isUpperIncluded, stride);	

Range widenLowerAndUpper(Range::unbounded(), Range::unbounded()) = Range::unbounded();

default Range widenLowerAndUpper(Range r1, Range r2) { throw "Cannot merge ranges if `isUpperIncluded` or `stride` differ.\n\n<r1>\n\n<r2>"; }

int arity(Type::typeSequence(list[Type] typeArguments)) = size(typeArguments);
default int arity(Type _) = 1;

int uniqueStates(Partition p) 
	= logicalSize(p)
when p.contentType != ctSlot();

int uniqueStates(Partition p) 
	= physicalSize(p)
when p.contentType == ctSlot();




/*
	specializedBitmapIndexedNode(mn, m)) | m <- [0..ts.nMax+1], mn <- [0..tupleLength(ts.ds) * (ts.nMax - m)  + 1], (m + mn/2) <= ts.nBound ]
	
	int heterogeneousBound = tupleLength(ts.ds) * ts.nBound;
*/

alias CoreModel = list[Partition];

CoreModel getCoreModel(TrieSpecifics ts) = simplify(currentCoreModel(), psStripIfReferenceType()); 

list[Partition] currentCoreModel() = [
		slice("payload", ctPayloadTuple(isRare = false), typeSequence([ primitive("int"), primitive("int") ])),
		slice("rarePayload", ctPayloadTuple(isRare = true), typeSequence([ object(), object() ])),		
		slice("node", ctNode(), specific("Node"), direction = backward())
	];

data Direction 
	= forward()
	| backward()
	| mixed(); // offset, heap/stack

// TODO: create 'either' type
// TODO: create 'strategies' for merging slices (e.g., based on IDs)
data Partition
	= slice(str id, ContentType contentType, Type itemType, Range range = unbounded(), Direction direction = forward())
	| strip(str id, ContentType contentType, Type itemType, Direction direction, list[Partition] sliceList, Range range = unbounded());
	
data Range
	= unbounded()
	| range(int lower, int upper, bool isUpperIncluded, int stride);
			
Range rangeFromToInclusive(int lower, int upper) = range(lower, upper, true, 1);
Range emptyRange() = range(0, 0, true, 1);
	
/*
	Example:
		slice("payload", ctPayloadTuple(), [ primitive("int"), primitive("int") ])
	
	typedPayload | typedNodes
	untypedPayload | untypedNodes
	[...]	
	typedPayload | typedRarePayload | typedNodes			pp(simplify(pscene_untypedPayload_untypedRarePayload_untypedNodes(), psStrip({ "payload", "node" })))
	typedPayload | untypedRarePayload | untypedNodes
	untypedPayload | untypedRarePayload | untypedNodes	
*/	

//str pp(list[Partition] partitionList) = ( "" | it + pp(e) | e <- partitionList );
//str pp(list[Partition] partitionList) = toString([ pp(e) | e <- partitionList ]); 
str pp(list[Partition] partitionList) = intercalate(" ++ ", [ pp(e) | e <- partitionList ]);

str pp(Partition::slice(str id, ContentType contentType, Type itemType)) 
	= "slice(<id>)"; 

str pp(Partition::strip(str _, ContentType contentType, Type itemType, Direction direction, list[Partition] sliceList)) 
	= "strip(<id>)"
when id := intercalate("+", [ e.id | e <- sliceList ]); 
	
data PartitionStrategy
	= psStrip(set[str] idSet)
	| psStripIfReferenceType();
	// TODO: add boxing strategy
	// TODO: add strategy to disect either types
	


list[Partition] pscene_typedPayload_typedNodes_bounded() = [
		slice("payload", ctPayloadTuple(isRare = false), typeSequence([ primitive("int"), primitive("int") ]), range = rangeFromToInclusive(0, 32)),
		slice("node", ctNode(), specific("Node"), direction = backward(), range = rangeFromToInclusive(0, 32))
	];

list[Partition] pscene_typedPayload_typedRarePayload_typedNodes_bounded() = [
		slice("payload", ctPayloadTuple(isRare = false), typeSequence([ primitive("int"), primitive("int") ]), range = rangeFromToInclusive(0, 32)),
		slice("rarePayload", ctPayloadTuple(isRare = true), typeSequence([ generic("K"), generic("V") ]), range = rangeFromToInclusive(0, 32)),		
		slice("node", ctNode(), specific("Node"), direction = backward(), range = rangeFromToInclusive(0, 32))
	];

list[Partition] pscene_typedPayload_typedRarePayload_typedNodes_bounded_simplifyWith_psStripIfReferenceType() = 
	simplify(pscene_typedPayload_typedRarePayload_typedNodes_bounded(), psStripIfReferenceType());	

		
	
list[Partition] pscene_typedPayload_typedNodes() = [
		slice("payload", ctPayloadTuple(isRare = false), typeSequence([ primitive("int"), primitive("int") ])),
		slice("node", ctNode(), specific("Node"), direction = backward())
	];

test bool test_pp_pscene_typedPayload_typedNodes() = 
	pp(pscene_typedPayload_typedNodes()) == "slice(payload) ++ slice(node)";
	
test bool test_pp_pscene_typedPayload_typedNodes_simplifyWith_psStrip() = 
	pp(simplify(pscene_typedPayload_typedNodes(), psStrip({ "payload", "node" }))) == "strip(payload+node)";
	
// no simplification happens
test bool test_pp_pscene_typedPayload_typedNodes_simplifyWith_psStripIfReferenceType() = 
	pp(simplify(pscene_typedPayload_typedNodes(), psStripIfReferenceType())) == "slice(payload) ++ slice(node)";			



	
list[Partition] pscene_typedPayload_typedRarePayload_typedNodes() = [
		slice("payload", ctPayloadTuple(isRare = false), typeSequence([ primitive("int"), primitive("int") ])),
		slice("rarePayload", ctPayloadTuple(isRare = true), typeSequence([ generic("K"), generic("V") ])),		
		slice("node", ctNode(), specific("Node"), direction = backward())
	];
	
// no simplification happens 
test bool test_pp_pscene_pscene_typedPayload_typedRarePayload_typedNodes_simplifyWith_psStrip() =
	pp(simplify(pscene_typedPayload_typedRarePayload_typedNodes(), psStrip({ "payload", "node" }))) == "slice(payload) ++ slice(rarePayload) ++ slice(node)";
	
test bool test_pp_pscene_pscene_typedPayload_typedRarePayload_typedNodes_simplifyWith_psStripIfReferenceType() = 
	pp(simplify(pscene_typedPayload_typedRarePayload_typedNodes(), psStripIfReferenceType())) == "slice(payload) ++ strip(rarePayload+node)";	
		
			
	
// Partition mkStrip(Partition::slice p1, Partition p2)
Partition mkStrip(Partition p1, Partition p2)
	= strip(id, contentType, itemType, direction, [ p1, p2 ], range = range)
when p1 is slice && p2 is slice && 
		id := "<p1.id>+<p2.id>" &&
		contentType := mkStripContentType(p1.contentType, p2.contentType) &&
		itemType := mkStripType(p1.itemType, p2.itemType) &&
		direction := mkStripDirection(p1.direction, p2.direction) &&
		range := mkStripRange(p1.range, p2.range);
			
Partition mkStrip(
			p1:Partition::strip(str _, ContentType _, Type _, Direction _, list[Partition] _), 
			p2:Partition::slice(str _, ContentType _, Type _))
	= strip(id, contentType, itemType, direction, [ *p1.sliceList, p2 ], range = range)
when id := "<p1.id>+<p2.id>" &&
		itemType := mkStripType(p1.itemType, p2.itemType) &&
		contentType := mkStripContentType(p1.contentType, p2.contentType) &&
		direction := mkStripDirection(p1.direction, p2.direction) && 
		range := mkStripRange(p1.range, p2.range);		
	
Partition mkStrip(Partition p1, Partition p2) = mkStrip(p2, p1) when p1 is slice && p2 is partition;

default Partition mkStrip(Partition p1, Partition p2) { print(
 	"<pp(p1)>
	'<pp(p2)>
	'<p1>
	'<p2>");
	
	throw "Ahhh";
}
	
/* calculates the joint Java type (without widening support for primitives) */ 
Type mkStripType(Type t, t) = t;
default Type mkStripType(Type _, Type _) = object();

ContentType mkStripContentType(ContentType ct, ct) = ct;
default ContentType mkStripContentType(ContentType _, ContentType _) = ContentType::ctSlot();
		
Direction mkStripDirection(Direction::forward(), Direction::forward()) = Direction::forward();
Direction mkStripDirection(Direction::forward(), Direction::backward()) = Direction::mixed();
Direction mkStripDirection(Direction::backward(), Direction::forward()) = Direction::mixed(); 
Direction mkStripDirection(Direction::backward(), Direction::backward()) = Direction::backward();

Range mkStripRange(Range::unbound(), Range _) = Range::unbound();
Range mkStripRange(Range _, Range::unbound()) = Range::unbound();
Range mkStripRange(Range r1, Range r2) = widenLowerAndUpper(r1, r2);




list[Partition] simplify(list[Partition] partitionList) = simplify(partitionList, {}); 

list[Partition] simplify(list[Partition] partitionList, PartitionStrategy strategy) = simplify(partitionList, { strategy }); 
	
list[Partition] simplify(list[Partition] partitionList, set[PartitionStrategy] strategySet) {
	simplifiedPartitionList = partitionList;
	
	solve(simplifiedPartitionList) {
		simplifiedPartitionList = simplifyOne(simplifiedPartitionList, strategySet);
	}
	
	return simplifiedPartitionList;
}


	
list[Partition] simplifyOne(list[Partition] partitionList:[ *front, p1, p2, *back ], set[PartitionStrategy] strategySet) 
	= [ *front, mkStrip(p1, p2), *back ]
when id1 := p1.id && id2 := p2.id && 
		/strategy:psStrip(idSet:{ id1, id2, *_ }) := strategySet;
		
list[Partition] simplifyOne(list[Partition] partitionList:[ *front, p1, p2, *back ], set[PartitionStrategy] strategySet) 
	= [ *front, mkStrip(p1, p2), *back ]
when isReference(p1.itemType) && isReference(p2.itemType) && 
		/strategy:psStripIfReferenceType() := strategySet;								
		
default list[Partition] simplifyOne(list[Partition] partitionList, set[PartitionStrategy] strategySet) = partitionList;





////
// FUNCTIONS TO MODEL COLLECTIONS GENERICS 
////////

/*
 * Expansion to the maximal length of generics for a 'java.util.Collection' data type.
 */
list[Type] CollectionGenericsExpanded(TrieSpecifics ts) {
	CoreModel cm = getCoreModel(ts);

	Type generalizedType = eitherTypeSequence({ s.itemType | s <- filterByContentTypePayloadTuple(cm) });
	Type simplifiedType = (generalizeEitherType o pushDownEitherType)(generalizedType);

	return singletonOrTypeSequenceToList(simplifiedType);
} 

str CollectionGenericsExpandedStr(TrieSpecifics ts)
	= "\<<intercalate(", ", mapper(typeList, typeToString))>\>"
when typeList := CollectionGenericsExpanded(ts);

/*
 * Functional view on a data structure. Views set data type as identity function.
 */
str SupplierIteratorGenericsStr(TrieSpecifics ts)
	= "\<<intercalate(", ", mapper(typeList, typeToString))>\>"
when typeList := singletonToTuple(CollectionGenericsExpanded(ts));



list[Type] GenericsExpandedUpperBounded(TrieSpecifics ts) 
	= mapper(CollectionGenericsExpanded(ts), upperBoundGeneric);

str GenericsExpandedUpperBoundedStr(TrieSpecifics ts)
	= "\<<intercalate(", ", mapper(typeList, typeToString))>\>"
when typeList := GenericsExpandedUpperBounded(ts);



Type dsAtFunction__domain_type(TrieSpecifics ts) 
	= singletonToTuple(CollectionGenericsExpanded(ts))[0];

Type dsAtFunction__range_type(TrieSpecifics ts) 
	= singletonToTuple(CollectionGenericsExpanded(ts))[1];

Type __internal__dsAtFunction__domain_type(TrieSpecifics ts) 
	= singletonToTuple(__new__internalPayloadTupleTypes__(ts))[0];

Type __internal__dsAtFunction__range_type(TrieSpecifics ts) 
	= singletonToTuple(__new__internalPayloadTupleTypes__(ts))[1];



Expression box(Expression source, Type sourceType, Type destinationType) = 
	exprFromString("<typeToString(destinationType)>.of<capitalize(typeToString(sourceType))>(<toString(source)>)")
when destinationType is eitherTypeSequence;

Expression box(Expression source, Type sourceType, sourceType) = source;

default Expression box(Expression source, Type sourceType, Type destinationType) { throw "Ahhh"; }

test bool test_boxToEither0() 
	= box(exprFromString("abc"), primitive("int"), primitive("int")) == exprFromString("abc");

test bool test_boxToEither1() 
	= box(exprFromString("abc"), primitive("int"), eitherType) == exprFromString("EitherIntOrObject.ofInt(abc)")
when eitherType := eitherTypeSequence({ object(), primitive("int") });
	
test bool test_boxToEither2() 
	= box(exprFromString("abc"), object(), eitherType) == exprFromString("EitherIntOrObject.ofObject(abc)")
when eitherType := eitherTypeSequence({ object(), primitive("int") });

/* USAGE */

str boxPayloadTupleArg1(TrieSpecifics ts, str expressionString, bool isRareCase)
	= toString(box(exprFromString(expressionString), ct2type(ts)[ctVal(isRare = isRareCase)], __new__internalPayloadTupleTypes__(ts)[1]));	



////
// TUPLE ARGUMENTS
////////

// TODO: this is flaky ...
list[Argument(Type)] typeToArgumentConverterList = [ key, val ];

 
Argument collTupleArg(TrieSpecifics ts, int idx)
	= collTupleArgs(ts)[idx];
/***/
list[Argument] collTupleArgs(TrieSpecifics ts) 
	= [ typeToArgumentConverterList[idx](\type) | idx <- [0 .. size], \type := typeList[idx] ]
when typeList := collTupleTypes(ts), size := size(typeList);

Type collTupleType(TrieSpecifics ts, int idx) = CollectionGenericsExpanded(ts)[idx];
list[Type] collTupleTypes(TrieSpecifics ts) = CollectionGenericsExpanded(ts);




//Argument payloadTupleArg(TrieSpecifics ts, int idx, bool isRare = false) = payloadTupleArgs(ts, isRare = isRare)[idx];
////list[Argument] payloadTupleArgs(TrieSpecifics ts) = __payloadTuple(ts.ds, ts.tupleTypes);
//list[Argument] payloadTupleArgs(TrieSpecifics ts, bool isRare = false) = contentList(ts, ctPayloadTuple(isRare = isRare));
/***/
list[Argument] payloadTupleArgs(TrieSpecifics ts, bool isRare = false)
	= [ typeToArgumentConverterList[idx](\type) | idx <- [0 .. size], \type := typeList[idx] ]
when typeList := payloadTupleTypes(ts, isRare = isRare), size := size(typeList);

Argument payloadTupleArg(TrieSpecifics ts, int idx, bool isRare = false) = payloadTupleArgs(ts, isRare = isRare)[idx];

list[Type] payloadTupleTypes(TrieSpecifics ts, bool isRare = false)
	= payloadTupleTypes(ts, isRare ? "rarePayload" : "payload");

list[Type] payloadTupleTypes(TrieSpecifics ts, str payloadId) {
	CoreModel cm = getCoreModel(ts);

	Type generalizedType = eitherTypeSequence({ s.itemType | s <- filterByContentTypePayloadTuple(cm, { payloadId }) });
	Type simplifiedType = (generalizeEitherType o pushDownEitherType)(generalizedType);

	return singletonOrTypeSequenceToList(simplifiedType);
}

list[Type] __new__internalPayloadTupleTypes__(TrieSpecifics ts) {
	CoreModel cm = getCoreModel(ts);

	Type generalizedType = eitherTypeSequence({ s.itemType | s <- filterByContentTypePayloadTuple(cm) });
	Type simplifiedType = (pushDownEitherType)(generalizedType);

	return singletonOrTypeSequenceToList(simplifiedType);
}

list[Type] sliceTypes(TrieSpecifics ts, str payloadId) {
	CoreModel cm = getCoreModel(ts);

	Type generalizedType = eitherTypeSequence({ s.itemType | s <- filterByPartitionTypeSlice(cm, { payloadId }) });
	Type simplifiedType = (generalizeEitherType o pushDownEitherType)(generalizedType);

	return singletonOrTypeSequenceToList(simplifiedType);
}





////
// UTILITY 
////////

list[&T] singletonToTuple(list[&T] lst:[ fst ]) = [ fst, fst ];
default list[&T] singletonToTuple(list[&T] lst) = lst;

list[Type] singletonOrTypeSequenceToList(typeSequence(typeArgumentList)) = typeArgumentList;
default list[Type] singletonOrTypeSequenceToList(Type t) = [ t ];

tuple[&T, &T] toTuple(list[&T] xs:[ x, y ]) = <x, y>;
tuple[&T, &T, &T] toTuple(list[&T] xs:[ x, y, z ]) = <x, y, z>; 
 
set[&T] toSet(tuple[&T, &T] xs:<x, y>) = { x, y };
set[&T] toSet(tuple[&T, &T, &T] xs:<x, y, z>) = { x, y, z };





////
// SANDBOX 
////////

/*
		// copy payload range (isRare = <!isRare>)				
		<copyPayloadRange(ts, artifact, iconst(0), call(getDef(ts, artifact, payloadArity(isRare = !isRare))), indexIdentity, indexIdentity, isRare = !isRare)>								
						
		// copy payload range (isRare = <isRare>)				
		<copyPayloadRange(ts, artifact, iconst(0), useExpr(valIdx), indexIdentity, indexIdentity, isRare = isRare)>

		<copyPayloadRange(ts, artifact, useExpr(valIdx), plus(useExpr(valIdx), iconst(1)), indexIdentity, indexIdentity, isRare = isRare, argsOverride = (ctKey(isRare = isRare): useExpr(key(ts.keyType)), ctVal(isRare = isRare): useExpr(val(ts.valType))))>

		<copyPayloadRange(ts, artifact, useExpr(valIdx), call(getDef(ts, artifact, payloadArity(isRare = isRare))), indexIdentity, indexAdd1, isRare = isRare)>

		// copy node range
		<copyNodeRange(ts, artifact, iconst(0), call(getDef(ts, artifact, nodeArity())), indexIdentity, indexIdentity)>
*/

data PartitionCopy 
	= rangeCopyWithShift(Partition p, Expression fromIndex, Expression untilIndex, Expression(Expression) srcIndexShift, Expression(Expression) dstIndexShift)
	| insertIntoPartition(Partition p, Expression atIndex, list[Expression] valueList);
	

PartitionCopy rangeCopyByIdentity(Partition p, Expression fromIndex, Expression untilIndex)
	= rangeCopyWithShift(p, fromIndex, untilIndex, indexIdentity, indexIdentity);




void sandbox(TrieSpecifics ts, str id = "payload", str indexStr = "valIdx", list[Expression] valueList = [ identifier("key"), identifier("val") ]) {
	CoreModel cm = filterByPartitionTypeSlice(getCoreModel(ts));

	for (PartitionCopy i <- applyManipulation(cm, copyAndInsert(id, indexStr, valueList))) {
		//iprint(partitionCopyAsForLoop(ts, i));
		//println();		
		println(toString(partitionCopyAsForLoop(ts, i)));
		println();
	}
}

data Manipulation = copyAndInsert(str id, str indexStr, list[Expression] valueList);

list[PartitionCopy] applyManipulation(CoreModel cm, Manipulation m:copyAndInsert(_, _, _))
	= [ *applyManipulation(p, m) | p <- cm ];	

list[PartitionCopy] applyManipulation(Partition p, Manipulation m:copyAndInsert(_, _, _))
	= [ rangeCopyByIdentity(p, iconst(0), exprFromString("<p.id>Arity()")) ]
when p.id != m.id;

list[PartitionCopy] applyManipulation(Partition p, Manipulation m:copyAndInsert(_, _, _)) =
	p.direction == forward() ? 
		resultList :
		reverse(resultList)
when p.id == m.id, resultList := [ 
			rangeCopyByIdentity(p, iconst(0), exprFromString(m.indexStr)),
			insertIntoPartition(p, exprFromString(m.indexStr), m.valueList),	
			rangeCopyWithShift(p, exprFromString(m.indexStr), exprFromString("<p.id>Arity()"), indexIdentity, indexAdd1) ];

//CoreModel applyManipulation(CoreModel cm, Manipulation m:copyAndInsert(_, _))
//	= [ *front, middle , *back ]
//when [ *front, middle , *back ] := cm, middle has id && middle.id == m.id;



Statement partitionCopyAsForLoop(TrieSpecifics ts, PartitionCopy pc)
	= forLoop("int i = <toString(pc.fromIndex)>", "i \< <toString(pc.untilIndex)>", "i++", body = partitionCopyAsForLoop_body(ts, pc))
when pc is rangeCopyWithShift && pc.p.direction == forward();

Statement partitionCopyAsForLoop(TrieSpecifics ts, PartitionCopy pc) 
	= forLoop("int i = <toString(indexSubtract1(pc.untilIndex))>", "i \>= <toString(pc.fromIndex)>", "i--", body = partitionCopyAsForLoop_body(ts, pc))
when pc is rangeCopyWithShift && pc.p.direction == backward();
 
Statement partitionCopyAsForLoop(TrieSpecifics ts, PartitionCopy pc) 
	= partitionCopyAsForLoop_body(ts, pc, pcs = partitionCopyStruct(ts, srcAdvance = false, valueList = pc.valueList))
when pc is insertIntoPartition;

data PartitionCopyStruct = partitionCopyStruct(TrieSpecifics ts);
data PartitionCopyStruct(list[Expression] valueList = []);
data PartitionCopyStruct(Expression src = identifier("src"));
data PartitionCopyStruct(Expression srcOffset = identifier("srcOffset"));
data PartitionCopyStruct(bool srcAdvance = true);
data PartitionCopyStruct(Expression dst = identifier("dst"));
data PartitionCopyStruct(Expression dstOffset = identifier("dstOffset"));
data PartitionCopyStruct(bool dstAdvance = true);

//data PartitionCopyStructMode
//	= srcToDst()
//	| toDst();

Statement partitionCopyAsForLoop_body(TrieSpecifics ts, PartitionCopy pc, PartitionCopyStruct pcs = partitionCopyStruct(ts))
	= compoundStatement([ createCopyStatement(pc, pcs, \type, \expr) | <\type, \expr> <- zip(sliceTypes(ts, pc.p.id), valueList) ]) 
when pc.p.direction == forward(),
	valueList := (pcs.valueList != [] ? pcs.valueList : [ getFromPartition(pcs, \type) | \type <- sliceTypes(ts, pc.p.id) ]);

Statement partitionCopyAsForLoop_body(TrieSpecifics ts, PartitionCopy pc, PartitionCopyStruct pcs = partitionCopyStruct(ts))
	= compoundStatement([ createCopyStatement(pc, pcs, \type, \expr) | <\type, \expr> <- reverse(zip(sliceTypes(ts, pc.p.id), valueList)) ]) 
when pc.p.direction == backward(),
	valueList := (pcs.valueList != [] ? pcs.valueList : [ getFromPartition(pcs, \type) | \type <- sliceTypes(ts, pc.p.id) ]);



default Statement createCopyStatement(PartitionCopy pc, PartitionCopyStruct pcs, Type \type, Expression \expr) 
	= setInPartitionStatement(pcs, \type, \expr);

Expression getFromPartition(PartitionCopyStruct pcs, Type t) = 
	exprFromString("unsafe.<unsafeGetMethodNameFromType(t)>(<toString(pcs.src)>, <toString(pcs.srcOffset)>)");
	
Statement setInPartitionStatement(PartitionCopyStruct pcs, Type t, Expression valueExpression) {
	list[Statement] stmts = [ uncheckedStringStatement("unsafe.<unsafePutMethodNameFromType(t)>(<toString(pcs.dst)>, <toString(pcs.dstOffset)>, <toString(valueExpression)>);") ];

	if (pcs.srcAdvance) {
		stmts += uncheckedStringStatement("<toString(pcs.srcOffset)> += <toString(sizeOfExpression(t))>;");
	}
	
	if (pcs.dstAdvance) {
		stmts += uncheckedStringStatement("<toString(pcs.dstOffset)> += <toString(sizeOfExpression(t))>;");
	}
	
	return compoundStatement(stmts);
} 



Expression sizeOfExpression(Type t)
	= iconst(sizeOf(jvmMemoryLayoutNoop(), t))
when ___primitive(_) := t;

default Expression sizeOfExpression(Type t)
	= exprFromString("addressSize");


/* 
 * slice(str id, ContentType contentType, Type itemType, Range range = unbounded(), Direction direction = forward())
 */
data Statement = forLoop(str initializationStr, str conditionStr, str incrementStr, Statement body = emptyStatement());

str toString(Statement e) = 
	"for (<e.initializationStr>; <e.conditionStr>; <e.incrementStr>) {
	'	<toString(e.body)>
	'}"
when e is forLoop;
