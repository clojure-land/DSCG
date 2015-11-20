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

import List;

int maxUntypedSlotArityCount(TrieSpecifics ts)
	= tupleLength(ts.ds) * ts.nMax 
when isOptionEnabled(ts.setup, useHeterogeneousEncoding()); 	
	
/*
	specializedBitmapIndexedNode(mn, m)) | m <- [0..ts.nMax+1], mn <- [0..tupleLength(ts.ds) * (ts.nMax - m)  + 1], (m + mn/2) <= ts.nBound ]
	
	int heterogeneousBound = tupleLength(ts.ds) * ts.nBound;
*/

data Direction 
	= forward()
	| backward()
	| mixed(); // offset, heap/stack

// TODO: create 'either' type
// TODO: create 'strategies' for merging slices (e.g., based on IDs)
data Partition
	= slice(str id, ContentType contentType, Type itemType, Range range = unbounded(), Direction direction = forward())
	| strip(str id, Type itemType, Direction direction, list[Partition] sliceList, Range range = unbounded());
	
data Range
	= unbounded()
	| range(int lower, int upper, bool isUpperIncluded);
	
Range rangeFromToInclusive(int lower, int upper) = range(lower, upper, true);
	
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

str pp(Partition::strip(str _, Type itemType, Direction direction, list[Partition] sliceList)) 
	= "strip(<id>)"
when id := intercalate("+", [ e.id | e <- sliceList ]); 
	
data PartitionStrategy
	= psStrip(set[str] idSet)
	| psStripIfReferenceType();
	// TODO: add boxing strategy
	// TODO: add strategy to disect either types
	
		
	
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
	= strip(id, itemType, direction, [ p1, p2 ])
when p1 is slice && p2 is slice && 
		id := "<p1.id>+<p2.id>" &&
		itemType := mkStripType(p1.itemType, p2.itemType) &&
		direction := mkStripDirection(p1.direction, p2.direction);
			
Partition mkStrip(
			p1:Partition::strip(str _, Type _, Direction _, list[Partition] _), 
			p2:Partition::slice(str _, ContentType _, Type _))
	= strip(id, itemType, direction, [ *p1.sliceList, p2 ])
when id := "<p1.id>+<p2.id>" &&
		itemType := mkStripType(p1.itemType, p2.itemType) &&
		direction := mkStripDirection(p1.direction, p2.direction);		
	
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
		
Direction mkStripDirection(Direction::forward(), Direction::forward()) = Direction::forward();
Direction mkStripDirection(Direction::forward(), Direction::backward()) = Direction::mixed();
Direction mkStripDirection(Direction::backward(), Direction::forward()) = Direction::mixed(); 
Direction mkStripDirection(Direction::backward(), Direction::backward()) = Direction::backward();



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