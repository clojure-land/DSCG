/**
 * Copyright (c) 2014 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 */
module dscg::GenerateTrie

import IO;
import List;
import util::Math;

import dscg::Common;
import dscg::GenerateImmutableMap;

import dscg::GenerateTrie_Optional;
import dscg::GenerateTrie_Result;

import dscg::GenerateTrie_AbstractAnyNode;
import dscg::GenerateTrie_AbstractNode;
import dscg::GenerateTrie_CompactNode;
import dscg::GenerateTrie_BitmapIndexedNode;
import dscg::GenerateTrie_HashCollisionNode;
import dscg::GenerateTrie_Iterator;
import dscg::GenerateTrie_EasyIterator;
import dscg::GenerateTrie_NodeIterator;
import dscg::GenerateTrie_Core;
import dscg::GenerateTrie_CoreTransient;

void main() {
	TrieSpecifics ts = trieSpecifics(\map(), 5, 8);	
	if(___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound) := ts) {

		rel[Option,bool] setup = { 
			<useSpecialization(),true>,
			<useFixedStackIterator(),true>,
			<useStructuralEquality(),true>,
			<methodsWithComparator(),true>
		}; // { compactionViaFieldToMethod() };
	
		list[str] innerClassStrings 
			= [ generateOptionalClassString() ]
			+ [ generateResultClassString() ]
			+ [ generateAbstractAnyNodeClassString(ts, setup)]
			+ [ generateAbstractNodeClassString(ts, setup)]		
			+ [ generateCompactNodeClassString(ts, setup)];

		if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {
			innerClassStrings = innerClassStrings + [ generateBitmapIndexedNodeClassString(ts, setup)];
		}

		innerClassStrings 
			= innerClassStrings
			+ [ generateHashCollisionNodeClassString(ts, setup)]
			+ [ generateIteratorClassString(ts, setup)]
			;
		
		if (!isOptionEnabled(setup,useFixedStackIterator())) {
			innerClassStrings = innerClassStrings + [ generateEasyIteratorClassString(ts, setup)];
		}
		
		innerClassStrings 
			= innerClassStrings
			+ [ generateNodeIteratorClassString(ts, setup)]
			+ [ generateCoreTransientClassString(ts, setup)]		
			;
			
		if (isOptionEnabled(setup,useSpecialization())) {
			innerClassStrings = innerClassStrings + 
			[ generateSpecializedNodeWithBitmapPositionsClassString(n, m, ts, setup) | m <- [0..nMax+1], n <- [0..nMax+1], (n + m) <= nBound ];
		}
			
		list[str] classStrings = [ generateCoreClassString(ts, setup, intercalate("\n", innerClassStrings)) ];	
			
		// writeFile(|project://DSCG/gen/org/eclipse/imp/pdb/facts/util/AbstractSpecialisedTrieMap.java|, classStrings);
		if (ds == \map()) {
			writeFile(|project://pdb.values/src/org/eclipse/imp/pdb/facts/util/TrieMap.java|, classStrings);
		} else if (ds == \set()) {
			writeFile(|project://pdb.values/src/org/eclipse/imp/pdb/facts/util/TrieSet.java|, classStrings);
		} else {
			throw "not supported";
		}
	}
}
	
str generateClassString(int n) =  
	"class Map<n><Generics(ds)> extends AbstractSpecialisedImmutableMap<Generics(ds)> {
	'	<for (i <- [1..n+1]) {>
	'	private final K <keyName><i>;
	'	private final V <valName><i>;
	'	<}>	
	'
	'	Map<n>(<for (i <- [1..n+1]) {>final K <keyName><i>, final V <valName><i><if (i != n) {>, <}><}>) {					
	'		<checkForDuplicateKeys(n)><intercalate("\n\n", ["this.<keyName><i> = <keyName><i>; this.<valName><i> = <valName><i>;" | i <- [1..n+1]])>
	'	}

	'	@Override
	'	public boolean containsKey(Object <keyName>) {
	'		<generate_bodyOf_containsKeyOrVal(n, equalityDefault, keyName)>	
	'	}

	'	@Override
	'	public boolean containsKeyEquivalent(Object <keyName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_containsKeyOrVal(n, equalityComparator, keyName)>	
	'	}
	
	'	@Override
	'	public boolean containsValue(Object <valName>) { 
	'		<generate_bodyOf_containsKeyOrVal(n, equalityDefault, valName)>
	'	}
	
	'	@Override
	'	public boolean containsValueEquivalent(Object <valName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_containsKeyOrVal(n, equalityComparator, valName)>
	'	}
		
	'	@Override
	'	public V get(Object <keyName>) {
	'		<generate_bodyOf_get(n, equalityDefault)>
	'	}
	
	'	@Override
	'	public V getEquivalent(Object <keyName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_get(n, equalityComparator)>
	'	}	

	'	@Override
	'	public int size() {
	'		return <n>;
	'	}

	'	@Override
	'	public Set\<Entry<Generics(ds)>\> entrySet() {
	'		<generate_bodyOf_entrySet(n)>
	'	}

	'	@Override
	'	public Set\<K\> keySet() {
	'		<generate_bodyOf_keySet(n)>
	'	}

	'	@Override
	'	public Collection\<V\> values() {
	'		<generate_bodyOf_values(n)>
	'	}
	
	'	@Override
	'	public SupplierIterator<SupplierIteratorGenerics(ds)> keyIterator() {
	'		<generate_bodyOf_keyIterator(n)>
	'	}	

	'	@Override
	'	public ImmutableMap<Generics(ds)> __put(K <keyName>, V <valName>) {
	'		<generate_bodyOf_put(n, equalityDefault)>
	'	}
	
	'	@Override
	'	public ImmutableMap<Generics(ds)> __putEquivalent(K <keyName>, V <valName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_put(n, equalityComparator)>
	'	}	

	'	@Override
	'	public ImmutableMap<Generics(ds)> __remove(K <keyName>) {
	'		<generate_bodyOf_remove(n, equalityDefault)>	
	'	}

	'	@Override
	'	public ImmutableMap<Generics(ds)> __removeEquivalent(K <keyName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_remove(n, equalityComparator)>
	'	}
	
	'	@Override
	'	public TransientMap<Generics(ds)> asTransient() {
	'		return TrieMap.transientOf(<for (i <- [1..n+1]) {><keyName><i>, <valName><i><if (i != n) {>, <}><}>);
	'	}
	
	'	@Override
	'	public int hashCode() {
	'		<if (n == 0) {>return 0;<} else {>return (<for (i <- [1..n+1]) {>(Objects.hashCode(<keyName><i>) ^ Objects.hashCode(<valName><i>))<if (i != n) {> + <}><}>);<}>
	'	}		
	
	'	@Override
	'	public String toString() {
	'		<if (n == 0) {>return \"{}\";<} else {>return String.format(\"{<for (i <- [1..n+1]) {>%s=%s<if (i != n) {>, <}><}>}\", <for (i <- [1..n+1]) {><keyName><i>, <valName><i><if (i != n) {>, <}><}>);<}>
	'	}
	
	'}
	";
			
// TODO: move to List.rsc?
list[&T] replace(list[&T] xs, list[&T] old, list[&T] new) 
	= before + new + after
when [*before, *old, *after] := xs;
	
default list[&T] replace(list[&T] xs, list[&T] old, list[&T] new) {throw "aaahh";}	

//default list[&T] replace(list[&T] xs, list[&T] old, list[&T] new) = xs;

// TODO: move to List.rsc?
list[&T] insertBeforeOrDefaultAtEnd(list[&T] xs, list[&T] old, list[&T] new)
	= before + new + old + after
when [*before, *old, *after] := xs;	

default list[&T] insertBeforeOrDefaultAtEnd(list[&T] xs, list[&T] old, list[&T] new) = xs + new;		

// TODO: move to List.rsc?
list[&T] insertAfterOrDefaultAtFront(list[&T] xs, list[&T] old, list[&T] new)
	= before + old + new + after
when [*before, *old, *after] := xs;	

default list[&T] insertAfterOrDefaultAtFront(list[&T] xs, list[&T] old, list[&T] new) = new + xs;

str generate_bodyOf_updated(0, 0, str(str, str) eq) = 
	"final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);
	'return Result.modified(<nodeOf(0, 1, "mask, <keyName><if (ds == \map()) {>, <valName><}>")>);"
	;
	
str generate_bodyOf_updated(_, _, _, rel[Option,bool] setup, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;	

default str generate_bodyOf_updated(int n, int m, DataStructure ds, rel[Option,bool] setup, str(str, str) eq) {	
	// TODO merge both functions
	replaceValueByNode = str (int i, int j) {	
		args = generateMembers(n, m) - payloadTriple(i);
		args = replace(args, subnodePair(j), [field("mask"), field("node")] + subnodePair(j));
		
		return use(args);
	};
	
	// TODO merge both functions
	replaceValueByNodeAtEnd = str (int i) {
		return use(generateMembers(n, m) - payloadTriple(i) + [field("mask"), field("node")]);
	};	
		
	updated_clause_inline = str (int i) { 
		switch (ds) {		
			case \map():
				return 
					"if (mask == <keyPosName><i>) {
					'	if (<eq("<keyName>", "<keyName><i>")>) {
					'		if (<eq("<valName>", "<valName><i>")>) {
					'			result = Result.unchanged(this);
					'		} else {		
					'			// update <keyName><i>, <valName><i>
					'			result = Result.updated(<nodeOf(n, m, use(replace(generateMembers(n, m), [ val(i) ], [ field(valName) ])))>, <use(val(i))>);
					'		}
					'	} else {
					'		// merge into node
					'		final <CompactNode(ds)><Generics(ds)> node = mergeNodes(<keyName><i>, <keyName><i>.hashCode(), <valName><i>, <keyName>, <keyName>Hash, <valName>, shift + BIT_PARTITION_SIZE);
					'		
					'		<if (isOptionEnabled(setup, useStructuralEquality())) {><if (n == 0) {>result = Result.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);<} else {><intercalate(" else ", [ "if (mask \< <nodePosName><j>) { result = Result.modified(<nodeOf(n+1, m-1, replaceValueByNode(i, j))>); }" | j <- [1..n+1] ])> else {
					'			result = Result.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);
					'		}<}><} else {>result = Result.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);<}>
					'	}
					'}"; 
		
			case \set():
				return 
					"if (mask == <keyPosName><i>) {
					'	if (<eq("<keyName>", "<keyName><i>")>) {
					'		result = Result.unchanged(this);
					'	} else {
					'		// merge into node
					'		final <CompactNode(ds)><Generics(ds)> node = mergeNodes(<keyName><i>, <keyName><i>.hashCode(), <keyName>, <keyName>Hash, shift + BIT_PARTITION_SIZE);
					'		
					'		<if (n == 0) {>result = Result.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);<} else {><intercalate(" else ", [ "if (mask \< <nodePosName><j>) { result = Result.modified(<nodeOf(n+1, m-1, replaceValueByNode(i, j))>); }" | j <- [1..n+1] ])> else {
					'			result = Result.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);
					'		}<}>
					'	}
					'}"; 
					
			default:
				throw "You forgot <ds>!";			
		}
	};
			
	updated_clause_node = str (int i) { 
		switch (ds) {		
			case \map():
				return 
					"if (mask == <nodePosName><i>) {
					'	final Result<ResultGenerics> <nestedResult> = <nodeName><i>.updated(
					'					mutator, key, keyHash, val, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
					'
					'	if (<nestedResult>.isModified()) {
					'		final <CompactNode(ds)><Generics(ds)> thisNew = <nodeOf(n, m, use(replace(generateMembers(n, m), subnodePair(i), [field("mask"), field("<nestedResult>.getNode()")])))>;
					'
					'		if (<nestedResult>.hasReplacedValue()) {
					'			result = Result.updated(thisNew, <nestedResult>.getReplacedValue());
					'		} else {
					'			result = Result.modified(thisNew);
					'		}
					'	} else {
					'		result = Result.unchanged(this);
					'	}
					'}
					"; 
		
			case \set():
				return 
					"if (mask == <nodePosName><i>) {
					'	final Result<ResultGenerics> <nestedResult> = <nodeName><i>.updated(
					'					mutator, key, keyHash, val, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
					'
					'	if (<nestedResult>.isModified()) {
					'		final <CompactNode(ds)><Generics(ds)> thisNew = <nodeOf(n, m, use(replace(generateMembers(n, m), subnodePair(i), [field("mask"), field("<nestedResult>.getNode()")])))>;
					'		result = Result.modified(thisNew);
					'	} else {
					'		result = Result.unchanged(this);
					'	}
					'}
					"; 
					
			default:
				throw "You forgot <ds>!";			
		}
	};
	
	return 
	"final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);
	'final Result<ResultGenerics> result;		
	'		
	'<intercalate(" else ", [ updated_clause_inline(i)| i <- [1..m+1]] + [ updated_clause_node(i)| i <- [1..n+1]])> else {
	'	// no value
	'	<if (isOptionEnabled(setup, useStructuralEquality())) {>result = Result.modified(inlineValue(mutator, <use(payloadTriple("mask"))>));<} else {>result = Result.modified(<nodeOf(n, m+1, use(generatePayloadMembers(m) + payloadTriple("mask") + generateSubnodeMembers(n)))>);<}>
	'}
	'		
	'return result;";	
}	


str nodeOf(int n, int m, "")
	= "<CompactNode(ds)>.<Generics(ds)> nodeOf(mutator)"
	;

default str nodeOf(int n, int m, str args)
	= "nodeOf(mutator, <args>)" 	//= "new Value<m>Index<n>Node(<args>)"
	;

str generate_bodyOf_removed(0, 0, _, _, str(str, str) eq)
	= "return Result.unchanged(this);"
	;
	
str generate_bodyOf_removed(_, _, _, rel[Option,bool] setup, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;	

str generate_bodyOf_removed(0, 2, _, _, str(str, str) eq) {
	removed_clause_inline = str (int i) { return 
		"if (mask == <keyPosName><i>) {
		'	if (<eq("<keyName>", "<keyName><i>")>) {
		'		/*
		'		 * Create node with <if (ds == \map()) {>pair<} else {>element<}> <keyName><3 - i><if (ds == \map()) {>, <valName><3 - i><}>. This
		'		 * node will a) either become the new root returned, or b)
		'		 * unwrapped and inlined.
		'		 */
		'		final byte <keyPosName><3 - i>AtShiftZero = (shift == 0) ? <keyPosName><3 - i> : (byte) (keyHash & BIT_PARTITION_MASK);
		'		result = Result.modified(<nodeOf(0, 1, use(payloadTriple("<keyPosName><3 - i>AtShiftZero", 3 - i)))>);
		'	} else {
		'		result = Result.unchanged(this);
		'	}
		'}";
	};
		
	return 
	"final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);
	'final Result<ResultGenerics> result;		
	'		
	'<intercalate(" else ", [ removed_clause_inline(i) | i <- [1..3]])> else {
	'	result = Result.unchanged(this);
	'}
	'
	'return result;";		
}

default str generate_bodyOf_removed(int n, int m, DataStructure ds, rel[Option,bool] setup, str(str, str) eq) {	
	removed_clause_inline = str (int i) { return 
		"if (mask == <keyPosName><i>) {
		'	if (<eq("<keyName>", "<keyName><i>")>) {
		'		// remove <keyName><i>, <valName><i>
		'		result = Result.modified(<nodeOf(n, m-1, use(generateMembers(n, m) - payloadTriple(i)))>);
		'	} else {
		'		result = Result.unchanged(this);
		'	}
		'}";
	};

	removed_clause_node = str (int i) { return 
		"if (mask == <nodePosName><i>) {
		'	final Result<ResultGenerics> <nestedResult> = <nodeName><i>.removed(
		'					mutator, key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
		'
		'	if (<nestedResult>.isModified()) {
				final <CompactNode(ds)><Generics(ds)> updatedNode = <nestedResult>.getNode();

				switch (updatedNode.sizePredicate()) {
				<if (n == 1 && m == 0) {>case SIZE_EMPTY:
				case SIZE_ONE:
					// escalate (singleton or empty) result
					result = <nestedResult>;
					break;< } else {> case SIZE_ONE:
					// inline sub-node value
					<if (isOptionEnabled(setup, useStructuralEquality())) {>result = Result.modified(removeNode<i>AndInlineValue(mutator, <use(payloadTriple("mask", "updatedNode.headKey()", "updatedNode.headVal()"))>));<} else {>result = Result.modified(<nodeOf(n-1, m+1, use(payloadTriple("mask", "updatedNode.headKey()", "updatedNode.headVal()") + generateMembers(n, m) - subnodePair(i)))>);<}>
					break;<}>
					
				case SIZE_MORE_THAN_ONE:
					// update <nodeName><i>
					result = Result.modified(<nodeOf(n, m, use(replace(generateMembers(n, m), subnodePair(i), [field("mask"), field("updatedNode")])))>);
					break;

				default:
					throw new IllegalStateException(\"Size predicate violates node invariant.\");
				}
		'	} else {
		'		result = Result.unchanged(this);
		'	}
		'}"; 
	};
	
	return 
	"final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);
	'final Result<ResultGenerics> result;		
	'		
	'<intercalate(" else ", [ removed_clause_inline(i)| i <- [1..m+1]] + [ removed_clause_node(i)| i <- [1..n+1]])> else {
	'	result = Result.unchanged(this);
	'}
	'
	'return result;";
}
		
str generate_bodyOf_containsKey(0, 0, _, _, str(str, str) eq) 
	= "return false;"
	;
	
str generate_bodyOf_containsKey(_, _, _, rel[Option,bool] setup, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;

default str generate_bodyOf_containsKey(int n, int m, DataStructure ds, rel[Option,bool] setup, str(str, str) eq) 
	= "final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);\n\n"	
	+ intercalate(" else ", 
		["if(mask == <keyPosName><i>) { return <eq("<keyName>", "<keyName><i>")>; }" | i <- [1..m+1]] +
		["if(mask == <nodePosName><i>) { return <nodeName><i>.containsKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>); }" | i <- [1..n+1]])
	+ " else { return false; }"
	;

/* binary search version */
//default str generate_bodyOf_containsKey(int n, int m, DataStructure ds, str(str, str) eq)
//	= 
//	"final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);\n\n
//	'<generate_bodyOf_containsKey_binarySearchPayload(1, m, eq)>
//	'<generate_bodyOf_containsKey_binarySearchNode(1, n, eq)>
//	"	
//	;



str generate_bodyOf_containsKey_binarySearchNode(int left, int right, str(str, str) eq) =
	"return false;"
when left > right;	


str generate_bodyOf_containsKey_binarySearchNode(int left, int right, str(str, str) eq) =
	"/*<left>..<right>*/
	'if (mask == <nodePosName><left>) {
	'	return <nodeName><left>.containsKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);	
	'} else {
	'	return false;	
	'}"
when left == right;	

str generate_bodyOf_containsKey_binarySearchNode(int left, int right, str(str, str) eq) =
	"/*<left>..<right>*/
	'if (mask == <nodePosName><left>) {
	'	/*<left>..<left>*/
	'	return <nodeName><left>.containsKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);	
	'} else {
	'	/*<right>..<right>*/
	'	if (mask == <nodePosName><right>) {
	'		return <nodeName><right>.containsKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);			
	'	} else {
	'		return false;
	'	}	
	'}"
when left == right - 1;	
	
default str generate_bodyOf_containsKey_binarySearchNode(int left, int right, str(str, str) eq) { 	
 	int pivot = (left + right) / 2;
 	
 	//println("<left>, <pivot>, <right>");
 
	return 
	"/*<left>..<right>*/
	'if (mask \<= <nodePosName><pivot>) {
	'	/*<left>..<pivot>*/	
	'	if (mask == <nodePosName><pivot>) {
	'		/*<pivot>..<pivot>*/
	'		return <nodeName><pivot>.containsKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);	
	'	} else {
	'		<generate_bodyOf_containsKey_binarySearchNode(left, pivot - 1, eq)>	
	'	}
	'} else {
	'	<generate_bodyOf_containsKey_binarySearchNode(pivot + 1, right, eq)>
	'}";	
}







str generate_bodyOf_containsKey_binarySearchPayload(int left, int right, str(str, str) eq) =
	"//return false;"
when left > right;	


str generate_bodyOf_containsKey_binarySearchPayload(int left, int right, str(str, str) eq) =
	"/*<left>..<right>*/
	'if (mask == <keyPosName><left> && <eq("<keyName>", "<keyName><left>")>) {
	'	return true;	
	'//} else {
	'//	return false;	
	'}"
when left == right;	

str generate_bodyOf_containsKey_binarySearchPayload(int left, int right, str(str, str) eq) =
	"/*<left>..<right>*/
	'if (mask == <keyPosName><left> && <eq("<keyName>", "<keyName><left>")>) {
	'	/*<left>..<left>*/
	'	return true;	
	'} else {
	'	/*<right>..<right>*/
	'	if (mask == <keyPosName><right> && <eq("<keyName>", "<keyName><right>")>) {
	'		return true;			
	'	//} else {
	'	//	return false;
	'	}	
	'}"
when left == right - 1;	
	
default str generate_bodyOf_containsKey_binarySearchPayload(int left, int right, str(str, str) eq) { 	
 	int pivot = (left + right) / 2;
 	
 	//println("<left>, <pivot>, <right>");
 
	return 
	"/*<left>..<right>*/
	'if (mask \<= <keyPosName><pivot>) {
	'	/*<left>..<pivot>*/	
	'	if (mask == <keyPosName><pivot> && <eq("<keyName>", "<keyName><pivot>")>) {
	'		/*<pivot>..<pivot>*/
	'		return true;	
	'	} else {
	'		<generate_bodyOf_containsKey_binarySearchPayload(left, pivot - 1, eq)>	
	'	}
	'} else {
	'	<generate_bodyOf_containsKey_binarySearchPayload(pivot + 1, right, eq)>
	'}";	
}














	
str generate_bodyOf_findByKey(0, 0, _, _, str(str, str) eq) 
	= "return Optional.empty();"
	;

str generate_bodyOf_findByKey(_, _, _, rel[Option,bool] setup, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;

default str generate_bodyOf_findByKey(int n, int m, DataStructure ds, rel[Option,bool] setup, str(str, str) eq) 
	= "final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);\n\n"	
	+ intercalate(" else ", 
		["if(mask == <keyPosName><i> && <eq("<keyName>", "<keyName><i>")>) { return Optional.of(<if (ds == \map()) {>entryOf(<keyName><i>, <valName><i>)<} else {><keyName><i><}>); }" | i <- [1..m+1]] +
		["if(mask == <nodePosName><i>) { return <nodeName><i>.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>); }" | i <- [1..n+1]])
	+ " else { return Optional.empty(); }"
	;	
			
str generateGenericNodeClassString(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) =
	"private static final class Index<n>Node<Generics(ds)> extends <CompactNode(ds)><Generics(ds)> {
	'	<for (i <- [1..n+1]) {>
	'	private final byte <nodePosName><i>;
	'	private final <CompactNode(ds)><Generics(ds)> <nodeName><i>;
	'	<}>	
	
	'	Index<n>Node(<for (i <- [1..n+1]) {>final byte <nodePosName><i>, final <CompactNode(ds)><Generics(ds)> <nodeName><i><if (i != n) {>, <}><}>) {					
	'		<intercalate("\n\n", ["this.<nodePosName><i> = <nodePosName><i>; this.<nodeName><i> = <nodeName><i>;" | i <- [1..n+1]])>
	'	}
	
	'	@SuppressWarnings(\"unchecked\")	
	'	@Override
	'	Result<ResultGenerics> updated(AtomicReference\<Thread\> mutator, K <keyName>, int <keyName>Hash, V <valName>, int shift) {
	'		<generate_bodyOf_GenericNode_updated(n, m, equalityDefault)>
	'	}

	'	@SuppressWarnings(\"unchecked\")	
	'	@Override
	'	Result<ResultGenerics> updated(AtomicReference\<Thread\> mutator, K <keyName>, int <keyName>Hash, V <valName>, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_GenericNode_updated(n, m, equalityComparator)>
	'	}

	'	@SuppressWarnings(\"unchecked\")	
	'	@Override
	'	Result<ResultGenerics> removed(AtomicReference\<Thread\> mutator, K <keyName>, int <keyName>Hash, int shift) {
	'		<generate_bodyOf_GenericNode_removed(n, m, equalityDefault)>
	'	}

	'	@SuppressWarnings(\"unchecked\")	
	'	@Override
	'	Result<ResultGenerics> removed(AtomicReference\<Thread\> mutator, K <keyName>, int <keyName>Hash, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_GenericNode_removed(n, m, equalityComparator)>
	'	}
	
	'	@SuppressWarnings(\"unchecked\")
	'	@Override
	'	boolean containsKey(Object <keyName>, int <keyName>Hash, int shift) {
	'		<generate_bodyOf_GenericNode_containsKey(n, m, equalityDefault)>
	'	}

	'	@SuppressWarnings(\"unchecked\")
	'	@Override
	'	boolean containsKey(Object <keyName>, int <keyName>Hash, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_GenericNode_containsKey(n, m, equalityComparator)>
	'	}

	'	@SuppressWarnings(\"unchecked\")
	'	@Override
	'	Optional<KeyOrMapEntryGenerics> findByKey(Object <keyName>, int <keyName>Hash, int shift) {
	'		<generate_bodyOf_GenericNode_findByKey(n, m, equalityDefault)>
	'	}

	'	@SuppressWarnings(\"unchecked\")
	'	@Override
	'	Optional<KeyOrMapEntryGenerics> findByKey(Object <keyName>, int <keyName>Hash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_GenericNode_findByKey(n, m, equalityComparator)>
	'	}

	'	@Override
	'	<AbstractNode(ds)><Generics(ds)> getNode(int index) {
	'		<generate_bodyOf_getNode(n)>
	'	}

	'	@Override
	'	int nodeArity() {
	'		return <n>;
	'	}
	}
	";
	
str generate_bodyOf_getNode(0)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_getNode(int n) = 	
	"		switch(index) {
	'			<for (i <- [1..n+1]) {>case <i-1>:
	'				return <nodeName><i>;
	'			<}>default:
	'				throw new IllegalStateException(\"Index out of range.\");
	'			}"
	;
	
str generate_bodyOf_getKey(0)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_getKey(int m) = 	
	"		switch(index) {
	'			<for (i <- [1..m+1]) {>case <i-1>:
	'				return <keyName><i>;
	'			<}>default:
	'				throw new IllegalStateException(\"Index out of range.\");
	'			}"
	;

str generate_bodyOf_getValue(0)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_getValue(int m) = 	
	"		switch(index) {
	'			<for (i <- [1..m+1]) {>case <i-1>:
	'				return <valName><i>;
	'			<}>default:
	'				throw new IllegalStateException(\"Index out of range.\");
	'			}"
	;

str generate_bodyOf_copyAndSetValue(_, 0, _)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_copyAndSetValue(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) = 	
	"	final int index = dataIndex(bitpos);
	'	
	'	<dec(___bitmapField(bitPartitionSize))> = this.<use(bitmapMethod)>;
	'	<dec(___valmapField(bitPartitionSize))> = this.<use(valmapMethod)>;
	'	
	'	switch(index) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			return <nodeOf(n, m, use(replace(metadataArguments(n, m, ts) + contentArguments(n, m, ts), [ val(i) ], [ field(valName) ])))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");
	'	}"

	;
	
str generate_bodyOf_copyAndSetNode(0, _, _)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_copyAndSetNode(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) = 
	"	final int index = nodeIndex(bitpos);
	'
	'	<dec(___bitmapField(bitPartitionSize))> = this.<use(bitmapMethod)>;
	'	<dec(___valmapField(bitPartitionSize))> = this.<use(valmapMethod)>;
	'	
	'	switch(index) {
	'		<for (i <- [1..n+1]) {>case <i-1>:
	'			return <nodeOf(n, m, use(replace(metadataArguments(n, m, ts) + contentArguments(n, m, ts), [ \node(ds, i) ], [ field(nodeName) ])))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"	
	;

str generate_bodyOf_copyAndInsertValue(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) =
	"throw new IllegalStateException();"
when (n + m) == nMax && (n + m) == nBound;
	
default str generate_bodyOf_copyAndInsertValue(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) = 	
	"	final int valIndex = dataIndex(bitpos);
	'
	'	<dec(___bitmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (this.<use(bitmapMethod)>);
	'	<dec(___valmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (this.<use(valmapMethod)> | bitpos);
	'
	'	switch(valIndex) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			return <nodeOf(n, m+1, use(replace(metadataArguments(n, m, ts) + contentArguments(n, m, ts), [ key(i), val(i) ], [ field(keyName), field(valName), key(i), val(i) ])))>;
	'		<}>case <m>:
	'			return <nodeOf(n, m+1, use(insertBeforeOrDefaultAtEnd(metadataArguments(n, m, ts) + contentArguments(n, m, ts), [ \node(ds, 1) ], [ field(keyName), field(valName) ])))>;
	'		default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
	;
	
str generate_bodyOf_copyAndRemoveValue(_, 0, _)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_copyAndRemoveValue(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) = 	
	"	final int valIndex = dataIndex(bitpos);
	'
	'	<dec(___bitmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (this.<use(bitmapMethod)>);
	'	<dec(___valmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (this.<use(valmapMethod)> ^ bitpos);
	'
	'	switch(valIndex) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			return <nodeOf(n, m-1, use(metadataArguments(n, m, ts) + contentArguments(n, m, ts) - [ key(i), val(i) ]))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
	;	
	
str generate_bodyOf_copyAndRemoveNode(0, _, _)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_copyAndRemoveNode(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) = 	
	"	final int bitIndex = nodeIndex(bitpos);
	'
	'	<dec(___bitmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (this.<use(bitmapMethod)> ^ bitpos);
	'	<dec(___valmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (this.<use(valmapMethod)>);
	'
	'	switch(bitIndex) {
	'		<for (i <- [1..n+1]) {>case <i-1>:
	'			return <nodeOf(n-1, m, use(metadataArguments(n, m, ts) + contentArguments(n, m, ts) - [ \node(ds, i) ]))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
	;	

str generate_bodyOf_copyAndMigrateFromInlineToNode(_, 0, _)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_copyAndMigrateFromInlineToNode(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) = 	
	"	final int bitIndex = nodeIndex(bitpos);
	'	final int valIndex = dataIndex(bitpos);
	'
	'	<dec(___bitmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (this.<use(bitmapMethod)> | bitpos);
	'	<dec(___valmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (this.<use(valmapMethod)> ^ bitpos);
	'
	'	switch(valIndex) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			switch(bitIndex) {
	'				<for (j <- [1..n+1]) {>case <j-1>:
	'					return <nodeOf(n+1, m-1, use(replace(metadataArguments(n, m, ts) + contentArguments(n, m, ts) - [ key(i), val(i) ], [ \node(ds, j) ], [ field(nodeName), \node(ds, j) ])))>;
	'				<}>case <n>:
	'					return <nodeOf(n+1, m-1, use(metadataArguments(n, m, ts) + contentArguments(n, m, ts) - [ key(i), val(i) ] + [ field(nodeName) ]))>;
	'				default:
	'					throw new IllegalStateException(\"Index out of range.\");	
	'			}
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
	;

str generate_bodyOf_copyAndMigrateFromNodeToInline(0, _, _)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_copyAndMigrateFromNodeToInline(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) = 	
	"	final int bitIndex = nodeIndex(bitpos);
	'	final int valIndex = dataIndex(bitpos);
	'	
	'	<dec(___bitmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (this.<use(bitmapMethod)> ^ bitpos);	
	'	<dec(___valmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (this.<use(valmapMethod)> | bitpos);
	'
	'	<dec(key())> = <nodeName>.headKey();
	'	<dec(val())> = <nodeName>.headVal();	
	'
	'	switch(bitIndex) {
	'		<for (i <- [1..n+1]) {>case <i-1>:
	'			switch(valIndex) {
	'				<for (j <- [1..m+1]) {>case <j-1>:
	'					return <nodeOf(n-1, m+1, use(replace(metadataArguments(n, m, ts) + contentArguments(n, m, ts) - [ \node(ds, i) ], [ key(j), val(j) ], [ key(), val(), key(j), val(j) ])))>;
	'				<}>case <m>:
	'					return <nodeOf(n-1, m+1, use([ bitmapField, valmapField ] + insertAfterOrDefaultAtFront(contentArguments(n, m, ts) - [ \node(ds, i) ], [ key(m), val(m) ], [ key(), val() ])))>;
	'				default:
	'					throw new IllegalStateException(\"Index out of range.\");	
	'			}
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
	;
	
str generate_bodyOf_getKeyValueEntry(DataStructure ds, 0)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_getKeyValueEntry(DataStructure ds, int m) = 	
	"		switch(index) {
	'			<for (i <- [1..m+1]) {>case <i-1>:
	'				return (java.util.Map.Entry<Generics(ds)>) entryOf(<keyName><i>, <valName><i>);
	'			<}>default:
	'				throw new IllegalStateException(\"Index out of range.\");
	'			}"
	;
			
str generateCompactNodeString() = 
	"private static abstract class <CompactNode(ds)><Generics(ds)> extends <AbstractNode(ds)><Generics(ds)> {

		@SuppressWarnings(\"unchecked\")
		static final AbstractNode EMPTY_INDEX_NODE = new IndexNode(0, new AbstractNode[0], 0);

		@SuppressWarnings(\"unchecked\")
		static <Generics(ds)> <CompactNode(ds)><Generics(ds)> mergeNodes(<CompactNode(ds)><Generics(ds)> node0, int hash0,
						<CompactNode(ds)><Generics(ds)> node1, int hash1, int shift) {
			final int mask0 = (hash0 \>\>\> shift) & BIT_PARTITION_MASK;
			final int mask1 = (hash1 \>\>\> shift) & BIT_PARTITION_MASK;

			if (mask0 != mask1) {
				// both nodes fit on same level
				final int bitmap = (1 \<\< mask0) | (1 \<\< mask1);
				final <AbstractNode(ds)><Generics(ds)>[] nodes = new AbstractNode[2];

				if (mask0 \< mask1) {
					nodes[0] = node0;
					nodes[1] = node1;
				} else {
					nodes[0] = node1;
					nodes[1] = node0;
				}

				return new IndexNode\<\>(bitmap, nodes, node0.size() + node1.size());
			} else {
				// values fit on next level
				final int bitmap = (1 \<\< mask0);
				final <AbstractNode(ds)><Generics(ds)> node = mergeNodes(node0, hash0, node1, hash1, shift
								+ BIT_PARTITION_SIZE);

				return new IndexNode\<\>(bitmap, node, node.size());
			}
		}
	}"
	;
	
str generateLeafNodeString() = 
	"private static final class LeafNode<Generics(ds)> extends <CompactNode(ds)><Generics(ds)> implements Map.Entry<Generics(ds)> {

		private final K key;
		private final V val;
		private final int keyHash;

		LeafNode(K key, int keyHash, V val) {
			this.key = key;
			this.val = val;
			this.keyHash = keyHash;
		}

		@Override
		Result<Generics(ds)> updated(AtomicReference\<Thread\> mutator, K key, int keyHash, V val, int shift,
						Comparator\<Object\> cmp) {
			if (this.keyHash != keyHash)
				// insert (no collision)
				return Result.modified(mergeNodes(this, this.keyHash, new LeafNode<Generics(ds)>(key,
								keyHash, val), keyHash, shift));

			if (cmp.compare(this.key, key) != 0)
				// insert (hash collision)
				return Result.modified(new LeafHashCollisionNode<Generics(ds)>(keyHash, new LeafNode[] {
								this, new LeafNode<Generics(ds)>(key, keyHash, val) }));

			if (cmp.compare(this.val, val) != 0)
				// value replaced
				return Result.updated(new LeafNode<Generics(ds)>(key, keyHash, val), val);

			return Result.unchanged(this);
		}

		@Override
		Result<Generics(ds)> removed(AtomicReference\<Thread\> mutator, K key, int hash, int shift,
						Comparator\<Object\> cmp) {
			if (cmp.compare(this.key, key) == 0) {
				return Result.modified(EMPTY_INDEX_NODE);
			} else {
				return Result.unchanged(this);
			}
		}

		@Override
		boolean containsKey(Object key, int hash, int shift, Comparator\<Object\> cmp) {
			return this.keyHash == hash && cmp.compare(this.key, key) == 0;
		}

		@Override
		Optional<KeyOrMapEntryGenerics> findByKey(Object key, int hash, int shift, Comparator\<Object\> cmp) {
			if (this.keyHash == hash && cmp.compare(this.key, key) == 0) {
				return Optional.of((Map.Entry<Generics(ds)>) this); // TODO: not correct
			} else {
				return Optional.empty();
			}
		}

		@Override
		public K getKey() {
			return key;
		}

		@Override
		public V getValue() {
			return val;
		}

		@Override
		public V setValue(V value) {
			throw new UnsupportedOperationException();
		}

		@Override
		int arity() {
			return 1;
		}

		@Override
		public int size() {
			return 1;
		}

		@Override
		boolean hasNodes() {
			return false;
		}

		@Override
		Iterator\<<AbstractNode(ds)><Generics(ds)>\> nodeIterator() {
			return Collections.emptyIterator();
		}

		@Override
		int nodeArity() {
			return 0;
		}

		@Override
		boolean hasPayload() {
			return true;
		}

		@Override
		SupplierIterator<SupplierIteratorGenerics(ds)> payloadIterator() {
			return ArrayKeyValueIterator.of(new Object[] { key, val }, 0, 2);
		}

		@Override
		int payloadArity() {
			return 1;
		}

		@Override
		public String toString() {
			return key + \"=\" + val;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = keyHash;
			result = prime * result + key.hashCode();
			result = prime * result + ((val == null) ? 0 : val.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object other) {
			if (null == other) {
				return false;
			}
			if (this == other) {
				return true;
			}
			if (getClass() != other.getClass()) {
				return false;
			}
			LeafNode that = (LeafNode) other;
			if (keyHash != that.keyHash) {
				return false;
			}
			if (!key.equals(that.key)) {
				return false;
			}
			if (!Objects.equals(val, that.val)) {
				return false;
			}
			return true;
		}
	}"
	; 
	
	
str generateTrieMapClassString(int n) =
	"
	"
	;	
	
	
	
str generate_bodyOf_GenericNode_containsKey(_, _, _, rel[Option,bool] setup, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;	
	
default str generate_bodyOf_GenericNode_containsKey(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(str, str) eq) = 
	"final int mask = (<keyName>Hash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(___bitposField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask);
	'
	'if ((valmap & bitpos) != 0) {
	'	return <eq("nodes[dataIndex(bitpos)]", keyName)>;
	'}
	'
	'if ((bitmap & bitpos) != 0) {
	'	return ((<AbstractNode(ds)><Generics(ds)>) nodes[bitIndex(bitpos)]).containsKey(<keyName>, <keyName>Hash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'}
	'
	'return false;"
	;
	
str generate_bodyOf_GenericNode_findByKey(_, _, _, rel[Option,bool] setup, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;		
	
default str generate_bodyOf_GenericNode_findByKey(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(str, str) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(___bitposField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask);

	'if ((valmap & bitpos) != 0) { // inplace value
	'	final int valIndex = dataIndex(bitpos);
	'
	'	if (<eq("nodes[valIndex]", keyName)>) {
	'		final K _key = (K) nodes[valIndex];
	'		final V _val = (V) nodes[valIndex + 1];
	'
	'		final Map.Entry<Generics(ds)> entry = entryOf(_key, _val);
	'		return Optional.of(entry);
	'	}
	'
	'	return Optional.empty();
	'}
	'
	'if ((bitmap & bitpos) != 0) { // node (not value)
	'	final <AbstractNode(ds)><Generics(ds)> subNode = ((<AbstractNode(ds)><Generics(ds)>) nodes[bitIndex(bitpos)]);
	'
	'	return subNode.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'}
	'
	'return Optional.empty();"
	;
	
str generate_bodyOf_GenericNode_updated(_, _, _, rel[Option,bool] setup, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;	
	
default str generate_bodyOf_GenericNode_updated(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(str, str) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(___bitposField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask);
	'
	'if ((valmap & bitpos) != 0) { // inplace value
	'	final int valIndex = dataIndex(bitpos);
	'
	'	final Object currentKey = nodes[valIndex];
	'
	'	if (<eq("currentKey", keyName)>) {
	'		<if (ds == \set()) {>return Result.unchanged(this);<} else {>final Object currentVal = nodes[valIndex + 1];
	'
	'		if (<eq("currentVal", valName)>) {
	'			return Result.unchanged(this);
	'		}
	'
	'		// update mapping
	'		final <CompactNode(ds)><Generics(ds)> thisNew;
	'
	'		if (isAllowedToEdit(this.mutator, mutator)) {
	'			// no copying if already editable
	'			this.nodes[valIndex + 1] = val;
	'			thisNew = this;
	'		} else {
	'			final Object[] editableNodes = copyAndSet(this.nodes, valIndex + 1, val);
	'
	'			thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator, bitmap, valmap, editableNodes, payloadArity);
	'		}
	'
	'		return Result.updated(thisNew, (V) currentVal);<}>
	'	} else {
	'		final <CompactNode(ds)><Generics(ds)> nodeNew = mergeNodes((K) nodes[valIndex], nodes[valIndex].hashCode(),<if (ds == \map()) {> (V) nodes[valIndex + 1],<}> key, keyHash,<if (ds == \map()) {> val,<}> shift + BIT_PARTITION_SIZE);
	'
	'		final int offset = <if (ds == \map()) {>2 * <}>(payloadArity - 1);
	'		final int index = Integer.bitCount(((bitmap | bitpos) ^ (valmap ^ bitpos)) & (bitpos - 1));
	'
	'		final Object[] editableNodes = copyAndMoveToBack<if (ds == \map()) {>Pair<}>(this.nodes, valIndex, offset + index, nodeNew);
	'
	'		final <CompactNode(ds)><Generics(ds)> thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator, bitmap | bitpos, valmap ^ bitpos, editableNodes, (byte) (payloadArity - 1));
	'
	'		return Result.modified(thisNew);
	'	}
	'} else if ((bitmap & bitpos) != 0) { // node (not value)
	'	final int bitIndex = bitIndex(bitpos);
	'	final <CompactNode(ds)><Generics(ds)> subNode = (<CompactNode(ds)><Generics(ds)>) nodes[bitIndex];
	'
	'	final Result<ResultGenerics> <nestedResult> = subNode.updated(mutator, key, keyHash, val, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'
	'	if (!<nestedResult>.isModified()) {
	'		return Result.unchanged(this);
	'	}
	'
	'	final <CompactNode(ds)><Generics(ds)> thisNew;
	'
	'	// modify current node (set replacement node)
	'	if (isAllowedToEdit(this.mutator, mutator)) {
	'		// no copying if already editable
	'		this.nodes[bitIndex] = <nestedResult>.getNode();
	'		thisNew = this;
	'	} else {
	'		final Object[] editableNodes = copyAndSet(this.nodes, bitIndex, <nestedResult>.getNode());
	'
	'		thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator, bitmap, valmap, editableNodes, payloadArity);
	'	}
	'
		<if (ds == \map()) {>
	'	if (<nestedResult>.hasReplacedValue()) {
	'		return Result.updated(thisNew, <nestedResult>.getReplacedValue());
	'	}
		<}>
	'
	'	return Result.modified(thisNew);
	'} else {
	'	// no value
	'	final Object[] editableNodes = copyAndInsert<if (ds == \map()) {>Pair<}>(this.nodes, dataIndex(bitpos), key<if (ds == \map()) {>, val<}>);
	'
	'	final <CompactNode(ds)><Generics(ds)> thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator, bitmap | bitpos, valmap | bitpos, editableNodes, (byte) (payloadArity + 1));
	'
	'	return Result.modified(thisNew);
	'}";	
		
str generate_bodyOf_GenericNode_removed(_, _, _, rel[Option,bool] setup, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;			
		
default str generate_bodyOf_GenericNode_removed(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(str, str) eq) =
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	<dec(___bitposField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask);

	if ((valmap & bitpos) != 0) { // inplace value
		final int valIndex = dataIndex(bitpos);

		if (<eq("nodes[valIndex]", keyName)>) {			
			if (!USE_SPECIALIAZIONS && this.payloadArity() == 2 && this.nodeArity() == 0) {
				/*
				 * Create new node with remaining pair. The new node
				 * will a) either become the new root returned, or b)
				 * unwrapped and inlined during returning.
				 */
				final <CompactNode(ds)><Generics(ds)> thisNew;
				final int newValmap = (shift == 0) ? this.valmap ^ bitpos
								: 1L \<\< (keyHash & BIT_PARTITION_MASK);

				if (valIndex == 0) {
					thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator, newValmap,
									newValmap, new Object[] { nodes[2], nodes[3] },
									(byte) (1));
				} else {
					thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator, newValmap,
									newValmap, new Object[] { nodes[0], nodes[1] },
									(byte) (1));
				}

				return Result.modified(thisNew);
			} else if (USE_SPECIALIAZIONS && this.arity() == <nBound + 1>) {
				final Object[] editableNodes = copyAndRemove<if (ds == \map()) {>Pair<}>(this.nodes, valIndex);
	
				final <CompactNode(ds)><Generics(ds)> thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator,
								this.bitmap ^ bitpos, this.valmap ^ bitpos, editableNodes,
								(byte) (payloadArity - 1));
	
				return Result.modified(thisNew.convertToGenericNode());
			} else {
				final Object[] editableNodes = copyAndRemove<if (ds == \map()) {>Pair<}>(this.nodes, valIndex);
	
				final <CompactNode(ds)><Generics(ds)> thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator,
								this.bitmap ^ bitpos, this.valmap ^ bitpos, editableNodes,
								(byte) (payloadArity - 1));
	
				return Result.modified(thisNew);
			}
		} else {		
			return Result.unchanged(this);
		}
	} else if ((bitmap & bitpos) != 0) { // node (not value)
		final int bitIndex = bitIndex(bitpos);
		final <CompactNode(ds)><Generics(ds)> subNode = (<CompactNode(ds)><Generics(ds)>) nodes[bitIndex];
		final Result<ResultGenerics> <nestedResult> = subNode.removed(
						mutator, key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);

		if (!<nestedResult>.isModified()) {
			return Result.unchanged(this);
		}

		final <CompactNode(ds)><Generics(ds)> subNodeNew = <nestedResult>.getNode();

		switch (subNodeNew.sizePredicate()) {
		case 0: {
			if (!USE_SPECIALIAZIONS && this.payloadArity() == 0 && this.nodeArity() == 1) {
				// escalate (singleton or empty) result
				return <nestedResult>;
			} else if (USE_SPECIALIAZIONS && this.arity() == <nBound + 1>) {
				// remove node
				final Object[] editableNodes = copyAndRemove<if (ds == \map()) {>Pair<}>(this.nodes, bitIndex);

				final <CompactNode(ds)><Generics(ds)> thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator,
								bitmap ^ bitpos, valmap, editableNodes, payloadArity);

				return Result.modified(thisNew.convertToGenericNode());
			} else {
				// remove node
				final Object[] editableNodes = copyAndRemove<if (ds == \map()) {>Pair<}>(this.nodes, bitIndex);

				final <CompactNode(ds)><Generics(ds)> thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator,
								bitmap ^ bitpos, valmap, editableNodes, payloadArity);

				return Result.modified(thisNew);
			}
		}
		case 1: {
			if (!USE_SPECIALIAZIONS && this.payloadArity() == 0 && this.nodeArity() == 1) {
				// escalate (singleton or empty) result
				return <nestedResult>;
			} else {
				// inline value (move to front)
				final int valIndexNew = Integer.bitCount((valmap | bitpos) & (bitpos - 1));
	
				final Object[] editableNodes = copyAndMoveToFront<if (ds == \map()) {>Pair<}>(this.nodes, bitIndex,
								valIndexNew, subNodeNew.headKey()<if (ds == \map()) {>, subNodeNew.headVal()<}>);
	
				final <CompactNode(ds)><Generics(ds)> thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator, bitmap,
								valmap | bitpos, editableNodes, (byte) (payloadArity + 1));
	
				return Result.modified(thisNew);
			}
		}
		default: {
			// modify current node (set replacement node)
			if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[bitIndex] = subNodeNew;
				return Result.modified(this);
			} else {
				final Object[] editableNodes = copyAndSet(this.nodes, bitIndex, subNodeNew);

				final <CompactNode(ds)><Generics(ds)> thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator,
								bitmap, valmap, editableNodes, payloadArity);

				return Result.modified(thisNew);
			}
		}
		}		
	}

	return Result.unchanged(this);";

list[Argument] generateMembers(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) 
	= [ *payloadTriple(i) | i <- [1..m+1]] 
	+ [ *subnodePair(i)   | i <- [1..n+1]]
	;

list[Argument] generatePayloadMembers(int m) 
	= [ *payloadTriple(i) | i <- [1..m+1]] 
	;

list[Argument] generateSubnodeMembers(int n) 
	= [ *subnodePair(i)   | i <- [1..n+1]]
	;	

//str generate_valNodeOf_factoryMethod_special(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) {
//	return 
//	"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {
//	'	final int bitmap = 0 <intercalate(" ", mapper(bitmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
//	'	final int valmap = 0 <intercalate(" ", mapper(valmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
//	'	final Object[] content = new Object[<2*m + n>];
//	'
//	'	final java.util.SortedMap\<Byte, Map.Entry<Generics(ds)>\> sortedPayloadMasks = new java.util.TreeMap\<\>();
//	'	<for (i <- [1..m+1]) {>
//	'	sortedPayloadMasks.put(<use(keyPos(i))>, entryOf(<use(key(i))>, <use(val(i))>));
//	'	<}>
//	'	
//	'	final java.util.SortedMap\<Byte, <CompactNode(ds)><Generics(ds)>\> sortedSubnodeMasks = new java.util.TreeMap\<\>();
//	'	<for (i <- [1..n+1]) {>
//	'	sortedSubnodeMasks.put(<use(nodePos(i))>, <use(\node(ds, i))>);
//	'	<}>
//	'
//	'	int index = 0;			
//	'	for (Map.Entry\<Byte, Map.Entry<Generics(ds)>\> entry : sortedPayloadMasks.entrySet()) {
//	'		content[index++] = entry.getValue().getKey();
//	'		content[index++] = entry.getValue().getValue();
//	'	}
//	'
//	'	for (Map.Entry\<Byte, CompactMapNode<Generics(ds)>\> entry : sortedSubnodeMasks.entrySet()) {
//	'		content[index++] = entry.getValue();
//	'	}			
//	'			
//	'	return nodeOf(mutator, bitmap, valmap, content, (byte) <m>);			
//	'}
//	";
//}


str generate_valNodeOf_factoryMethod(0, 0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) { throw "TODO"; }
		
str generate_valNodeOf_factoryMethod(1, 0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) { throw "TODO"; }

str generate_valNodeOf_factoryMethod(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) {
	// TODO: remove code duplication
	members = generateMembers(n, m);
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	className = "<toString(ds)><m>To<n>Node";

	if ((n + m) <= nBound) {		
		return
		"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return new <className>\<\>(<intercalate(", ", mapper(constructorArgs, use))>);
		'}
		"; 
	} else if ((n + m) == nBound + 1 && (n + m) < nMax) {
		list[Argument] keyPosArgs  =  [ keyPos(i) | i <- [1..m+1]];
		list[Argument] nodePosArgs = [ nodePos(j) | j <- [1..n+1]];

		list[Argument] bitmapArgs = [ keyPos(i) | i <- [1..m+1]] + [ nodePos(j) | j <- [1..n+1]];
		list[Argument] valmapArgs = [ keyPos(i) | i <- [1..m+1]];
		
		list[Argument] argsForArray = [];

		if (ds == \map()) {
			argsForArray = [ key(i), val(i) | i <- [1..m+1]] + [ \node(ds, j) | j <- [1..n+1]];
		} else { 
			argsForArray = [ key(i) | i <- [1..m+1]] + [ \node(ds, j) | j <- [1..n+1]];
		}
		
		if (isOptionEnabled(setup, useStructuralEquality())) {			
			return
			"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
			'	final int bitmap = 0 <intercalate(" ", mapper(bitmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
			'	final int valmap = 0 <intercalate(" ", mapper(valmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
			'
			'	return nodeOf(mutator, bitmap, valmap, new Object[] { <use(argsForArray)> }, (byte) <m>);
			'}
			";
		} else {				
			return 
			"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {
			'	final int bitmap = 0 <intercalate(" ", mapper(bitmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
			'	final int valmap = 0 <intercalate(" ", mapper(valmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
			'	final Object[] content = new Object[] { <use(argsForArray)> } ;
			'
			'	<if (m > 1) {>
			'	<if (ds == \map()) {>
			'	// final BitonicSorterForArbitraryN_Pairs sorterPayload = new BitonicSorterForArbitraryN_Pairs();
			'	BitonicSorterForArbitraryN_Pairs.sort(new int[] { <use(keyPosArgs)> }, content, 0);
			'	<} else {>
			'	// final BitonicSorterForArbitraryN_Single sorterPayload = new BitonicSorterForArbitraryN_Single();
			'	BitonicSorterForArbitraryN_Single.sort(new int[] { <use(keyPosArgs)> }, content, 0);			
			'	<}>
			'	<}>
			'	
			'	<if (n > 1) {>
			'	// final BitonicSorterForArbitraryN_Single sorterSubnodes = new BitonicSorterForArbitraryN_Single();
			'	BitonicSorterForArbitraryN_Single.sort(new int[] { <use(nodePosArgs)> }, content, <if (ds == \map()) {><2*m><}else{><m><}>);
			'	<}>
			'
			'	return nodeOf(mutator, bitmap, valmap, content, (byte) <m>);		
			'}
			";			
		}
	} else {
		throw "Arguments out of bounds.";
	}
}
	
str generateSpecializedNodeWithBytePositionsClassString(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) {
	members = generateMembers(n, m);
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	className = "<toString(ds)><m>To<n>Node";

	return
	"private static final class <className><Generics(ds)> extends <className_compactNode(ts, setup, n != 0, m != 0)><Generics(ds)> {
	'	<intercalate("\n", mapper(members, str(Argument a) { 
			str dec = "private final <dec(a)>;";
			
			if (field(_, /.*pos.*/) := a || getter(_, /.*pos.*/) := a) {
				return "\n<dec>";
			} else {
				return dec;
			} 
		}))>
				
	'	<className>(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {		
	'		<intercalate("\n", mapper(members, str(Argument a) { 
				str dec = "this.<use(a)> = <use(a)>;";
				
				if (field(_, /.*pos.*/) := a || getter(_, /.*pos.*/) := a) {
					return "\n<dec>";
				} else {
					return dec;
				} 
			}))>
	'		<if ((n + m) > 0) {>
	'		<}>assert nodeInvariant();
	'	}

	'	@Override
	'	Result<ResultGenerics> updated(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, V<if (ds == \set()) {>oid<}> val, int shift) {
	'		<generate_bodyOf_updated(n, m, equalityDefault)>
	'	}

	'	@Override
	'	Result<ResultGenerics> updated(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, V<if (ds == \set()) {>oid<}> val, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_updated(n, m, equalityComparator)>
	'	}

	'	@Override
	'	Result<ResultGenerics> removed(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, int shift) {
	'		<generate_bodyOf_removed(n, m, equalityDefault)>
	'	}

	'	@Override
	'	Result<ResultGenerics> removed(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_removed(n, m, equalityComparator)>
	'	}

	<if (isOptionEnabled(setup, useStructuralEquality())) {>
	'	<if ((n + m) > 0) {>
	'	private <CompactNode(ds)><Generics(ds)> inlineValue(AtomicReference\<Thread\> mutator, <dec(payloadTriple("mask"))>) {
	'		<generate_bodyOf_inlineValue(n, m)>
	'	}
	'	<}>
	<}>
	
	<if (isOptionEnabled(setup, useStructuralEquality())) {>
	'	<for (j <- [1..n+1]) {>
	'	private <CompactNode(ds)><Generics(ds)> removeNode<j>AndInlineValue(AtomicReference\<Thread\> mutator, <dec(payloadTriple("mask"))>) {
	'		<generate_bodyOf_removeNodeAndInlineValue(n, m, j)>
	'	}
	'	<}>
	<}>

	'	@Override
	'	boolean containsKey(Object key, int keyHash, int shift) {
	'		<generate_bodyOf_containsKey(n, m, equalityDefault)>
	'	}

	'	@Override
	'	boolean containsKey(Object key, int keyHash, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_containsKey(n, m, equalityComparator)>
	'	}

	'	@Override
	'	Optional<KeyOrMapEntryGenerics> findByKey(Object key, int keyHash, int shift) {
	'		<generate_bodyOf_findByKey(n, m, equalityDefault)>
	'	}

	'	@Override
	'	Optional<KeyOrMapEntryGenerics> findByKey(Object key, int keyHash, int shift,
	'					Comparator\<Object\> cmp) {
	'		<generate_bodyOf_findByKey(n, m, equalityComparator)>
	'	}
	
	'	@SuppressWarnings(\"unchecked\")
	'	@Override
	'	Iterator\<<CompactNode(ds)><Generics(ds)>\> nodeIterator() {
	'		<if (n > 0) {>return ArrayIterator.\<<CompactNode(ds)><Generics(ds)>\> of(new <CompactNode(ds)>[] { <intercalate(", ", ["<nodeName><i>" | i <- [1..n+1]])> });<} else {>return Collections.emptyIterator();<}>
	'	}

	'	@Override
	'	boolean hasNodes() {
	'		return <if (n > 0) {>true<} else {>false<}>;
	'	}

	'	@Override
	'	int nodeArity() {
	'		return <n>;
	'	}	

	<if (ds == \map()) {>
	'	@Override
	'	SupplierIterator<SupplierIteratorGenerics(ds)> payloadIterator() {
	'		<if (m > 0) {>return ArrayKeyValueIterator.of(new Object[] { <intercalate(", ", ["<keyName><i>, <valName><i>"  | i <- [1..m+1]])> });<} else {>return EmptySupplierIterator.emptyIterator();<}>
	'	}
	<} else {>
	'	@Override
	'	SupplierIterator<SupplierIteratorGenerics(ds)> payloadIterator() {
	'		<if (m > 0) {>return ArrayKeyValueIterator.of(new Object[] { <intercalate(", ", ["<keyName><i>, <keyName><i>"  | i <- [1..m+1]])> });<} else {>return EmptySupplierIterator.emptyIterator();<}>
	'	}	
	<}>

	'	@Override
	'	boolean hasPayload() {
	'		return <if (m > 0) {>true<} else {>false<}>;
	'	}

	'	@Override
	'	int payloadArity() {
	'		return <m>;
	'	}
	
	'	@Override
	'	<key().\type> headKey() {
	'		<if (m == 0) {>throw new UnsupportedOperationException(\"Node does not directly contain a key.\")<} else {>return key1<}>;
	'	}

	<if (ds == \map()) {>
	'	@Override
	'	<val().\type> headVal() {
	'		<if (m == 0) {>throw new UnsupportedOperationException(\"Node does not directly contain a value.\")<} else {>return val1<}>;
	'	}	
	<}>
	
	'	@Override
	'	<CompactNode(ds)><Generics(ds)> getNode(int index) {
	'		<generate_bodyOf_getNode(n)>
	'	}
	
	'	@Override
	'	K getKey(int index) {
	'		<generate_bodyOf_getKey(m)>
	'	}

	<if (ds == \map()) {>
	'	@Override
	'	V getValue(int index) {
	'		<generate_bodyOf_getValue(m)>
	'	}
	<}>
		
	<if (ds == \map()) {>
	'	@Override
	'	Map.Entry<Generics(ds)> getKeyValueEntry(int index) {
	'		<generate_bodyOf_getKeyValueEntry(ds, m)>
	'	}
	<}>	
	
	'	@Override
	'	byte sizePredicate() {
	'		return <generate_bodyOf_sizePredicate(n, m)>;
	'	}

	<if (isOptionEnabled(setup, useStructuralEquality())) {>
	'	@Override
	'	public int hashCode() {
	'		<if ((n + m) > 0) {>final int prime = 31; int result = 1;<} else {>int result = 1;<}>
	'		<for (i <- [1..m+1]) {>
	'		<if (ds == \map()) {>result = prime * result + <valName><i>.hashCode();<}>
	'		<}><for (i <- [1..n+1]) {>
	'		result = prime * result + <nodePosName><i>;
	'		result = prime * result + <nodeName><i>.hashCode();
	'		<}>	
	'		return result;
	'	}

	'	@Override
	'	public boolean equals(Object other) {
	'		if (null == other) {
	'			return false;
	'		}
	'		if (this == other) {
	'			return true;
	'		}
	'		if (getClass() != other.getClass()) {
	'			return false;
	'		}
	'		<if ((n + m) > 0) {><className><QuestionMarkGenerics(ds)> that = (<className><QuestionMarkGenerics(ds)>) other;
	'
	'		<generate_equalityComparisons(n, m, equalityDefault)><}>
	'
	'		return true;
	'	}
	<}>	
	

	'	@Override
	'	public String toString() {		
	'		<if (n == 0 && m == 0) {>return \"[]\";<} else {>return String.format(\"[<intercalate(", ", [ "@%d: %s<if (ds == \map()) {>=%s<}>" | i <- [1..m+1] ] + [ "@%d: %s" | i <- [1..n+1] ])>]\", <use(members)>);<}>
	'	}
	
	'}
	"
	;
}

str generate_bodyOf_sizePredicate(0, 0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) = "SIZE_EMPTY";
str generate_bodyOf_sizePredicate(0, 1, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) = "SIZE_ONE";	
default str generate_bodyOf_sizePredicate(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) = "SIZE_MORE_THAN_ONE";


str generate_equalityComparisons(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), str(Argument, Argument) eq) =
	"if (<use(bitmapMethod)> != that.<use(bitmapMethod)>) {
	'	return false;
	'}
	'if (<use(valmapMethod)> != that.<use(valmapMethod)>) {
	'	return false;
	'}
	'<for (i <- [1..m+1]) {>
	'if (!(<eq(key("<keyName><i>"), key("that.<keyName><i>"))>)) {
	'	return false;
	'}
	'<if (ds == \map()) {>if (!(<eq(val("<valName><i>"), val("that.<valName><i>"))>)) {
	'	return false;
	'}<}><}><for (i <- [1..n+1]) {>
	'if (!(<eq(\node(ds, "<nodeName><i>"), \node(ds, "that.<nodeName><i>"))>)) {
	'	return false;
	'}<}>"
	;
	 

str generate_bodyOf_inlineValue(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) =
	"return <nodeOf(n, m+1, use(payloadTriple("mask") + generateSubnodeMembers(n)))>;"
when m == 0;

default str generate_bodyOf_inlineValue(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) =
	"<intercalate(" else ", [ "if (mask \< <keyPosName><i>) { return <nodeOf(n, m+1, use(insertBeforeOrDefaultAtEnd(generateMembers(n, m), payloadTriple(i), payloadTriple("mask"))))>; }" | i <- [1..m+1] ])> else {
	'	return <nodeOf(n, m+1, use(generatePayloadMembers(m) + payloadTriple("mask") + generateSubnodeMembers(n)))>;
	'}"
	;
	
str generate_bodyOf_removeNodeAndInlineValue(int n, int m, int j) =
	"return <nodeOf(n-1, m+1, use(payloadTriple("mask") + generateSubnodeMembers(n) - subnodePair(j)))>;"
when m == 0;

default str generate_bodyOf_removeNodeAndInlineValue(int n, int m, int j) =
	"<intercalate(" else ", [ "if (mask \< <keyPosName><i>) { return <nodeOf(n-1, m+1, use(insertBeforeOrDefaultAtEnd(generatePayloadMembers(m), payloadTriple(i), payloadTriple("mask")) + generateSubnodeMembers(n) - subnodePair(j)))>; }" | i <- [1..m+1] ])> else {
	'	return <nodeOf(n-1, m+1, use(generatePayloadMembers(m) + payloadTriple("mask") + generateSubnodeMembers(n) - subnodePair(j)))>;
	'}"
	;

str generateSpecializedNodeWithBitmapPositionsClassString(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) {
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + metadataArguments(n, m, ts) + contentArguments(n, m, ts);

	className = "<toString(ds)><m>To<n>Node";

	return
	"private static final class <className><Generics(ds)> extends <className_compactNode(ts, setup, n != 0, m != 0)><Generics(ds)> {
	'	<intercalate("\n", mapper(contentArguments(n, m, ts), str(Argument a) { 
			str dec = "private <dec(a)>;";
			
			if (field(_, /.*pos.*/) := a || getter(_, /.*pos.*/) := a) {
				return "\n<dec>";
			} else {
				return dec;
			} 
		}))>
				
	'	<className>(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
	'		super(mutator, <use(bitmapField)>, <use(valmapField)>);
	'		<intercalate("\n", mapper(contentArguments(n, m, ts), str(Argument a) { 
				str dec = "this.<use(a)> = <use(a)>;";
				
				if (field(_, /.*pos.*/) := a || getter(_, /.*pos.*/) := a) {
					return "\n<dec>";
				} else {
					return dec;
				} 
			}))>
	'		<if ((n + m) > 0) {>
	'		<}>assert nodeInvariant();
	'	}
	
	'	@Override
	'	Iterator\<<CompactNode(ds)><Generics(ds)>\> nodeIterator() {
	'		<if (n > 0) {>return ArrayIterator.of(<intercalate(", ", ["<nodeName><i>" | i <- [1..n+1]])>);<} else {>return Collections.\<<CompactNode(ds)><Generics(ds)>\>emptyIterator();<}>
	'	}

	'	@Override
	'	boolean hasNodes() {
	'		return <if (n > 0) {>true<} else {>false<}>;
	'	}

	'	@Override
	'	int nodeArity() {
	'		return <n>;
	'	}	

	<if (ds == \map()) {>
	'	@Override
	'	SupplierIterator<SupplierIteratorGenerics(ds)> payloadIterator() {
	'		<if (m > 0) {>return ArrayKeyValueIterator.of(new Object[] { <intercalate(", ", ["<keyName><i>, <valName><i>"  | i <- [1..m+1]])> });<} else {>return EmptySupplierIterator.emptyIterator();<}>
	'	}
	<} else {>
	'	@Override
	'	SupplierIterator<SupplierIteratorGenerics(ds)> payloadIterator() {
	'		<if (m > 0) {>return ArrayKeyValueIterator.of(new Object[] { <intercalate(", ", ["<keyName><i>, <keyName><i>"  | i <- [1..m+1]])> });<} else {>return EmptySupplierIterator.emptyIterator();<}>
	'	}	
	<}>

	'	@Override
	'	boolean hasPayload() {
	'		return <if (m > 0) {>true<} else {>false<}>;
	'	}

	'	@Override
	'	int payloadArity() {
	'		return <m>;
	'	}
	
	'	@Override
	'	<key().\type> headKey() {
	'		<if (m == 0) {>throw new UnsupportedOperationException(\"Node does not directly contain a key.\")<} else {>return key1<}>;
	'	}

	<if (ds == \map()) {>
	'	@Override
	'	<val().\type> headVal() {
	'		<if (m == 0) {>throw new UnsupportedOperationException(\"Node does not directly contain a value.\")<} else {>return val1<}>;
	'	}	
	<}>
	
	'	@Override
	'	<CompactNode(ds)><Generics(ds)> getNode(int index) {
	'		<generate_bodyOf_getNode(n)>
	'	}
	
	'	@Override
	'	<key().\type> getKey(int index) {
	'		<generate_bodyOf_getKey(m)>
	'	}

	<if (ds == \map()) {>
	'	@Override
	'	<val().\type> getValue(int index) {
	'		<generate_bodyOf_getValue(m)>
	'	}
	<}>
	
	<if (ds == \map()) {>
	'	@Override
	'	Map.Entry<Generics(ds)> getKeyValueEntry(int index) {
	'		<generate_bodyOf_getKeyValueEntry(ds, m)>
	'	}
	<}>		

	<if (ds == \map()) {>
	'	@Override
	'	<CompactNode(ds)><Generics(ds)> copyAndSetValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, V <valName>) {
	'		<generate_bodyOf_copyAndSetValue(n, m, ts)>
	'	}
	<}>	
	
	'	@Override
	'	<CompactNode(ds)><Generics(ds)> copyAndInsertValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, K <keyName>, V <valName>) {		
	'		<generate_bodyOf_copyAndInsertValue(n, m, ts)>
	'	}
	
	'	@Override
	'	<CompactNode(ds)><Generics(ds)> copyAndRemoveValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>) {
	'		<generate_bodyOf_copyAndRemoveValue(n, m, ts)>
	'	}	

	'	@Override
	'	<CompactNode(ds)><Generics(ds)> copyAndSetNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> <nodeName>) {
	'		<generate_bodyOf_copyAndSetNode(n, m, ts)>
	'	}	

	'	@Override
	'	<CompactNode(ds)><Generics(ds)> copyAndRemoveNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>) {
	'		<generate_bodyOf_copyAndRemoveNode(n, m, ts)>
	'	}	

	'	@Override
	'	<CompactNode(ds)><Generics(ds)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> <nodeName>) {
	'		<generate_bodyOf_copyAndMigrateFromInlineToNode(n, m, ts)>
	'	}
	
	'	@Override
	'	<CompactNode(ds)><Generics(ds)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> <nodeName>) {
	'		<generate_bodyOf_copyAndMigrateFromNodeToInline(n, m, ts)>
	'	}		
	
	<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {>
	'	@Override
	'	<CompactNode(ds)><Generics(ds)> convertToGenericNode() {
	'		return nodeOf(<use(thisMutator)>, <use(bitmapMethod)>, <use(valmapMethod)>, new Object[] { <use(contentArguments(n, m, ts))> }, (byte) <m>);
	'	}
	<}>	
	
	'	@Override
	'	byte sizePredicate() {
	'		return <generate_bodyOf_sizePredicate(n, m, ts)>;
	'	}

	<if (isOptionEnabled(setup, useStructuralEquality())) {>
	'	@Override
	'	public int hashCode() {
	'		<if ((n + m) > 0) {>final int prime = 31; int result = 1; result = prime * result + (<primitiveHashCode(___bitmapMethod(bitPartitionSize))>); result = prime * result + (<primitiveHashCode(___valmapMethod(bitPartitionSize))>);<} else {>int result = 1;<}>	
	'		<for (i <- [1..m+1]) {>		
	'		result = prime * result + <hashCode(key(i))>;
	'		<if (ds == \map()) {>result = prime * result + <hashCode(val(i))>;<}>
	'		<}><for (i <- [1..n+1]) {>
	'		result = prime * result + <hashCode(\node(ds, i))>;
	'		<}>	
	'		return result;
	'	}

	'	@Override
	'	public boolean equals(Object other) {
	'		if (null == other) {
	'			return false;
	'		}
	'		if (this == other) {
	'			return true;
	'		}
	'		if (getClass() != other.getClass()) {
	'			return false;
	'		}
	'		<if ((n + m) > 0) {><className><QuestionMarkGenerics(ds)> that = (<className><QuestionMarkGenerics(ds)>) other;
	'
	'		<generate_equalityComparisons(n, m, ts, equalityDefaultForArguments)><}>
	'
	'		return true;
	'	}
	<}>	
	

	'	@Override
	'	public String toString() {		
	'		<if (n == 0 && m == 0) {>return \"[]\";<} else {>return String.format(\"[<intercalate(", ", [ "@%d: %s<if (ds == \map()) {>=%s<}>" | i <- [1..m+1] ] + [ "@%d: %s" | i <- [1..n+1] ])>]\", <use([ field("recoverMask(<use(valmapMethod)>, (byte) <i>)"), key(i), val(i) | i <- [1..m+1]] + [ field("recoverMask(<use(bitmapMethod)>, (byte) <i>)"), \node(ds, i)	| i <- [1..n+1]])>);<}>
	'	}
	
	'}
	"
	;	
	
}
