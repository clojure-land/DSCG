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
import dscg::GenerateTrie_BitmapIndexedNode;

data DataStructure
	= \map()
	| \set()
	| \vector()
	;

data Argument
	= field (str \type, str name)
	| getter(str \type, str name)
	;

data Position // TODO: finish!
	= positionField(bool sorted = true)
	| positionBitmap()
	;
	
data Option // TODO: finish!
	= none() 
	| useSpecialization()
	| methodsWithComparator()
	| compactionViaFieldToMethod()
	;

/*
 * Rewrite Rules
 */
Argument field (str name) = field ("???", name);
Argument getter(str name) = getter("???", name);

Argument keyPos(int i) = field("byte", "<keyPosName><i>");
Argument key()		= field("K", "<keyName>");
Argument key(int i) = field("K", "<keyName><i>");
Argument val()		= field("V", "<valName>");
Argument val(int i) = field("V", "<valName><i>");

Argument nodePos(int i) = field("byte", "<nodePosName><i>");
Argument \node()		= field("<CompactNode()><Generics()>", "<nodeName>");
Argument \node(int i) 	= field("<CompactNode()><Generics()>", "<nodeName><i>");

Argument bitmap = field("int", "bitmap");
Argument valmap = field("int", "valmap");

/*
 * Functions
 */
str use(field(_, name)) = name;
str use(getter(_, name)) = "<name>()";
default str use(Argument a) { throw "You forgot <a>!"; }
/***/
str use(list[Argument] xs) = intercalate(", ", mapper(xs, use));

str dec(field(\type, name)) = "<\type> <name>";
str dec(getter(\type, name)) = "<\type> <name>()";
default str dec(Argument a) { throw "You forgot <a>!"; }
/***/
str dec(list[Argument] xs) = intercalate(", ", mapper(xs, dec));


default str toString(\map()) = "Map";
default str toString(\set()) = "Set";
default str toString(\vector()) = "Vector";
default str toString(DataStructure ds) { throw "You forgot <ds>!"; }

/*
 * Global State [TODO: remove me!]
 */
DataStructure ds = \map();

bool sortedContent = true;
bool onlyEqualityDefault = false;

str nodeName = "node";
str nodePosName = "npos";
int nMax = 32;
int nBound = 8;

str nestedResult = "nestedResult";

str keyPosName = "pos";

Argument thisMutator = field("Void", "null");

/*
 * Convenience Functions [TODO: remove global state dependency!]
 */
list[Argument] payloadTriple(int i) {
	if (ds == \map()) {
		return [ keyPos(i), key(i), val(i) ];
	} else { 
		return [ keyPos(i), key(i) ];
	}
}

list[Argument] payloadTriple(str posName) {
	if (ds == \map()) {
		return [ field("byte", posName), key(), val() ];
	} else { 
		return [ field("byte", posName), key() ];
	}
}

list[Argument] payloadTriple(str posName, int i) {
	if (ds == \map()) {
		return [ field("byte", posName), key(i), val(i) ];
	} else { 
		return [ field("byte", posName), key(i) ];
	}
}

list[Argument] payloadTriple(str posName, str keyName, str valName) {
	if (ds == \map()) {
		return [ field("byte", posName), field("K", keyName), field("V", valName) ];
	} else { 
		return [ field("byte", posName), field("K", keyName) ];
	}
} 

list[Argument] subnodePair(int i) = [ nodePos(i), \node(i) ];

str AbstractNode() = "Abstract<toString(ds)>Node";
str CompactNode() = "Compact<toString(ds)>Node";
str AwesomeNode() = "Awesome<toString(ds)>Node";

str Generics() = (ds == \map()) ? "\<K, V\>" : "\<K\>";
str ResultGenerics = (ds == \map()) ? "\<K, V, ? extends <CompactNode()><Generics()>\>" : "\<K, Void, ? extends <CompactNode()><Generics()>\>";
str KeyOrMapEntryGenerics = (ds == \map()) ? "\<java.util.Map.Entry<Generics()>\>" : "\<K\>";
str SupplierIteratorGenerics = (ds == \map()) ? "\<K, V\>" : "\<K, K\>";
str QuestionMarkGenerics = (ds == \map()) ? "\<?, ?\>" : "\<?\>";


void main() {
	set[Option] setup = { useSpecialization() }; // { compactionViaFieldToMethod() };

	classStrings = 
		[ generateBitmapIndexedNodeClassString(ds, setup)] +
		[ generateCompactNodeClassString(setup = setup)];
		
	if ({_*, useSpecialization()} := setup) {	
		classStrings = classStrings + 
		[ generateSpecializedNodeWithBitmapPositionsClassString(n, m) | m <- [0..nMax+1], n <- [0..nMax+1], (n + m) <= nBound ];
	}	
		
	writeFile(|project://DSCG/gen/org/eclipse/imp/pdb/facts/util/AbstractSpecialisedTrieMap.java|, classStrings);
}
	
str generateClassString(int n) =  
	"class Map<n><Generics()> extends AbstractSpecialisedImmutableMap<Generics()> {
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
	'	public Set\<Entry<Generics()>\> entrySet() {
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
	'	public SupplierIterator<SupplierIteratorGenerics> keyIterator() {
	'		<generate_bodyOf_keyIterator(n)>
	'	}	

	'	@Override
	'	public ImmutableMap<Generics()> __put(K <keyName>, V <valName>) {
	'		<generate_bodyOf_put(n, equalityDefault)>
	'	}
	
	'	@Override
	'	public ImmutableMap<Generics()> __putEquivalent(K <keyName>, V <valName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_put(n, equalityComparator)>
	'	}	

	'	@Override
	'	public ImmutableMap<Generics()> __remove(K <keyName>) {
	'		<generate_bodyOf_remove(n, equalityDefault)>	
	'	}

	'	@Override
	'	public ImmutableMap<Generics()> __removeEquivalent(K <keyName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_remove(n, equalityComparator)>
	'	}
	
	'	@Override
	'	public TransientMap<Generics()> asTransient() {
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
	
str generate_bodyOf_updated(_, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when onlyEqualityDefault && !(eq == equalityDefault)
	;	

default str generate_bodyOf_updated(int n, int m, str(str, str) eq) {	
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
					'		final <CompactNode()><Generics()> node = mergeNodes(<keyName><i>, <keyName><i>.hashCode(), <valName><i>, <keyName>, <keyName>Hash, <valName>, shift + BIT_PARTITION_SIZE);
					'		
					'		<if (sortedContent) {><if (n == 0) {>result = Result.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);<} else {><intercalate(" else ", [ "if (mask \< <nodePosName><j>) { result = Result.modified(<nodeOf(n+1, m-1, replaceValueByNode(i, j))>); }" | j <- [1..n+1] ])> else {
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
					'		final <CompactNode()><Generics()> node = mergeNodes(<keyName><i>, <keyName><i>.hashCode(), <keyName>, <keyName>Hash, shift + BIT_PARTITION_SIZE);
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
					'		final <CompactNode()><Generics()> thisNew = <nodeOf(n, m, use(replace(generateMembers(n, m), subnodePair(i), [field("mask"), field("<nestedResult>.getNode()")])))>;
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
					'		final <CompactNode()><Generics()> thisNew = <nodeOf(n, m, use(replace(generateMembers(n, m), subnodePair(i), [field("mask"), field("<nestedResult>.getNode()")])))>;
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
	'	<if (sortedContent) {>result = Result.modified(inlineValue(mutator, <use(payloadTriple("mask"))>));<} else {>result = Result.modified(<nodeOf(n, m+1, use(generatePayloadMembers(m) + payloadTriple("mask") + generateSubnodeMembers(n)))>);<}>
	'}
	'		
	'return result;";	
}	


str nodeOf(int n, int m, "")
	= "<CompactNode()>.<Generics()> valNodeOf(mutator)"
	;

default str nodeOf(int n, int m, str args)
	= "valNodeOf(mutator, <args>)" 	//= "new Value<m>Index<n>Node(<args>)"
	;

str generate_bodyOf_removed(0, 0, str(str, str) eq)
	= "return Result.unchanged(this);"
	;
	
str generate_bodyOf_removed(_, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when onlyEqualityDefault && !(eq == equalityDefault)
	;	

str generate_bodyOf_removed(0, 2, str(str, str) eq) {
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

default str generate_bodyOf_removed(int n, int m, str(str, str) eq) {	
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
				final <CompactNode()><Generics()> updatedNode = <nestedResult>.getNode();

				switch (updatedNode.sizePredicate()) {
				<if (n == 1 && m == 0) {>case SIZE_EMPTY:
				case SIZE_ONE:
					// escalate (singleton or empty) result
					result = <nestedResult>;
					break;< } else {> case SIZE_ONE:
					// inline sub-node value
					<if (sortedContent) {>result = Result.modified(removeNode<i>AndInlineValue(mutator, <use(payloadTriple("mask", "updatedNode.headKey()", "updatedNode.headVal()"))>));<} else {>result = Result.modified(<nodeOf(n-1, m+1, use(payloadTriple("mask", "updatedNode.headKey()", "updatedNode.headVal()") + generateMembers(n, m) - subnodePair(i)))>);<}>
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
		
str generate_bodyOf_containsKey(0, 0, str(str, str) eq) 
	= "return false;"
	;
	
str generate_bodyOf_containsKey(_, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when onlyEqualityDefault && !(eq == equalityDefault)
	;

default str generate_bodyOf_containsKey(int n, int m, str(str, str) eq) 
	= "final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);\n\n"	
	+ intercalate(" else ", 
		["if(mask == <keyPosName><i>) { return <eq("<keyName>", "<keyName><i>")>; }" | i <- [1..m+1]] +
		["if(mask == <nodePosName><i>) { return <nodeName><i>.containsKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>); }" | i <- [1..n+1]])
	+ " else { return false; }"
	;

/* binary search version */
//default str generate_bodyOf_containsKey(int n, int m, str(str, str) eq)
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














	
str generate_bodyOf_findByKey(0, 0, str(str, str) eq) 
	= "return Optional.empty();"
	;

str generate_bodyOf_findByKey(_, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when onlyEqualityDefault && !(eq == equalityDefault)
	;

default str generate_bodyOf_findByKey(int n, int m, str(str, str) eq) 
	= "final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);\n\n"	
	+ intercalate(" else ", 
		["if(mask == <keyPosName><i> && <eq("<keyName>", "<keyName><i>")>) { return Optional.of(<if (ds == \map()) {>entryOf(<keyName><i>, <valName><i>)<} else {><keyName><i><}>); }" | i <- [1..m+1]] +
		["if(mask == <nodePosName><i>) { return <nodeName><i>.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>); }" | i <- [1..n+1]])
	+ " else { return Optional.empty(); }"
	;	
			
str generateGenericNodeClassString(int n, int m) =
	"private static final class Index<n>Node<Generics()> extends <CompactNode()><Generics()> {
	'	<for (i <- [1..n+1]) {>
	'	private final byte <nodePosName><i>;
	'	private final <CompactNode()><Generics()> <nodeName><i>;
	'	<}>	
	
	'	Index<n>Node(<for (i <- [1..n+1]) {>final byte <nodePosName><i>, final <CompactNode()><Generics()> <nodeName><i><if (i != n) {>, <}><}>) {					
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
	'	<AbstractNode()><Generics()> getNode(int index) {
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

str generate_bodyOf_copyAndSetValue(_, 0)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_copyAndSetValue(int n, int m) = 	
	//"	if (isAllowedToEdit(<use(thisMutator)>, mutator)) {
	//'		// no copying if already editable
	//'
	//'		switch(index) {
	//'			<for (i <- [1..m+1]) {>case <i-1>:
	//'				<valName><i> = <valName>;
	//'			<}>default:
	//'				throw new IllegalStateException(\"Index out of range.\");
	//'		}
	//'		return this;
	//'	} else {
	//'		// create node with updated value
	//'
	//'		switch(index) {
	//'			<for (i <- [1..m+1]) {>case <i-1>:
	//'				return <nodeOf(n, m, use(replace(generateMembers_bitmap(n, m), [ val(i) ], [ field(valName) ])))>;
	//'			<}>default:
	//'				throw new IllegalStateException(\"Index out of range.\");
	//'		}	
	//'	}"
	"	switch(index) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			return <nodeOf(n, m, use(replace(generateMembers_bitmap(n, m), [ val(i) ], [ field(valName) ])))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");
	'	}"

	;
	
str generate_bodyOf_copyAndSetNode(0, _)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_copyAndSetNode(int n, int m) = 	
	//"	if (isAllowedToEdit(<use(thisMutator)>, mutator)) {
	//'		// no copying if already editable
	//'
	//'		switch(index) {
	//'			<for (i <- [1..n+1]) {>case <i-1>:
	//'				<nodeName><i> = <nodeName>;
	//'			<}>default:
	//'				throw new IllegalStateException(\"Index out of range.\");
	//'		}
	//'		return this;
	//'	} else {
	//'		// create node with updated value
	//'
	//'		switch(index) {
	//'			<for (i <- [1..n+1]) {>case <i-1>:
	//'				return <nodeOf(n, m, use(replace(generateMembers_bitmap(n, m), [ \node(i) ], [ field(nodeName) ])))>;
	//'			<}>default:
	//'				throw new IllegalStateException(\"Index out of range.\");
	//'		}	
	//'	}"
	"	switch(index) {
	'		<for (i <- [1..n+1]) {>case <i-1>:
	'			return <nodeOf(n, m, use(replace(generateMembers_bitmap(n, m), [ \node(i) ], [ field(nodeName) ])))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"	
	;
	
default str generate_bodyOf_copyAndInsertValue(int n, int m) = 	
	"	final int valIndex = valIndex(bitpos);
	'
	'	final int bitmap = this.bitmap | bitpos;
	'	final int valmap = this.valmap | bitpos;
	'
	'	switch(valIndex) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			return <nodeOf(n, m+1, use(replace(generateMembers_bitmap(n, m), [ key(i), val(i) ], [ field(keyName), field(valName), key(i), val(i) ])))>;
	'		<}>case <m>:
	'			return <nodeOf(n, m+1, use(insertBeforeOrDefaultAtEnd(generateMembers_bitmap(n, m), [ \node(1) ], [ field(keyName), field(valName) ])))>;
	'		default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
	;
	
str generate_bodyOf_copyAndRemoveValue(_, 0)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_copyAndRemoveValue(int n, int m) = 	
	"	final int valIndex = valIndex(bitpos);
	'
	'	final int bitmap = this.bitmap & ~bitpos;
	'	final int valmap = this.valmap & ~bitpos;
	'
	'	switch(valIndex) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			return <nodeOf(n, m-1, use(generateMembers_bitmap(n, m) - [ key(i), val(i) ]))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
	;	
	
str generate_bodyOf_copyAndRemoveNode(0, _)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_copyAndRemoveNode(int n, int m) = 	
	"	final int bitIndex = nodeIndex(bitpos);
	'
	'	final int bitmap = this.bitmap & ~bitpos;
	'
	'	switch(bitIndex) {
	'		<for (i <- [1..n+1]) {>case <i-1>:
	'			return <nodeOf(n-1, m, use(generateMembers_bitmap(n, m) - [ \node(i) ]))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
	;	

str generate_bodyOf_copyAndMigrateFromInlineToNode(_, 0)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_copyAndMigrateFromInlineToNode(int n, int m) = 	
	"	final int bitIndex = Integer.bitCount(((bitmap | bitpos) ^ (valmap & ~bitpos)) & (bitpos - 1));
	'	final int valIndex = valIndex(bitpos);
	'
	'	final int bitmap = this.bitmap | bitpos;
	'	final int valmap = this.valmap & ~bitpos;
	'
	'	switch(valIndex) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			switch(bitIndex) {
	'				<for (j <- [1..n+1]) {>case <j-1>:
	'					return <nodeOf(n+1, m-1, use(replace(generateMembers_bitmap(n, m) - [ key(i), val(i) ], [ \node(j) ], [ field(nodeName), \node(j) ])))>;
	'				<}>case <n>:
	'					return <nodeOf(n+1, m-1, use(generateMembers_bitmap(n, m) - [ key(i), val(i) ] + [ field(nodeName) ]))>;
	'				default:
	'					throw new IllegalStateException(\"Index out of range.\");	
	'			}
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
	;

str generate_bodyOf_copyAndMigrateFromNodeToInline(0, _)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_copyAndMigrateFromNodeToInline(int n, int m) = 	
	"	final int bitIndex = nodeIndex(bitpos);
	'	final int valIndex = valIndex(bitpos);
	'	
	'	final int valmap = this.valmap | bitpos;
	'
	'	final <dec(key())> = <nodeName>.headKey();
	'	final <dec(val())> = <nodeName>.headVal();	
	'
	'	switch(bitIndex) {
	'		<for (i <- [1..n+1]) {>case <i-1>:
	'			switch(valIndex) {
	'				<for (j <- [1..m+1]) {>case <j-1>:
	'					return <nodeOf(n-1, m+1, use(replace(generateMembers_bitmap(n, m) - [ \node(i) ], [ key(j), val(j) ], [ key(), val(), key(j), val(j) ])))>;
	'				<}>case <m>:
	'					return <nodeOf(n-1, m+1, use([ bitmap, valmap ] + insertAfterOrDefaultAtFront(generateMembers_bitmap(n, m) - [ bitmap, valmap ] - [ \node(i) ], [ key(m), val(m) ], [ key(), val() ])))>;
	'				default:
	'					throw new IllegalStateException(\"Index out of range.\");	
	'			}
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
	;
	
str generate_bodyOf_getKeyValueEntry(0)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_getKeyValueEntry(int m) = 	
	"		switch(index) {
	'			<for (i <- [1..m+1]) {>case <i-1>:
	'				return entryOf(<keyName><i>, <valName><i>);
	'			<}>default:
	'				throw new IllegalStateException(\"Index out of range.\");
	'			}"
	;
			
str generateCompactNodeString() = 
	"private static abstract class <CompactNode()><Generics()> extends <AbstractNode()><Generics()> {

		@SuppressWarnings(\"unchecked\")
		static final AbstractNode EMPTY_INDEX_NODE = new IndexNode(0, new AbstractNode[0], 0);

		@SuppressWarnings(\"unchecked\")
		static <Generics()> <CompactNode()><Generics()> mergeNodes(<CompactNode()><Generics()> node0, int hash0,
						<CompactNode()><Generics()> node1, int hash1, int shift) {
			final int mask0 = (hash0 \>\>\> shift) & BIT_PARTITION_MASK;
			final int mask1 = (hash1 \>\>\> shift) & BIT_PARTITION_MASK;

			if (mask0 != mask1) {
				// both nodes fit on same level
				final int bitmap = (1 \<\< mask0) | (1 \<\< mask1);
				final <AbstractNode()><Generics()>[] nodes = new AbstractNode[2];

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
				final <AbstractNode()><Generics()> node = mergeNodes(node0, hash0, node1, hash1, shift
								+ BIT_PARTITION_SIZE);

				return new IndexNode\<\>(bitmap, node, node.size());
			}
		}
	}"
	;
	
str generateLeafNodeString() = 
	"private static final class LeafNode<Generics()> extends <CompactNode()><Generics()> implements Map.Entry<Generics()> {

		private final K key;
		private final V val;
		private final int keyHash;

		LeafNode(K key, int keyHash, V val) {
			this.key = key;
			this.val = val;
			this.keyHash = keyHash;
		}

		@Override
		Result<Generics()> updated(AtomicReference\<Thread\> mutator, K key, int keyHash, V val, int shift,
						Comparator\<Object\> cmp) {
			if (this.keyHash != keyHash)
				// insert (no collision)
				return Result.modified(mergeNodes(this, this.keyHash, new LeafNode<Generics()>(key,
								keyHash, val), keyHash, shift));

			if (cmp.compare(this.key, key) != 0)
				// insert (hash collision)
				return Result.modified(new LeafHashCollisionNode<Generics()>(keyHash, new LeafNode[] {
								this, new LeafNode<Generics()>(key, keyHash, val) }));

			if (cmp.compare(this.val, val) != 0)
				// value replaced
				return Result.updated(new LeafNode<Generics()>(key, keyHash, val), val);

			return Result.unchanged(this);
		}

		@Override
		Result<Generics()> removed(AtomicReference\<Thread\> mutator, K key, int hash, int shift,
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
				return Optional.of((Map.Entry<Generics()>) this); // TODO: not correct
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
		Iterator\<<AbstractNode()><Generics()>\> nodeIterator() {
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
		SupplierIterator<SupplierIteratorGenerics> payloadIterator() {
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
	
	
	
str generate_bodyOf_GenericNode_containsKey(_, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when onlyEqualityDefault && !(eq == equalityDefault)
	;	
	
default str generate_bodyOf_GenericNode_containsKey(int n, int m, str(str, str) eq) = 
	"final int mask = (<keyName>Hash \>\>\> shift) & BIT_PARTITION_MASK;
	'final int bitpos = (1 \<\< mask);
	'
	'if ((valmap & bitpos) != 0) {
	'	return <eq("nodes[valIndex(bitpos)]", keyName)>;
	'}
	'
	'if ((bitmap & bitpos) != 0) {
	'	return ((<AbstractNode()><Generics()>) nodes[bitIndex(bitpos)]).containsKey(<keyName>, <keyName>Hash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'}
	'
	'return false;"
	;	
	
str generate_bodyOf_SpecializedBitmapPositionNode_containsKey(_, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when onlyEqualityDefault && !(eq == equalityDefault)
	;	
	
default str generate_bodyOf_SpecializedBitmapPositionNode_containsKey(int n, int m, str(str, str) eq) = 
	"final int mask = (<keyName>Hash \>\>\> shift) & BIT_PARTITION_MASK;
	'final int bitpos = (1 \<\< mask);
	'
	'if ((valmap & bitpos) != 0) {
	'	return <eq("getKey(keyIndex(bitpos))", keyName)>;
	'}
	'
	'if ((bitmap & bitpos) != 0) {
	'	return getNode(nodeIndex(bitpos)).containsKey(<keyName>, <keyName>Hash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'}
	'
	'return false;"
	;		
	
str generate_bodyOf_GenericNode_findByKey(_, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when onlyEqualityDefault && !(eq == equalityDefault)
	;		
	
default str generate_bodyOf_GenericNode_findByKey(int n, int m, str(str, str) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'final int bitpos = (1 \<\< mask);

	'if ((valmap & bitpos) != 0) { // inplace value
	'	final int valIndex = valIndex(bitpos);
	'
	'	if (<eq("nodes[valIndex]", keyName)>) {
	'		final K _key = (K) nodes[valIndex];
	'		final V _val = (V) nodes[valIndex + 1];
	'
	'		final Map.Entry<Generics()> entry = entryOf(_key, _val);
	'		return Optional.of(entry);
	'	}
	'
	'	return Optional.empty();
	'}
	'
	'if ((bitmap & bitpos) != 0) { // node (not value)
	'	final <AbstractNode()><Generics()> subNode = ((<AbstractNode()><Generics()>) nodes[bitIndex(bitpos)]);
	'
	'	return subNode.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'}
	'
	'return Optional.empty();"
	;
	
str generate_bodyOf_SpecializedBitmapPositionNode_findByKey(_, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when onlyEqualityDefault && !(eq == equalityDefault)
	;		
	
default str generate_bodyOf_SpecializedBitmapPositionNode_findByKey(int n, int m, str(str, str) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'final int bitpos = (1 \<\< mask);

	'if ((valmap & bitpos) != 0) { // inplace value
	'	// final int valIndex = valIndex(bitpos);
	'
	'	if (<eq("getKey(keyIndex(bitpos))", keyName)>) {
	'		final K _key = getKey(keyIndex(bitpos));
	'		final V _val = getValue(valIndex(bitpos));
	'
	'		final Map.Entry<Generics()> entry = entryOf(_key, _val);
	'		return Optional.of(entry);
	'	}
	'
	'	return Optional.empty();
	'}
	'
	'if ((bitmap & bitpos) != 0) { // node (not value)
	'	final <AbstractNode()><Generics()> subNode = getNode(nodeIndex(bitpos));
	'
	'	return subNode.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'}
	'
	'return Optional.empty();"
	;
	
str generate_bodyOf_GenericNode_updated(_, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when onlyEqualityDefault && !(eq == equalityDefault)
	;	
	
default str generate_bodyOf_GenericNode_updated(int n, int m, str(str, str) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'final int bitpos = (1 \<\< mask);
	'
	'if ((valmap & bitpos) != 0) { // inplace value
	'	final int valIndex = valIndex(bitpos);
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
	'		final <CompactNode()><Generics()> thisNew;
	'
	'		if (isAllowedToEdit(this.mutator, mutator)) {
	'			// no copying if already editable
	'			this.nodes[valIndex + 1] = val;
	'			thisNew = this;
	'		} else {
	'			final Object[] editableNodes = copyAndSet(this.nodes, valIndex + 1, val);
	'
	'			thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, bitmap, valmap, editableNodes, payloadArity);
	'		}
	'
	'		return Result.updated(thisNew, (V) currentVal);<}>
	'	} else {
	'		final <CompactNode()><Generics()> nodeNew = mergeNodes((K) nodes[valIndex], nodes[valIndex].hashCode(),<if (ds == \map()) {> (V) nodes[valIndex + 1],<}> key, keyHash,<if (ds == \map()) {> val,<}> shift + BIT_PARTITION_SIZE);
	'
	'		final int offset = <if (ds == \map()) {>2 * <}>(payloadArity - 1);
	'		final int index = Integer.bitCount(((bitmap | bitpos) ^ (valmap & ~bitpos)) & (bitpos - 1));
	'
	'		final Object[] editableNodes = copyAndMoveToBack<if (ds == \map()) {>Pair<}>(this.nodes, valIndex, offset + index, nodeNew);
	'
	'		final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, bitmap | bitpos, valmap & ~bitpos, editableNodes, (byte) (payloadArity - 1));
	'
	'		return Result.modified(thisNew);
	'	}
	'} else if ((bitmap & bitpos) != 0) { // node (not value)
	'	final int bitIndex = bitIndex(bitpos);
	'	final <CompactNode()><Generics()> subNode = (<CompactNode()><Generics()>) nodes[bitIndex];
	'
	'	final Result<ResultGenerics> <nestedResult> = subNode.updated(mutator, key, keyHash, val, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'
	'	if (!<nestedResult>.isModified()) {
	'		return Result.unchanged(this);
	'	}
	'
	'	final <CompactNode()><Generics()> thisNew;
	'
	'	// modify current node (set replacement node)
	'	if (isAllowedToEdit(this.mutator, mutator)) {
	'		// no copying if already editable
	'		this.nodes[bitIndex] = <nestedResult>.getNode();
	'		thisNew = this;
	'	} else {
	'		final Object[] editableNodes = copyAndSet(this.nodes, bitIndex, <nestedResult>.getNode());
	'
	'		thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, bitmap, valmap, editableNodes, payloadArity);
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
	'	final Object[] editableNodes = copyAndInsert<if (ds == \map()) {>Pair<}>(this.nodes, valIndex(bitpos), key<if (ds == \map()) {>, val<}>);
	'
	'	final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, bitmap | bitpos, valmap | bitpos, editableNodes, (byte) (payloadArity + 1));
	'
	'	return Result.modified(thisNew);
	'}";
		
str generate_bodyOf_SpecializedBitmapPositionNode_updated(_, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when onlyEqualityDefault && !(eq == equalityDefault)
	;	
	
default str generate_bodyOf_SpecializedBitmapPositionNode_updated(int n, int m, str(str, str) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'final int bitpos = (1 \<\< mask);
	'
	'if ((valmap & bitpos) != 0) { // inplace value
	'	final K currentKey = getKey(keyIndex(bitpos));
	'
	'	if (<eq("currentKey", keyName)>) {
	'		<if (ds == \set()) {>return Result.unchanged(this);<} else {>final V currentVal = getValue(valIndex(bitpos));
	'
	'		if (<eq("currentVal", valName)>) {
	'			return Result.unchanged(this);
	'		}
	'
	'		// update mapping
	'		final <CompactNode()><Generics()> thisNew = copyAndSetValue(mutator, valIndex(bitpos), val);
	'
	'		return Result.updated(thisNew, currentVal);<}>
	'	} else {
	'		final <CompactNode()><Generics()> nodeNew = mergeNodes(getKey(keyIndex(bitpos)), getKey(keyIndex(bitpos)).hashCode(),<if (ds == \map()) {> getValue(valIndex(bitpos)),<}> key, keyHash,<if (ds == \map()) {> val,<}> shift + BIT_PARTITION_SIZE);
	'
	'		final <CompactNode()><Generics()> thisNew = copyAndMigrateFromInlineToNode(mutator, bitpos, nodeNew);
	'
	'		return Result.modified(thisNew);
	'	}
	'} else if ((bitmap & bitpos) != 0) { // node (not value)
	'	final <CompactNode()><Generics()> subNode = getNode(nodeIndex(bitpos));
	'
	'	final Result<ResultGenerics> <nestedResult> = subNode.updated(mutator, key, keyHash, val, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'
	'	if (!<nestedResult>.isModified()) {
	'		return Result.unchanged(this);
	'	}
	'
	'	final <CompactNode()><Generics()> thisNew = copyAndSetNode(mutator, nodeIndex(bitpos), <nestedResult>.getNode());
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
	'	final <CompactNode()><Generics()> thisNew = copyAndInsertValue(mutator, bitpos, key<if (ds == \map()) {>, val<}>);
	'
	'	return Result.modified(thisNew);
	'}";		
		
str generate_bodyOf_GenericNode_removed(_, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when onlyEqualityDefault && !(eq == equalityDefault)
	;			
		
default str generate_bodyOf_GenericNode_removed(int n, int m, str(str, str) eq) =
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	final int bitpos = (1 \<\< mask);

	if ((valmap & bitpos) != 0) { // inplace value
		final int valIndex = valIndex(bitpos);

		if (<eq("nodes[valIndex]", keyName)>) {			
			if (!USE_SPECIALIAZIONS && this.payloadArity() == 2 && this.nodeArity() == 0) {
				/*
				 * Create new node with remaining pair. The new node
				 * will a) either become the new root returned, or b)
				 * unwrapped and inlined during returning.
				 */
				final <CompactNode()><Generics()> thisNew;
				final int newValmap = (shift == 0) ? this.valmap & ~bitpos
								: 1 \<\< (keyHash & BIT_PARTITION_MASK);

				if (valIndex == 0) {
					thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, newValmap,
									newValmap, new Object[] { nodes[2], nodes[3] },
									(byte) (1));
				} else {
					thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, newValmap,
									newValmap, new Object[] { nodes[0], nodes[1] },
									(byte) (1));
				}

				return Result.modified(thisNew);
			} else if (USE_SPECIALIAZIONS && this.arity() == <nBound + 1>) {
				final Object[] editableNodes = copyAndRemove<if (ds == \map()) {>Pair<}>(this.nodes, valIndex);
	
				final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator,
								this.bitmap & ~bitpos, this.valmap & ~bitpos, editableNodes,
								(byte) (payloadArity - 1));
	
				return Result.modified(thisNew.convertToGenericNode());
			} else {
				final Object[] editableNodes = copyAndRemove<if (ds == \map()) {>Pair<}>(this.nodes, valIndex);
	
				final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator,
								this.bitmap & ~bitpos, this.valmap & ~bitpos, editableNodes,
								(byte) (payloadArity - 1));
	
				return Result.modified(thisNew);
			}
		} else {		
			return Result.unchanged(this);
		}
	} else if ((bitmap & bitpos) != 0) { // node (not value)
		final int bitIndex = bitIndex(bitpos);
		final <CompactNode()><Generics()> subNode = (<CompactNode()><Generics()>) nodes[bitIndex];
		final Result<ResultGenerics> <nestedResult> = subNode.removed(
						mutator, key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);

		if (!<nestedResult>.isModified()) {
			return Result.unchanged(this);
		}

		final <CompactNode()><Generics()> subNodeNew = <nestedResult>.getNode();

		switch (subNodeNew.sizePredicate()) {
		case 0: {
			if (!USE_SPECIALIAZIONS && this.payloadArity() == 0 && this.nodeArity() == 1) {
				// escalate (singleton or empty) result
				return <nestedResult>;
			} else if (USE_SPECIALIAZIONS && this.arity() == <nBound + 1>) {
				// remove node
				final Object[] editableNodes = copyAndRemove<if (ds == \map()) {>Pair<}>(this.nodes, bitIndex);

				final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator,
								bitmap & ~bitpos, valmap, editableNodes, payloadArity);

				return Result.modified(thisNew.convertToGenericNode());
			} else {
				// remove node
				final Object[] editableNodes = copyAndRemove<if (ds == \map()) {>Pair<}>(this.nodes, bitIndex);

				final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator,
								bitmap & ~bitpos, valmap, editableNodes, payloadArity);

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
	
				final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, bitmap,
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

				final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator,
								bitmap, valmap, editableNodes, payloadArity);

				return Result.modified(thisNew);
			}
		}
		}		
	}

	return Result.unchanged(this);";


str removed_value_block(set[Option] setup:{_*, useSpecialization()}) =
	"if (this.arity() == <nBound + 1>) {
	'	final <CompactNode()><Generics()> thisNew = copyAndRemoveValue(mutator, bitpos).convertToGenericNode();
	'
	'	return Result.modified(thisNew);
	'}";
	
default str removed_value_block(set[Option] setup) =
	"if (this.payloadArity() == 2 && this.nodeArity() == 0) {
	'	/*
	'	 * Create new node with remaining pair. The new node
	'	 * will a) either become the new root returned, or b)
	'	 * unwrapped and inlined during returning.
	'	 */
	'	final <CompactNode()><Generics()> thisNew;
	'	final int newValmap = (shift == 0) ? this.valmap & ~bitpos
	'					: 1 \<\< (keyHash & BIT_PARTITION_MASK);
	'
	'	if (valIndex == 0) {
	'		thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, newValmap,
	'						newValmap, getKey(1), getValue(1));
	'	} else {
	'		thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, newValmap,
	'						newValmap, getKey(0), getValue(0));
	'	}
	'
	'	return Result.modified(thisNew);
	'}";	
	
str generate_bodyOf_SpecializedBitmapPositionNode_removed(_, _, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when onlyEqualityDefault && !(eq == equalityDefault)
	;			
		
str removed_in_subnode_with_newsize0_block(set[Option] setup:{_*, useSpecialization()}) = 
	"if (this.arity() == <nBound + 1>) {
	'	// remove node and convert
	'	final <CompactNode()><Generics()> thisNew = copyAndRemoveNode(mutator, bitpos).convertToGenericNode();
	'
	'	return Result.modified(thisNew);
	'}";

default str removed_in_subnode_with_newsize0_block(set[Option] setup) = 
	"if (this.payloadArity() == 0 && this.nodeArity() == 1) {
	'	// escalate (singleton or empty) result
	'	return <nestedResult>;
	'}";
		
default str generate_bodyOf_SpecializedBitmapPositionNode_removed(int n, int m, set[Option] setup, str(str, str) eq) =
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	final int bitpos = (1 \<\< mask);

	if ((valmap & bitpos) != 0) { // inplace value
		final int valIndex = valIndex(bitpos);

		if (<eq("getKey(valIndex)", keyName)>) {			
			<removed_value_block(setup)> else {
				final <CompactNode()><Generics()> thisNew = copyAndRemoveValue(mutator, bitpos);
	
				return Result.modified(thisNew);
			}
		} else {		
			return Result.unchanged(this);
		}
	} else if ((bitmap & bitpos) != 0) { // node (not value)
		final <CompactNode()><Generics()> subNode = getNode(nodeIndex(bitpos));
		final Result<ResultGenerics> <nestedResult> = subNode.removed(
						mutator, key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);

		if (!<nestedResult>.isModified()) {
			return Result.unchanged(this);
		}

		final <CompactNode()><Generics()> subNodeNew = <nestedResult>.getNode();

		switch (subNodeNew.sizePredicate()) {
		case 0: {
			<removed_in_subnode_with_newsize0_block(setup)> else {
				// remove node
				final <CompactNode()><Generics()> thisNew = copyAndRemoveNode(mutator, bitpos);

				return Result.modified(thisNew);
			}
		}
		case 1: {
			<if ({_*, useSpecialization()} := setup) {>// inline value (move to front)
				final <CompactNode()><Generics()> thisNew = copyAndMigrateFromNodeToInline(mutator, bitpos, subNodeNew);
	
				return Result.modified(thisNew);<} else {>if (this.payloadArity() == 0 && this.nodeArity() == 1) {
				// escalate (singleton or empty) result
				return <nestedResult>;
			} else {
				// inline value (move to front)
				final <CompactNode()><Generics()> thisNew = copyAndMigrateFromNodeToInline(mutator, bitpos, subNodeNew);
	
				return Result.modified(thisNew);
			}<}>
		}
		default: {
			// modify current node (set replacement node)
			final <CompactNode()><Generics()> thisNew = copyAndSetNode(mutator, bitpos, subNodeNew);
	
			return Result.modified(thisNew);
		}
		}		
	}

	return Result.unchanged(this);";	

list[Argument] generateMembers(int n, int m) 
	= [ *payloadTriple(i) | i <- [1..m+1]] 
	+ [ *subnodePair(i)   | i <- [1..n+1]]
	;

list[Argument] generatePayloadMembers(int m) 
	= [ *payloadTriple(i) | i <- [1..m+1]] 
	;

list[Argument] generateSubnodeMembers(int n) 
	= [ *subnodePair(i)   | i <- [1..n+1]]
	;	

list[Argument] generateMembers_bitmap(int n, int m) 
	= [ bitmap, valmap ]
	+ [ key(i), val(i) | i <- [1..m+1]] 
	+ [ \node(i)	   | i <- [1..n+1]]
	;

//str generate_valNodeOf_factoryMethod_special(int n, int m) {
//	return 
//	"static final <Generics()> <CompactNode()><Generics()> valNodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {
//	'	final int bitmap = 0 <intercalate(" ", mapper(bitmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
//	'	final int valmap = 0 <intercalate(" ", mapper(valmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
//	'	final Object[] content = new Object[<2*m + n>];
//	'
//	'	final java.util.SortedMap\<Byte, Map.Entry<Generics()>\> sortedPayloadMasks = new java.util.TreeMap\<\>();
//	'	<for (i <- [1..m+1]) {>
//	'	sortedPayloadMasks.put(<use(keyPos(i))>, entryOf(<use(key(i))>, <use(val(i))>));
//	'	<}>
//	'	
//	'	final java.util.SortedMap\<Byte, <CompactNode()><Generics()>\> sortedSubnodeMasks = new java.util.TreeMap\<\>();
//	'	<for (i <- [1..n+1]) {>
//	'	sortedSubnodeMasks.put(<use(nodePos(i))>, <use(\node(i))>);
//	'	<}>
//	'
//	'	int index = 0;			
//	'	for (Map.Entry\<Byte, Map.Entry<Generics()>\> entry : sortedPayloadMasks.entrySet()) {
//	'		content[index++] = entry.getValue().getKey();
//	'		content[index++] = entry.getValue().getValue();
//	'	}
//	'
//	'	for (Map.Entry\<Byte, CompactMapNode<Generics()>\> entry : sortedSubnodeMasks.entrySet()) {
//	'		content[index++] = entry.getValue();
//	'	}			
//	'			
//	'	return valNodeOf(mutator, bitmap, valmap, content, (byte) <m>);			
//	'}
//	";
//}


str generate_valNodeOf_factoryMethod(0, 0) { throw "TODO"; }
		
str generate_valNodeOf_factoryMethod(1, 0) { throw "TODO"; }

str generate_valNodeOf_factoryMethod(int n, int m) {
	// TODO: remove code duplication
	members = generateMembers(n, m);
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	className = "<toString(ds)><m>To<n>Node";

	if ((n + m) <= nBound) {		
		return
		"static final <Generics()> <CompactNode()><Generics()> valNodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
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
			argsForArray = [ key(i), val(i) | i <- [1..m+1]] + [ \node(j) | j <- [1..n+1]];
		} else { 
			argsForArray = [ key(i) | i <- [1..m+1]] + [ \node(j) | j <- [1..n+1]];
		}
		
		if (sortedContent) {			
			return
			"static final <Generics()> <CompactNode()><Generics()> valNodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
			'	final int bitmap = 0 <intercalate(" ", mapper(bitmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
			'	final int valmap = 0 <intercalate(" ", mapper(valmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
			'
			'	return valNodeOf(mutator, bitmap, valmap, new Object[] { <use(argsForArray)> }, (byte) <m>);
			'}
			";
		} else {				
			return 
			"static final <Generics()> <CompactNode()><Generics()> valNodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {
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
			'	return valNodeOf(mutator, bitmap, valmap, content, (byte) <m>);		
			'}
			";			
		}
	} else {
		throw "Arguments out of bounds.";
	}
}		

// bit-shifting with signed integers in Java
int oneShiftedLeftBy(int count) = toInt(pow(2, count)) when count >= 0 && count <= 30;
int oneShiftedLeftBy(31) = -2147483648;
default int oneShiftedLeftBy(int count) { throw "Not supported!"; }

str generate_valNodeOf_factoryMethod_bitmap(0, 0) { 
	// TODO: remove code duplication
	members = generateMembers_bitmap(0, 0);
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	return
	"static final <Generics()> <CompactNode()><Generics()> valNodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
	'	return EMPTY_INPLACE_INDEX_NODE;
	'}"
	;
}

str generate_valNodeOf_factoryMethod_bitmap(n:1, m:0, set[Option] setup:{_*, compactionViaFieldToMethod()}) {
	// TODO: remove code duplication
	members = generateMembers_bitmap(n, m);
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	className = "<toString(ds)><m>To<n>Node";

	return
	"static final <Generics()> <CompactNode()><Generics()> valNodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
	'	switch(bitmap) {
	'	<for (i <- [1..nMax+1]) {>case <oneShiftedLeftBy(i-1)>:
	'		return new <toString(ds)><m>To<n>NodeAtMask<i-1>\<\>(<intercalate(", ", mapper(constructorArgs, use))>);
	'	<}>default:
	'		throw new IllegalStateException(\"Index out of range.\");
	'	}
	'}"
	;
}

default str generate_valNodeOf_factoryMethod_bitmap(int n, int m, set[Option] setup) {
	// TODO: remove code duplication
	members = generateMembers_bitmap(n, m);
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	className = "<toString(ds)><m>To<n>Node";

	if ((n + m) <= nBound) {		
		return
		"static final <Generics()> <CompactNode()><Generics()> valNodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
		'	return new <className>\<\>(<intercalate(", ", mapper(constructorArgs, use))>);
		'}
		"; 
	} else if ((n + m) == nBound + 1 && (n + m) < nMax) {
		list[Argument] argsForArray = generateMembers_bitmap(n, m) - [ field("int", "bitmap"), field("int", "valmap") ];

		return
		"static final <Generics()> <CompactNode()><Generics()> valNodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
		'	return valNodeOf(mutator, bitmap, valmap, new Object[] { <use(argsForArray)> }, (byte) <m>);
		'}
		";
	} else {
		throw "Arguments out of bounds.";
	}
}
	
str generateSpecializedNodeWithBytePositionsClassString(int n, int m) {
	members = generateMembers(n, m);
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	className = "<toString(ds)><m>To<n>Node";

	return
	"private static final class <className><Generics()> extends Compact<toString(ds)>Node<Generics()> {
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

	<if (sortedContent) {>
	'	<if ((n + m) > 0) {>
	'	private <CompactNode()><Generics()> inlineValue(AtomicReference\<Thread\> mutator, <dec(payloadTriple("mask"))>) {
	'		<generate_bodyOf_inlineValue(n, m)>
	'	}
	'	<}>
	<}>
	
	<if (sortedContent) {>
	'	<for (j <- [1..n+1]) {>
	'	private <CompactNode()><Generics()> removeNode<j>AndInlineValue(AtomicReference\<Thread\> mutator, <dec(payloadTriple("mask"))>) {
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
	'	Iterator\<<CompactNode()><Generics()>\> nodeIterator() {
	'		<if (n > 0) {>return ArrayIterator.\<<CompactNode()><Generics()>\> of(new <CompactNode()>[] { <intercalate(", ", ["<nodeName><i>" | i <- [1..n+1]])> });<} else {>return Collections.emptyIterator();<}>
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
	'	SupplierIterator<SupplierIteratorGenerics> payloadIterator() {
	'		<if (m > 0) {>return ArrayKeyValueIterator.of(new Object[] { <intercalate(", ", ["<keyName><i>, <valName><i>"  | i <- [1..m+1]])> });<} else {>return EmptySupplierIterator.emptyIterator();<}>
	'	}
	<} else {>
	'	@Override
	'	SupplierIterator<SupplierIteratorGenerics> payloadIterator() {
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
	'	K headKey() {
	'		<if (m == 0) {>throw new UnsupportedOperationException(\"Node does not directly contain a key.\")<} else {>return key1<}>;
	'	}

	<if (ds == \map()) {>
	'	@Override
	'	V headVal() {
	'		<if (m == 0) {>throw new UnsupportedOperationException(\"Node does not directly contain a value.\")<} else {>return val1<}>;
	'	}	
	<}>
	
	'	@Override
	'	<CompactNode()><Generics()> getNode(int index) {
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
	'	Map.Entry<Generics()> getKeyValueEntry(int index) {
	'		<generate_bodyOf_getKeyValueEntry(m)>
	'	}
	<}>	
	
	'	@Override
	'	byte sizePredicate() {
	'		return <generate_bodyOf_sizePredicate(n, m)>;
	'	}

	<if (sortedContent) {>
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
	'		<if ((n + m) > 0) {><className><QuestionMarkGenerics> that = (<className><QuestionMarkGenerics>) other;
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

str generate_bodyOf_sizePredicate(0, 0) = "SIZE_EMPTY";
str generate_bodyOf_sizePredicate(0, 1) = "SIZE_ONE";	
default str generate_bodyOf_sizePredicate(int n, int m) = "SIZE_MORE_THAN_ONE";


str generate_equalityComparisons(int n, int m, str(str, str) eq) =
	"if (bitmap != that.bitmap) {
	'	return false;
	'}
	'if (valmap != that.valmap) {
	'	return false;
	'}
	'<for (i <- [1..m+1]) {>
	'if (!<eq("<keyName><i>", "that.<keyName><i>")>) {
	'	return false;
	'}
	'<if (ds == \map()) {>if (!<eq("<valName><i>", "that.<valName><i>")>) {
	'	return false;
	'}<}><}><for (i <- [1..n+1]) {>
	'if (!<eq("<nodeName><i>", "that.<nodeName><i>")>) {
	'	return false;
	'}<}>"
	;
	 

str generate_bodyOf_inlineValue(int n, int m) =
	"return <nodeOf(n, m+1, use(payloadTriple("mask") + generateSubnodeMembers(n)))>;"
when m == 0;

default str generate_bodyOf_inlineValue(int n, int m) =
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

str generateSpecializedNodeWithBitmapPositionsClassString(int n, int m) {
	members = generateMembers_bitmap(n, m);
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	className = "<toString(ds)><m>To<n>Node";

	return
	"private static final class <className><Generics()> extends Compact<toString(ds)>Node<Generics()> {
	'	<intercalate("\n", mapper(members, str(Argument a) { 
			str dec = "private final <dec(a)>;";
			
			if (field(_, /.*pos.*/) := a || getter(_, /.*pos.*/) := a) {
				return "\n<dec>";
			} else {
				return dec;
			} 
		}))>
				
	'	<className>(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
	'		super(mutator, bitmap, valmap);
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
	'	Iterator\<<CompactNode()><Generics()>\> nodeIterator() {
	'		<if (n > 0) {>return ArrayIterator.of(<intercalate(", ", ["<nodeName><i>" | i <- [1..n+1]])>);<} else {>return Collections.\<<CompactNode()><Generics()>\>emptyIterator();<}>
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
	'	SupplierIterator<SupplierIteratorGenerics> payloadIterator() {
	'		<if (m > 0) {>return ArrayKeyValueIterator.of(new Object[] { <intercalate(", ", ["<keyName><i>, <valName><i>"  | i <- [1..m+1]])> });<} else {>return EmptySupplierIterator.emptyIterator();<}>
	'	}
	<} else {>
	'	@Override
	'	SupplierIterator<SupplierIteratorGenerics> payloadIterator() {
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
	'	K headKey() {
	'		<if (m == 0) {>throw new UnsupportedOperationException(\"Node does not directly contain a key.\")<} else {>return key1<}>;
	'	}

	<if (ds == \map()) {>
	'	@Override
	'	V headVal() {
	'		<if (m == 0) {>throw new UnsupportedOperationException(\"Node does not directly contain a value.\")<} else {>return val1<}>;
	'	}	
	<}>
	
	'	@Override
	'	<CompactNode()><Generics()> getNode(int index) {
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
	'	Map.Entry<Generics()> getKeyValueEntry(int index) {
	'		<generate_bodyOf_getKeyValueEntry(m)>
	'	}
	<}>		

	<if (ds == \map()) {>
	'	@Override
	'	<CompactNode()><Generics()> copyAndSetValue(AtomicReference\<Thread\> mutator, int index, V <valName>) {
	'		<generate_bodyOf_copyAndSetValue(n, m)>
	'	}
	<}>	
	
	'	@Override
	'	<CompactNode()><Generics()> copyAndInsertValue(AtomicReference\<Thread\> mutator, int bitpos, K <keyName>, V <valName>) {		
	'		<generate_bodyOf_copyAndInsertValue(n, m)>
	'	}
	
	'	@Override
	'	<CompactNode()><Generics()> copyAndRemoveValue(AtomicReference\<Thread\> mutator, int bitpos) {
	'		<generate_bodyOf_copyAndRemoveValue(n, m)>
	'	}	

	'	@Override
	'	<CompactNode()><Generics()> copyAndSetNode(AtomicReference\<Thread\> mutator, int index, <CompactNode()><Generics()> <nodeName>) {
	'		<generate_bodyOf_copyAndSetNode(n, m)>
	'	}	

	'	@Override
	'	<CompactNode()><Generics()> copyAndRemoveNode(AtomicReference\<Thread\> mutator, int bitpos) {
	'		<generate_bodyOf_copyAndRemoveNode(n, m)>
	'	}	

	'	@Override
	'	<CompactNode()><Generics()> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator, int bitpos, <CompactNode()><Generics()> <nodeName>) {
	'		<generate_bodyOf_copyAndMigrateFromInlineToNode(n, m)>
	'	}
	
	'	@Override
	'	<CompactNode()><Generics()> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator, int bitpos, <CompactNode()><Generics()> <nodeName>) {
	'		<generate_bodyOf_copyAndMigrateFromNodeToInline(n, m)>
	'	}		
	
	'	@Override
	'	<CompactNode()><Generics()> convertToGenericNode() {
	'		return valNodeOf(<use(thisMutator)>, bitmap, valmap, new Object[] { <use(generateMembers_bitmap(n, m) - [ field("int", "bitmap"), field("int", "valmap") ])> }, (byte) <m>);
	'	}	
	
	'	@Override
	'	byte sizePredicate() {
	'		return <generate_bodyOf_sizePredicate(n, m)>;
	'	}

	<if (sortedContent) {>
	'	@Override
	'	public int hashCode() {
	'		<if ((n + m) > 0) {>final int prime = 31; int result = 1; result = prime * result + bitmap; result = prime * result + valmap;<} else {>int result = 1;<}>	
	'		<for (i <- [1..m+1]) {>		
	'		result = prime * result + <keyName><i>.hashCode();
	'		<if (ds == \map()) {>result = prime * result + <valName><i>.hashCode();<}>
	'		<}><for (i <- [1..n+1]) {>
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
	'		<if ((n + m) > 0) {><className><QuestionMarkGenerics> that = (<className><QuestionMarkGenerics>) other;
	'
	'		<generate_equalityComparisons(n, m, equalityDefault)><}>
	'
	'		return true;
	'	}
	<}>	
	

	'	@Override
	'	public String toString() {		
	'		<if (n == 0 && m == 0) {>return \"[]\";<} else {>return String.format(\"[<intercalate(", ", [ "@%d: %s<if (ds == \map()) {>=%s<}>" | i <- [1..m+1] ] + [ "@%d: %s" | i <- [1..n+1] ])>]\", <use([ field("recoverMask(valmap, (byte) <i>)"), key(i), val(i) | i <- [1..m+1]] + [ field("recoverMask(bitmap ^ valmap, (byte) <i>)"), \node(i)	| i <- [1..n+1]])>);<}>
	'	}
	
	'}
	"
	;	
	
}

str generate_bodyOf_mergeTwoValues(set[Option] setup:{_*, useSpecialization()}, Position pos:positionField()) =
	"if (mask0 \< mask1) {
	'	return valNodeOf(null, (byte) mask0, key0, val0, (byte) mask1, key1, val1);
	'} else {
	'	return valNodeOf(null, (byte) mask1, key1, val1, (byte) mask0, key0, val0);
	'}";

str generate_bodyOf_mergeTwoValues(set[Option] setup:{_*, useSpecialization()}, Position pos:positionBitmap()) =
	"final int valmap = 1 \<\< mask0 | 1 \<\< mask1;
	'
	'if (mask0 \< mask1) {
	'	return valNodeOf(null, valmap, valmap, key0, val0, key1, val1);
	'} else {
	'	return valNodeOf(null, valmap, valmap, key1, val1, key0, val0);
	'}";	
	
str generate_bodyOf_mergeTwoValues(set[Option] setup, Position _) =
	"final int valmap = 1 \<\< mask0 | 1 \<\< mask1;
	'
	'if (mask0 \< mask1) {
	'	return valNodeOf(null, valmap, valmap, new Object[] { key0, val0, key1, val1 }, (byte) 2);
	'} else {
	'	return valNodeOf(null, valmap, valmap, new Object[] { key1, val1, key0, val0 }, (byte) 2);
	'}"
when !(useSpecialization() in setup);	

default str generate_bodyOf_mergeTwoValues(Option _, Position _) { throw "something went wrong"; }

str generate_bodyOf_mergeOnNextLevel(set[Option] setup:{_*, useSpecialization()}, Position pos:positionField()) =
	"return valNodeOf(null, (byte) mask0, node);";

str generate_bodyOf_mergeOnNextLevel(set[Option] setup:{_*, useSpecialization()}, Position pos:positionBitmap()) =
	"final int bitmap = 1 \<\< mask0;
	'return valNodeOf(null, bitmap, 0, node);";		
	
str generate_bodyOf_mergeOnNextLevel(set[Option] setup, Position _) =
	"final int bitmap = 1 \<\< mask0;
	'return valNodeOf(null, bitmap, 0, new Object[] { node }, (byte) 0);"
when !(useSpecialization() in setup);	

default str generate_bodyOf_mergeOnNextLevel(Option _, Position _) { throw "something went wrong"; }

str generate_bodyOf_mergeNodeAndValue(set[Option] setup:{_*, useSpecialization()}, Position pos:positionField()) =
	"// store values before node
	'return valNodeOf(null, (byte) mask1, key1, val1, (byte) mask0, node0);";

str generate_bodyOf_mergeNodeAndValue(set[Option] setup:{_*, useSpecialization()}, Position pos:positionBitmap()) =
	"final int bitmap = 1 \<\< mask0 | 1 \<\< mask1;
	'final int valmap = 1 \<\< mask1;
	'
	'// store values before node
	'return valNodeOf(null, bitmap, valmap, key1, val1, node0);";		
	
str generate_bodyOf_mergeNodeAndValue(set[Option] setup, Position _) =
	"final int bitmap = 1 \<\< mask0 | 1 \<\< mask1;
	'final int valmap = 1 \<\< mask1;
	'
	'// store values before node
	'return valNodeOf(null, bitmap, valmap, new Object[] { key1, val1, node0 }, (byte) 1);"
when !(useSpecialization() in setup);			

default str generate_bodyOf_mergeNodeAndValue(Option _, Position _) { throw "something went wrong"; }

str generateCompactNodeClassString(int n=0, int m=0, set[Option] setup = {}) {
	members = [ field("int", "bitmap"), field("int", "valmap") ];
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	className = "Compact<toString(ds)>Node";

	Position positionStyle = positionBitmap();

//		@Override
//		abstract Result<ResultGenerics> updated(AtomicReference\<Thread\> mutator, K key, int keyHash, V val, int shift);
//
//		@Override
//		abstract Result<ResultGenerics> updated(AtomicReference\<Thread\> mutator, K key, int keyHash, V val, int shift, Comparator\<Object\> cmp);
//
//		@Override
//		abstract Result<ResultGenerics> removed(AtomicReference\<Thread\> mutator, K key, int hash, int shift);
//
//		@Override
//		abstract Result<ResultGenerics> removed(AtomicReference\<Thread\> mutator, K key, int hash, int shift, Comparator\<Object\> cmp);

	return
	"private static abstract class <className><Generics()> extends Abstract<toString(ds)>Node<Generics()> {
	'
	'	protected static final int BIT_PARTITION_SIZE = 5;
	'	protected static final int BIT_PARTITION_MASK = 0x1f;
	'
	'	<intercalate("\n", mapper(members, str(Argument a) { 
			str dec = "protected final <dec(a)>;";
			
			if (field(_, /.*pos.*/) := a || getter(_, /.*pos.*/) := a) {
				return "\n<dec>";
			} else {
				return dec;
			} 
		}))>
				
	'	<className>(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
	'		super();
	'		<intercalate("\n", mapper(members, str(Argument a) { 
				str dec = "this.<use(a)> = <use(a)>;";
				
				if (field(_, /.*pos.*/) := a || getter(_, /.*pos.*/) := a) {
					return "\n<dec>";
				} else {
					return dec;
				} 
			}))>
	'	}

		static final byte SIZE_EMPTY = 0b00;
		static final byte SIZE_ONE = 0b01;
		static final byte SIZE_MORE_THAN_ONE = 0b10;

		abstract <CompactNode()><Generics()> convertToGenericNode();

		/**
		 * Abstract predicate over a node\'s size. Value can be either
		 * {@value #SIZE_EMPTY}, {@value #SIZE_ONE}, or
		 * {@value #SIZE_MORE_THAN_ONE}.
		 * 
		 * @return size predicate
		 */
		abstract byte sizePredicate();

		/**
		 * Returns the first key stored within this node.
		 * 
		 * @return first key
		 */
		abstract K headKey();

		/**
		 * Returns the first value stored within this node.
		 * 
		 * @return first value
		 */
		abstract V headVal();

		@Override
		abstract <CompactNode()><Generics()> getNode(int index);

		@Override
		abstract Iterator\<? extends <CompactNode()><Generics()>\> nodeIterator();

		boolean nodeInvariant() {
			boolean inv1 = (size() - payloadArity() \>= 2 * (arity() - payloadArity()));
			boolean inv2 = (this.arity() == 0) ? sizePredicate() == SIZE_EMPTY : true;
			boolean inv3 = (this.arity() == 1 && payloadArity() == 1) ? sizePredicate() == SIZE_ONE
							: true;
			boolean inv4 = (this.arity() \>= 2) ? sizePredicate() == SIZE_MORE_THAN_ONE : true;

			boolean inv5 = (this.nodeArity() \>= 0) && (this.payloadArity() \>= 0)
							&& ((this.payloadArity() + this.nodeArity()) == this.arity());

			return inv1 && inv2 && inv3 && inv4 && inv5;
		}

	<if (ds == \map()) {>
	'	abstract <CompactNode()><Generics()> copyAndSetValue(AtomicReference\<Thread\> mutator, int index, V <valName>);
	<}>	
	
	'	abstract <CompactNode()><Generics()> copyAndInsertValue(AtomicReference\<Thread\> mutator, int bitpos, K <keyName>, V <valName>);
	
	'	abstract <CompactNode()><Generics()> copyAndRemoveValue(AtomicReference\<Thread\> mutator, int bitpos);

	'	abstract <CompactNode()><Generics()> copyAndSetNode(AtomicReference\<Thread\> mutator, int index, <CompactNode()><Generics()> <nodeName>);

	'	abstract <CompactNode()><Generics()> copyAndRemoveNode(AtomicReference\<Thread\> mutator, int bitpos);

	'	abstract <CompactNode()><Generics()> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator, int bitpos, <CompactNode()><Generics()> <nodeName>);
	
	'	abstract <CompactNode()><Generics()> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator, int bitpos, <CompactNode()><Generics()> <nodeName>);

		@SuppressWarnings(\"unchecked\")
		static final <Generics()> <CompactNode()><Generics()> mergeNodes(K key0, int keyHash0, V val0, K key1,
						int keyHash1, V val1, int shift) {
			assert key0.equals(key1) == false;

			if (keyHash0 == keyHash1) {
				return new HashCollisionMapNode\<\>(keyHash0, (K[]) new Object[] { key0, key1 },
								(V[]) new Object[] { val0, val1 });
			}

			final int mask0 = (keyHash0 \>\>\> shift) & BIT_PARTITION_MASK;
			final int mask1 = (keyHash1 \>\>\> shift) & BIT_PARTITION_MASK;

			if (mask0 != mask1) {
				// both nodes fit on same level
				<generate_bodyOf_mergeTwoValues(setup, positionStyle)>
			} else {
				// values fit on next level
				final <CompactNode()><Generics()> node = mergeNodes(key0, keyHash0, val0, key1, keyHash1,
								val1, shift + BIT_PARTITION_SIZE);

				<generate_bodyOf_mergeOnNextLevel(setup, positionStyle)>
			}
		}

		static final <Generics()> <CompactNode()><Generics()> mergeNodes(<CompactNode()><Generics()> node0,
						int keyHash0, K key1, int keyHash1, V val1, int shift) {
			final int mask0 = (keyHash0 \>\>\> shift) & BIT_PARTITION_MASK;
			final int mask1 = (keyHash1 \>\>\> shift) & BIT_PARTITION_MASK;

			if (mask0 != mask1) {
				// both nodes fit on same level
				<generate_bodyOf_mergeNodeAndValue(setup, positionStyle)>
			} else {
				// values fit on next level
				final <CompactNode()><Generics()> node = mergeNodes(node0, keyHash0, key1, keyHash1, val1,
								shift + BIT_PARTITION_SIZE);

				<generate_bodyOf_mergeOnNextLevel(setup, positionStyle)>
			}
		}	

	'	static final CompactMapNode EMPTY_INPLACE_INDEX_NODE;

	'	static {
	'		<if ({_*, useSpecialization()} := setup) {>EMPTY_INPLACE_INDEX_NODE = new Map0To0Node\<\>(null, 0, 0);<} else {>EMPTY_INPLACE_INDEX_NODE = new BitmapIndexedMapNode\<\>(null, 0, 0, new Object[] {}, (byte) 0);<}>	
	'	};
	
	'	static final <Generics()> <CompactNode()><Generics()> valNodeOf(AtomicReference\<Thread\> mutator,
	'					int bitmap, int valmap, Object[] nodes, byte payloadArity) {
	'		return new BitmapIndexedMapNode\<\>(mutator, bitmap, valmap, nodes, payloadArity);
	'	}	

	'	// TODO: consolidate and remove
	'	static final <Generics()> <CompactNode()><Generics()> valNodeOf(AtomicReference\<Thread\> mutator) {
	'		return valNodeOf(mutator, 0, 0);
	'	}

	<if ({_*, useSpecialization()} := setup) {>
		<for(j <- [0..nMax+1], i <- [0..nMax+1], (i + j) <= nBound + 1 && !(i == nBound + 1)) {>
			<generate_valNodeOf_factoryMethod_bitmap(i, j, setup)>
		<}>
	<}>	

	'	final int keyIndex(int bitpos) {
	'		return Integer.bitCount(valmap & (bitpos - 1));
	'	}

	'	final int valIndex(int bitpos) {
	'		return Integer.bitCount(valmap & (bitpos - 1));
	'	}

	'	// TODO: obviate necessity for bitmap ^ valmap
	'	final int nodeIndex(int bitpos) {
	'		return Integer.bitCount((bitmap ^ valmap) & (bitpos - 1));
	'	}

	'	@Override
	'	boolean containsKey(Object key, int keyHash, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_containsKey(n, m, equalityDefault)>
	'	}

	'	@Override
	'	boolean containsKey(Object key, int keyHash, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_containsKey(n, m, equalityComparator)>
	'	}

	'	@Override
	'	Optional<KeyOrMapEntryGenerics> findByKey(Object key, int keyHash, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_findByKey(n, m, equalityDefault)>
	'	}

	'	@Override
	'	Optional<KeyOrMapEntryGenerics> findByKey(Object key, int keyHash, int shift,
	'					Comparator\<Object\> cmp) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_findByKey(n, m, equalityComparator)>
	'	}

	'	@Override
	'	Result<ResultGenerics> updated(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, V<if (ds == \set()) {>oid<}> val, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_updated(n, m, equalityDefault)>
	'	}

	'	@Override
	'	Result<ResultGenerics> updated(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, V<if (ds == \set()) {>oid<}> val, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_updated(n, m, equalityComparator)>
	'	}

	'	@Override
	'	Result<ResultGenerics> removed(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_removed(n, m, setup, equalityDefault)>
	'	}

	'	@Override
	'	Result<ResultGenerics> removed(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_removed(n, m, setup, equalityComparator)>
	'	}
	
	'}
	"
	;
}
