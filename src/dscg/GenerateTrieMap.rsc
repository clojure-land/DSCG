/*******************************************************************************
 * Copyright (c) 2014 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 *******************************************************************************/
module dscg::GenerateTrieMap

import IO;
import List;

import dscg::Common;
import dscg::GenerateImmutableMap;

/* 
 * Configuration 
 */
str nodeName = "node";
str nodePosName = "npos";
int nMax = 32;

str nestedResult = "nestedResult";

str keyPosName = "pos";

void main() {
	//classStrings = [ generateClassString(n) | n <- [0..6] ];
	classStrings = 
		//generateCompactNodeString() + 
		//generateLeafNodeString() + 
		//[ generateGenericNodeClassString(0, 0)] +		
		[ generateSpecializedMixedNodeClassString(n, m) | m <- [0..33], n <- [0..33], (n + m) <= 4 && !((n == 1) && (m == 0)) ];  
	writeFile(|project://DSCG/gen/org/eclipse/imp/pdb/facts/util/AbstractSpecialisedTrieMap.java|, classStrings);
}

str(str,str) getStuff() = equalityDefault;
	
str generateClassString(int n) =  
	"class Map<n>\<K, V\> extends AbstractSpecialisedImmutableMap\<K, V\> {
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
	'	public Set\<Entry\<K, V\>\> entrySet() {
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
	'	public SupplierIterator\<K, V\> keyIterator() {
	'		<generate_bodyOf_keyIterator(n)>
	'	}	

	'	@Override
	'	public ImmutableMap\<K, V\> __put(K <keyName>, V <valName>) {
	'		<generate_bodyOf_put(n, equalityDefault)>
	'	}
	
	'	@Override
	'	public ImmutableMap\<K, V\> __putEquivalent(K <keyName>, V <valName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_put(n, equalityComparator)>
	'	}	

	'	@Override
	'	public ImmutableMap\<K, V\> __remove(K <keyName>) {
	'		<generate_bodyOf_remove(n, equalityDefault)>	
	'	}

	'	@Override
	'	public ImmutableMap\<K, V\> __removeEquivalent(K <keyName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_remove(n, equalityComparator)>
	'	}
	
	'	@Override
	'	public TransientMap\<K, V\> asTransient() {
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
		
// TODO check if used
str posNodeArgsUnmodified(int n) 
	= intercalate(", ", ["<nodePosName><i>, <nodeName><i>"  | i <- [1..n+1]])
	;

// TODO check if used
str posNodeArgsReplaced(int n, int j, str newPos, str newName) 
	= intercalate(", ", ["<if (i == j) {><newPos>, <newName><} else {><nodePosName><i>, <nodeName><i><}>" | i <- [1..n+1]])
	;

// TODO check if used
str posNodeArgsInsertAt(int n, 0, str newPos, str newName) 
	= "<newPos>, <newName>, " + posNodeArgsUnmodified(n)
	;

// TODO check if used	
str posNodeArgsInsertAt(int n, int j, str newPos, str newName) 
	= "<newPos>, <newName>, " + posNodeArgsUnmodified(n)
		when j == n+1;

str posNodeArgsInsertAt(int n, int j, str newPos, str newName) 
	= intercalate(", ", ["<if (i == j) {><newPos>, <newName>, <}><nodePosName><i>, <nodeName><i>" | i <- [1..n+1]])
	;	

str posNodeArgsRemoved(int n, int j) 
	= intercalate(", ", ["<nodePosName><i>, <nodeName><i>" | i <- [1..n+1], i != j])
	;






str posKeyValArgsUnmodified(int m) 
	= intercalate(", ", ["<keyPosName><i>, <keyName><i>, <valName><i>"  | i <- [1..m+1]])
	;

str posNodeArgsUnmodified(int n) 
	= intercalate(", ", ["<nodePosName><i>, <nodeName><i>"  | i <- [1..n+1]])
	;

//str argsInsertKeyValAt(int n, int m, 0, str newPos, str newKeyName, str newValName) 
//	= "<newPos>, <newKeyName>, <newValName>, <posKeyValArgsUnmodified(m)>, <posNodeArgsUnmodified(n)>"
//	;
//	
//str argsInsertKeyValAt(int n, int m, int j, str newPos, str newKeyName, str newValName) 
//	= "<newPos>, <newName>, " + posNodeArgsUnmodified(n)
//		when j == n+1;

// TODO: improve
default str argsInsertKeyValAt(int n, int m, int j, str newPos, str newKeyName, str newValName) 
	= intercalate(", ", 
		["<if (i == j) {><newPos>, <newKeyName>, <newValName>, <}><keyPosName><i>, <keyName><i>, <valName><i>" | i <- [1..m+1]] +
		["<nodePosName><i>, <nodeName><i>" | i <- [1..n+1]])
	;

// TODO: improve	
str argsInsertKeyValAt(int n, int m, int j, str newPos, str newKeyName, str newValName)
	= intercalate(", ", 
		["<keyPosName><i>, <keyName><i>, <valName><i>" | i <- [1..m+1]] +
		["<newPos>, <newKeyName>, <newValName>"] +
		["<nodePosName><i>, <nodeName><i>" | i <- [1..n+1]])
		when j == m+1;
		
// TODO: improve	
str argsInsertKeyValAt(int n, int m, 0, str newPos, str newKeyName, str newValName)
	= intercalate(", ", 
		["<newPos>, <newKeyName>, <newValName>"] +
		["<keyPosName><i>, <keyName><i>, <valName><i>" | i <- [1..m+1]] +
		["<nodePosName><i>, <nodeName><i>" | i <- [1..n+1]]);
			
str argsReplacedKeyVal(int n, int m, int j, str newPos, str newKeyName, str newValName) 
	= intercalate(", ", 
		["<if (i == j) {><newPos>, <newKeyName>, <newValName><} else {><keyPosName><i>, <keyName><i>, <valName><i><}>" | i <- [1..m+1]] +
		["<nodePosName><i>, <nodeName><i>" | i <- [1..n+1]]
		)
	;
	
str argsReplacedNode(int n, int m, int j, str newPos, str newNodeName) 
	= intercalate(", ", 
		["<keyPosName><i>, <keyName><i>, <valName><i>" | i <- [1..m+1]] +
		["<if (i == j) {><newPos>, <newNodeName><} else {><nodePosName><i>, <nodeName><i><}>" | i <- [1..n+1]]
		)
	;

str argsWithoutKeyVal(int n, int m, int j) 
	= intercalate(", ", 
		["<keyPosName><i>, <keyName><i>, <valName><i>" | i <- [1..m+1], i != j] +
		["<nodePosName><i>, <nodeName><i>" | i <- [1..n+1]])
	;
	
str argsWithoutNode(int n, int m, int j) 
	= intercalate(", ", 
		["<keyPosName><i>, <keyName><i>, <valName><i>" | i <- [1..m+1]] +
		["<nodePosName><i>, <nodeName><i>" | i <- [1..n+1], i != j])
	;	
	
str argsUnmodified(int n, int m) 
	= intercalate(", ", 
		["<keyPosName><i>, <keyName><i>, <valName><i>" | i <- [1..m+1]] +
		["<nodePosName><i>, <nodeName><i>" | i <- [1..n+1]])
	;
	
	
	
	
str generate_bodyOf_updated(0, 0, str(str, str) eq) = 
	"final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);
	'return Result.modified(<nodeOf(0, 1, "mask, <keyName>, <valName>")>);"
	;

str generate_bodyOf_updated(int n, int m, str(str, str) eq) {	
	replaceValueByNode = str (int i, int j) { return
		"<intercalate(", ", 
			["<keyPosName><k>, <keyName><k>, <valName><k>" | k <- [1..m+1], k != i] +
			["<if (l == j) {>mask, node, <}><nodePosName><l>, <nodeName><l>" | l <- [1..n+1]])>"; 
	};
	
	replaceValueByNodeAtEnd = str (int i) { return
		"<intercalate(", ", 
			["<keyPosName><k>, <keyName><k>, <valName><k>" | k <- [1..m+1], k != i] +
			["<nodePosName><l>, <nodeName><l>" | l <- [1..n+1]] +
			["mask, node"])>"; 
	};	
		
	updated_clause_inline = str (int i) { return 
		"if (mask == <keyPosName><i>) {
		'	if (<eq("<keyName>", "<keyName><i>")>) {
		'		if (<eq("<valName>", "<valName><i>")>) {
		'			result = Result.unchanged(this);
		'		} else {		
		'			// update <keyName><i>, <valName><i>
		'			result = Result.updated(<nodeOf(n, m, argsReplacedKeyVal(n, m, i, "<keyPosName><i>", "<keyName><i>", valName))>, <valName><i>);
		'		}
		'	} else {
		'		// merge into node
		'		final CompactNode\<K, V\> node = mergeNodes(<keyName><i>, <keyName><i>.hashCode(), <valName><i>, <keyName>, <keyName>Hash, <valName>, shift + BIT_PARTITION_SIZE);
		'		
		'		<if (n == 0) {>result = Result.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);<} else {><intercalate(" else ", [ "if (mask \< <nodePosName><j>) { result = Result.modified(<nodeOf(n+1, m-1, replaceValueByNode(i, j))>); }" | j <- [1..n+1] ])> else {
		'			result = Result.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);
		'		}<}>
		'	}
		'}"; 
	};
	
	updated_clause_node = str (int i) { return 
		"if (mask == <nodePosName><i>) {
		'	final Result\<K, V, ? extends CompactNode\<K, V\>\> <nestedResult> = <nodeName><i>.updated(
		'					mutator, key, keyHash, val, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
		'
		'	if (<nestedResult>.isModified()) {
		'		final CompactNode\<K, V\> thisNew = <nodeOf(n, m, argsReplacedNode(n, m, i, "mask", "<nestedResult>.getNode()"))>;
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
	};
	
	return 
	"final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);
	'final Result\<K, V, ? extends CompactNode\<K, V\>\> result;		
	'		
	'<intercalate(" else ", [ updated_clause_inline(i)| i <- [1..m+1]] + [ updated_clause_node(i)| i <- [1..n+1]])> else {
	'	// no value
	'	result = Result.modified(inlineValue(mutator, mask, <keyName>, <valName>));
	'}
	'		
	'return result;";	
}	


str nodeOf(int n, int m, "")
	= "CompactNode.\<K, V\> valNodeOf(mutator)"
	;

str nodeOf(int n, int m, str args)
	= "valNodeOf(mutator, <args>)" 	//= "new Value<m>Index<n>Node(<args>)"
	;

str generate_bodyOf_removed(0, 0, str(str, str) eq)
	= "return Result.unchanged(this);"
	;

str generate_bodyOf_removed(int n, int m, str(str, str) eq) {	
	removed_clause_inline = str (int i) { return 
		"if (mask == <keyPosName><i>) {
		'	if (<eq("<keyName>", "<keyName><i>")>) {
		'		// remove <keyName><i>, <valName><i>
		'		result = Result.modified(<nodeOf(n, m-1, argsWithoutKeyVal(n, m, i))>);
		'	} else {
		'		result = Result.unchanged(this);
		'	}
		'}"; 
	};

	removed_clause_node = str (int i) { return 
		"if (mask == <nodePosName><i>) {
		'	final Result\<K, V, ? extends CompactNode\<K, V\>\> <nestedResult> = <nodeName><i>.removed(
		'					mutator, key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
		'
		'	if (<nestedResult>.isModified()) {
				final CompactNode\<K, V\> updatedNode = <nestedResult>.getNode();

				switch (updatedNode.sizePredicate()) {
				<if (n == 1 && m == 0) {>case SIZE_EMPTY:
				case SIZE_ONE:
					// escalate (singleton or empty) result
					result = <nestedResult>;
					break;< } else {> case SIZE_ONE:
					// inline sub-node value
					result = Result.modified(inlineValue(mutator, mask, updatedNode.headKey(), updatedNode.headVal()));
					break;<}>
					
				case SIZE_MORE_THAN_ONE:
					// update <nodeName><i>
					result = Result.modified(<nodeOf(n, m, argsReplacedNode(n, m, i, "mask", "updatedNode"))>);
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
	'final Result\<K, V, ? extends CompactNode\<K, V\>\> result;		
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

str generate_bodyOf_containsKey(int n, int m, str(str, str) eq) 
	= "final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);\n\n"	
	+ intercalate(" else ", 
		["if(mask == <keyPosName><i> && <eq("<keyName>", "<keyName><i>")>) { return true; }" | i <- [1..m+1]] +
		["if(mask == <nodePosName><i>) { return <nodeName><i>.containsKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>); }" | i <- [1..n+1]])
	+ " else { return false; }"
	;
	
str generate_bodyOf_findByKey(0, 0, str(str, str) eq) 
	= "return Optional.empty();"
	;

str generate_bodyOf_findByKey(int n, int m, str(str, str) eq) 
	= "final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);\n\n"	
	+ intercalate(" else ", 
		["if(mask == <keyPosName><i> && <eq("<keyName>", "<keyName><i>")>) { return Optional.of(entryOf(<keyName><i>, <valName><i>)); }" | i <- [1..m+1]] +
		["if(mask == <nodePosName><i>) { return <nodeName><i>.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>); }" | i <- [1..n+1]])
	+ " else { return Optional.empty(); }"
	;	
			
str generateGenericNodeClassString(int n, int m) =
	"private static final class Index<n>Node\<K, V\> extends CompactNode\<K, V\> {
	'	<for (i <- [1..n+1]) {>
	'	private final byte <nodePosName><i>;
	'	private final CompactNode\<K, V\> <nodeName><i>;
	'	<}>	
	
	'	Index<n>Node(<for (i <- [1..n+1]) {>final byte <nodePosName><i>, final CompactNode\<K, V\> <nodeName><i><if (i != n) {>, <}><}>) {					
	'		<intercalate("\n\n", ["this.<nodePosName><i> = <nodePosName><i>; this.<nodeName><i> = <nodeName><i>;" | i <- [1..n+1]])>
	'	}
	
	'	@SuppressWarnings(\"unchecked\")	
	'	@Override
	'	Result\<K, V, ? extends CompactNode\<K, V\>\> updated(AtomicReference\<Thread\> mutator, K <keyName>, int <keyName>Hash, V <valName>, int shift) {
	'		<generate_bodyOf_GenericNode_updated(n, m, equalityDefault)>
	'	}

	'	@SuppressWarnings(\"unchecked\")	
	'	@Override
	'	Result\<K, V, ? extends CompactNode\<K, V\>\> updated(AtomicReference\<Thread\> mutator, K <keyName>, int <keyName>Hash, V <valName>, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_GenericNode_updated(n, m, equalityComparator)>
	'	}

	'	@SuppressWarnings(\"unchecked\")	
	'	@Override
	'	Result\<K, V, ? extends CompactNode\<K, V\>\> removed(AtomicReference\<Thread\> mutator, K <keyName>, int <keyName>Hash, int shift) {
	'		<generate_bodyOf_GenericNode_removed(n, m, equalityDefault)>
	'	}

	'	@SuppressWarnings(\"unchecked\")	
	'	@Override
	'	Result\<K, V, ? extends CompactNode\<K, V\>\> removed(AtomicReference\<Thread\> mutator, K <keyName>, int <keyName>Hash, int shift, Comparator\<Object\> <cmpName>) {
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
	'	Optional\<java.util.Map.Entry\<K, V\>\> findByKey(Object <keyName>, int <keyName>Hash, int shift) {
	'		<generate_bodyOf_GenericNode_findByKey(n, m, equalityDefault)>
	'	}

	'	@SuppressWarnings(\"unchecked\")
	'	@Override
	'	Optional\<java.util.Map.Entry\<K, V\>\> findByKey(Object <keyName>, int <keyName>Hash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_GenericNode_findByKey(n, m, equalityComparator)>
	'	}

	'	@Override
	'	AbstractNode\<K, V\> getNode(int index) {
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
		
str generateCompactNodeString() = 
	"private static abstract class CompactNode\<K, V\> extends AbstractNode\<K, V\> {

		@SuppressWarnings(\"unchecked\")
		static final AbstractNode EMPTY_INDEX_NODE = new IndexNode(0, new AbstractNode[0], 0);

		@SuppressWarnings(\"unchecked\")
		static \<K, V\> CompactNode\<K, V\> mergeNodes(CompactNode\<K, V\> node0, int hash0,
						CompactNode\<K, V\> node1, int hash1, int shift) {
			final int mask0 = (hash0 \>\>\> shift) & BIT_PARTITION_MASK;
			final int mask1 = (hash1 \>\>\> shift) & BIT_PARTITION_MASK;

			if (mask0 != mask1) {
				// both nodes fit on same level
				final int bitmap = (1 \<\< mask0) | (1 \<\< mask1);
				final AbstractNode\<K, V\>[] nodes = new AbstractNode[2];

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
				final AbstractNode\<K, V\> node = mergeNodes(node0, hash0, node1, hash1, shift
								+ BIT_PARTITION_SIZE);

				return new IndexNode\<\>(bitmap, node, node.size());
			}
		}
	}"
	;
	
str generateLeafNodeString() = 
	"private static final class LeafNode\<K, V\> extends CompactNode\<K, V\> implements Map.Entry\<K, V\> {

		private final K key;
		private final V val;
		private final int keyHash;

		LeafNode(K key, int keyHash, V val) {
			this.key = key;
			this.val = val;
			this.keyHash = keyHash;
		}

		@Override
		Result\<K, V\> updated(AtomicReference\<Thread\> mutator, K key, int keyHash, V val, int shift,
						Comparator\<Object\> cmp) {
			if (this.keyHash != keyHash)
				// insert (no collision)
				return Result.modified(mergeNodes(this, this.keyHash, new LeafNode\<K, V\>(key,
								keyHash, val), keyHash, shift));

			if (cmp.compare(this.key, key) != 0)
				// insert (hash collision)
				return Result.modified(new LeafHashCollisionNode\<K, V\>(keyHash, new LeafNode[] {
								this, new LeafNode\<K, V\>(key, keyHash, val) }));

			if (cmp.compare(this.val, val) != 0)
				// value replaced
				return Result.updated(new LeafNode\<K, V\>(key, keyHash, val), val);

			return Result.unchanged(this);
		}

		@Override
		Result\<K, V\> removed(AtomicReference\<Thread\> mutator, K key, int hash, int shift,
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
		Optional\<Map.Entry\<K, V\>\> findByKey(Object key, int hash, int shift, Comparator\<Object\> cmp) {
			if (this.keyHash == hash && cmp.compare(this.key, key) == 0) {
				return Optional.of((Map.Entry\<K, V\>) this);
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
		Iterator\<AbstractNode\<K, V\>\> nodeIterator() {
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
		SupplierIterator\<K, V\> payloadIterator() {
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
	
	
	
	
str generate_bodyOf_GenericNode_containsKey(int n, int m, str(str, str) eq) = 
	"final int mask = (<keyName>Hash \>\>\> shift) & BIT_PARTITION_MASK;
	'final int bitpos = (1 \<\< mask);
	'
	'if ((valmap & bitpos) != 0) {
	'	return <eq("nodes[valIndex(bitpos)]", keyName)>;
	'}
	'
	'if ((bitmap & bitpos) != 0) {
	'	return ((AbstractNode\<K, V\>) nodes[bitIndex(bitpos)]).containsKey(<keyName>, <keyName>Hash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'}
	'
	'return false;"
	;
	
str generate_bodyOf_GenericNode_findByKey(int n, int m, str(str, str) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'final int bitpos = (1 \<\< mask);

	'if ((valmap & bitpos) != 0) { // inplace value
	'	final int valIndex = valIndex(bitpos);
	'
	'	if (<eq("nodes[valIndex]", keyName)>) {
	'		final K _key = (K) nodes[valIndex];
	'		final V _val = (V) nodes[valIndex + 1];
	'
	'		final Map.Entry\<K, V\> entry = entryOf(_key, _val);
	'		return Optional.of(entry);
	'	}
	'
	'	return Optional.empty();
	'}
	'
	'if ((bitmap & bitpos) != 0) { // node (not value)
	'	final AbstractNode\<K, V\> subNode = ((AbstractNode\<K, V\>) nodes[bitIndex(bitpos)]);
	'
	'	return subNode.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'}
	'
	'return Optional.empty();"
	;	
	
str generate_bodyOf_GenericNode_updated(int n, int m, str(str, str) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'final int bitpos = (1 \<\< mask);
	'
	'if ((valmap & bitpos) != 0) { // inplace value
	'	final int valIndex = valIndex(bitpos);
	'
	'	final Object currentKey = nodes[valIndex];
	'
	'	if (<eq("currentKey", keyName)>) {
	'
	'		final Object currentVal = nodes[valIndex + 1];
	'
	'		if (<eq("currentVal", valName)>) {
	'			return Result.unchanged(this);
	'		}
	'
	'		// update mapping
	'		final CompactNode\<K, V\> thisNew;
	'
	'		if (isAllowedToEdit(this.mutator, mutator)) {
	'			// no copying if already editable
	'			this.nodes[valIndex + 1] = val;
	'			thisNew = this;
	'		} else {
	'			final Object[] editableNodes = copyAndSet(this.nodes, valIndex + 1, val);
	'
	'			thisNew = CompactNode.\<K, V\> valNodeOf(mutator, bitmap, valmap, editableNodes, valueArity);
	'		}
	'
	'		return Result.updated(thisNew, (V) currentVal);
	'	} else {
	'		final CompactNode\<K, V\> nodeNew = mergeNodes((K) nodes[valIndex], nodes[valIndex].hashCode(), (V) nodes[valIndex + 1], key, keyHash, val, shift + BIT_PARTITION_SIZE);
	'
	'		final int offset = 2 * (valueArity - 1);
	'		final int index = Integer.bitCount(((bitmap | bitpos) ^ (valmap & ~bitpos)) & (bitpos - 1));
	'
	'		final Object[] editableNodes = copyAndMoveToBackPair(this.nodes, valIndex, offset + index, nodeNew);
	'
	'		final CompactNode\<K, V\> thisNew = CompactNode.\<K, V\> valNodeOf(mutator, bitmap | bitpos, valmap & ~bitpos, editableNodes, (byte) (valueArity - 1));
	'
	'		return Result.modified(thisNew);
	'	}
	'} else if ((bitmap & bitpos) != 0) { // node (not value)
	'	final int bitIndex = bitIndex(bitpos);
	'	final CompactNode\<K, V\> subNode = (CompactNode\<K, V\>) nodes[bitIndex];
	'
	'	final Result\<K, V, ? extends CompactNode\<K, V\>\> <nestedResult> = subNode.updated(mutator, key, keyHash, val, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'
	'	if (!<nestedResult>.isModified()) {
	'		return Result.unchanged(this);
	'	}
	'
	'	final CompactNode\<K, V\> thisNew;
	'
	'	// modify current node (set replacement node)
	'	if (isAllowedToEdit(this.mutator, mutator)) {
	'		// no copying if already editable
	'		this.nodes[bitIndex] = <nestedResult>.getNode();
	'		thisNew = this;
	'	} else {
	'		final Object[] editableNodes = copyAndSet(this.nodes, bitIndex, <nestedResult>.getNode());
	'
	'		thisNew = CompactNode.\<K, V\> valNodeOf(mutator, bitmap, valmap, editableNodes, valueArity);
	'	}
	'
	'	if (<nestedResult>.hasReplacedValue()) {
	'		return Result.updated(thisNew, <nestedResult>.getReplacedValue());
	'	}
	'
	'	return Result.modified(thisNew);
	'} else {
	'	// no value
	'	final Object[] editableNodes = copyAndInsertPair(this.nodes, valIndex(bitpos), key, val);
	'
	'	final CompactNode\<K, V\> thisNew = CompactNode.\<K, V\> valNodeOf(mutator, bitmap | bitpos, valmap | bitpos, editableNodes, (byte) (valueArity + 1));
	'
	'	return Result.modified(thisNew);
	'}";
		
str generate_bodyOf_GenericNode_removed(int n, int m, str(str, str) eq) =
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	final int bitpos = (1 \<\< mask);

	if ((valmap & bitpos) != 0) { // inplace value
		final int valIndex = valIndex(bitpos);

		if (<eq("nodes[valIndex]", keyName)>) {
			return Result.unchanged(this);
		}

		if (this.arity() == 5) {
			return Result.modified(removeInplaceValueAndConvertSpecializedNode(mask, bitpos));
		} else {
			final Object[] editableNodes = copyAndRemovePair(this.nodes, valIndex);

			final CompactNode\<K, V\> thisNew = CompactNode.\<K, V\> valNodeOf(mutator,
							this.bitmap & ~bitpos, this.valmap & ~bitpos, editableNodes,
							(byte) (valueArity - 1));

			return Result.modified(thisNew);
		}
	} else if ((bitmap & bitpos) != 0) { // node (not value)
		final int bitIndex = bitIndex(bitpos);
		final CompactNode\<K, V\> subNode = (CompactNode\<K, V\>) nodes[bitIndex];
		final Result\<K, V, ? extends CompactNode\<K, V\>\> <nestedResult> = subNode.removed(
						mutator, key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);

		if (!<nestedResult>.isModified()) {
			return Result.unchanged(this);
		}

		final CompactNode\<K, V\> subNodeNew = <nestedResult>.getNode();

		switch (subNodeNew.sizePredicate()) {
		case 0: {
			// remove node
			if (this.arity() == 5) {
				return Result.modified(removeSubNodeAndConvertSpecializedNode(mask, bitpos));
			} else {
				final Object[] editableNodes = copyAndRemovePair(this.nodes, bitIndex);

				final CompactNode\<K, V\> thisNew = CompactNode.\<K, V\> valNodeOf(mutator,
								bitmap & ~bitpos, valmap, editableNodes, valueArity);

				return Result.modified(thisNew);
			}
		}
		case 1: {
			// inline value (move to front)
			final int valIndexNew = Integer.bitCount((valmap | bitpos) & (bitpos - 1));

			final Object[] editableNodes = copyAndMoveToFrontPair(this.nodes, bitIndex,
							valIndexNew, subNodeNew.headKey(), subNodeNew.headVal());

			final CompactNode\<K, V\> thisNew = CompactNode.\<K, V\> valNodeOf(mutator, bitmap,
							valmap | bitpos, editableNodes, (byte) (valueArity + 1));

			return Result.modified(thisNew);
		}
		default: {
			// modify current node (set replacement node)
			if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[bitIndex] = subNodeNew;
				return Result.modified(this);
			} else {
				final Object[] editableNodes = copyAndSet(this.nodes, bitIndex, subNodeNew);

				final CompactNode\<K, V\> thisNew = CompactNode.\<K, V\> valNodeOf(mutator,
								bitmap, valmap, editableNodes, valueArity);

				return Result.modified(thisNew);
			}
		}
		}		
	}

	return Result.unchanged(this);";
	
str generateSpecializedMixedNodeClassString(int n, int m) =
	"private static final class Value<m>Index<n>Node\<K, V\> extends CompactNode\<K, V\> {
	'	<for (i <- [1..m+1]) {>
	'	private final byte <keyPosName><i>;
	'	private final K <keyName><i>;
	'	private final V <valName><i>;
	'	<}><for (i <- [1..n+1]) {>
	'	private final byte <nodePosName><i>;
	'	private final CompactNode\<K, V\> <nodeName><i>;
	'	<}>	
		
	'	Value<m>Index<n>Node(final AtomicReference\<Thread\> mutator<if ((n + m) > 0) {>, <}><intercalate(", ", 
		["final byte <keyPosName><i>, final K <keyName><i>, final V <valName><i>" | i <- [1..m+1]] +
		["final byte <nodePosName><i>, final CompactNode\<K, V\> <nodeName><i>" | i <- [1..n+1]])>) {					
	'		<intercalate("\n\n", 
				["this.<keyPosName><i> = <keyPosName><i>; this.<keyName><i> = <keyName><i>; this.<valName><i> = <valName><i>;" | i <- [1..m+1]] +
				["this.<nodePosName><i> = <nodePosName><i>; this.<nodeName><i> = <nodeName><i>;" | i <- [1..n+1]])>
	'		<if ((n + m) > 0) {>
	'		<}>assert nodeInvariant();
	'	}

	'	@Override
	'	Result\<K, V, ? extends CompactNode\<K, V\>\> updated(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, V val, int shift) {
	'		<generate_bodyOf_updated(n, m, equalityDefault)>
	'	}

	'	@Override
	'	Result\<K, V, ? extends CompactNode\<K, V\>\> updated(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, V val, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_updated(n, m, equalityComparator)>
	'	}

	'	@Override
	'	Result\<K, V, ? extends CompactNode\<K, V\>\> removed(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, int shift) {
	'		<generate_bodyOf_removed(n, m, equalityDefault)>
	'	}

	'	@Override
	'	Result\<K, V, ? extends CompactNode\<K, V\>\> removed(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_removed(n, m, equalityComparator)>
	'	}

	'	<if ((n + m) > 0) {>
	'	private CompactNode\<K, V\> inlineValue(AtomicReference\<Thread\> mutator, byte mask, K <keyName>, V <valName>) {
	'		<generate_bodyOf_inlineValue(n, m)>
	'	}
	'	<}>

	'	@Override
	'	boolean containsKey(Object key, int keyHash, int shift) {
	'		<generate_bodyOf_containsKey(n, m, equalityDefault)>
	'	}

	'	@Override
	'	boolean containsKey(Object key, int keyHash, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_containsKey(n, m, equalityComparator)>
	'	}

	'	@Override
	'	Optional\<java.util.Map.Entry\<K, V\>\> findByKey(Object key, int keyHash, int shift) {
	'		<generate_bodyOf_findByKey(n, m, equalityDefault)>
	'	}

	'	@Override
	'	Optional\<java.util.Map.Entry\<K, V\>\> findByKey(Object key, int keyHash, int shift,
	'					Comparator\<Object\> cmp) {
	'		<generate_bodyOf_findByKey(n, m, equalityComparator)>
	'	}
	
	'	@SuppressWarnings(\"unchecked\")
	'	@Override
	'	Iterator\<CompactNode\<K, V\>\> nodeIterator() {
	'		return ArrayIterator.\<CompactNode\<K, V\>\> of(new CompactNode[] { <intercalate(", ", ["<nodeName><i>" | i <- [1..n+1]])> });
	'	}

	'	@Override
	'	boolean hasNodes() {
	'		return <if (n > 0) {>true<} else {>false<}>;
	'	}

	'	@Override
	'	int nodeArity() {
	'		return <n>;
	'	}	

	'	@Override
	'	SupplierIterator\<K, V\> payloadIterator() {
	'		return ArrayKeyValueIterator.of(new Object[] { <keyValArgsUnmodified(m)> });
	'	}

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

	'	@Override
	'	V headVal() {
	'		<if (m == 0) {>throw new UnsupportedOperationException(\"Node does not directly contain a value.\")<} else {>return val1<}>;
	'	}	
	
	'	@Override
	'	AbstractNode\<K, V\> getNode(int index) {
	'		<generate_bodyOf_getNode(n)>
	'	}
	
	'	@Override
	'	K getKey(int index) {
	'		<generate_bodyOf_getKey(m)>
	'	}

	'	@Override
	'	V getValue(int index) {
	'		<generate_bodyOf_getValue(m)>
	'	}
	
	'	@Override
	'	byte sizePredicate() {
	'		return <generate_bodyOf_sizePredicate(n, m)>;
	'	}

	'	@Override
	'	public int hashCode() {
	'		<if ((n + m) > 0) {>final int prime = 31;<}>int result = 1;<for (i <- [1..m+1]) {>
	'		result = prime * result + <keyPosName><i>;
	'		result = prime * result + <keyName><i>.hashCode();
	'		result = prime * result + <valName><i>.hashCode();
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
	'		<if ((n + m) > 0) {>Value<m>Index<n>Node\<?, ?\> that = (Value<m>Index<n>Node\<?, ?\>) other;
	'
	'		<generate_equalityComparisons(n, m, equalityDefault)><}>
	'
	'		return true;
	'	}

	'	@Override
	'	public String toString() {		
	'		<if (n == 0 && m == 0) {>return \"[]\";<} else {>return String.format(\"[<intercalate(", ", [ "@%d: %s=%s" | i <- [1..m+1] ] + [ "@%d: %s" | i <- [1..n+1] ])>]\", <argsUnmodified(n, m)>);<}>
	'	}
	
	'}
	"
	;


str generate_bodyOf_sizePredicate(0, 0) = "SIZE_EMPTY";
str generate_bodyOf_sizePredicate(0, 1) = "SIZE_ONE";	
default str generate_bodyOf_sizePredicate(int n, int m) = "SIZE_MORE_THAN_ONE";


str generate_equalityComparisons(int n, int m, str(str, str) eq) =
	"<for (i <- [1..m+1]) {>
	'if (<keyPosName><i> != that.<keyPosName><i>) {
	'	return false;
	'}
	'if (!<eq("<keyName><i>", "that.<keyName><i>")>) {
	'	return false;
	'}
	'if (!<eq("<valName><i>", "that.<valName><i>")>) {
	'	return false;
	'}<}><for (i <- [1..n+1]) {>
	'if (<nodePosName><i> != that.<nodePosName><i>) {
	'	return false;
	'}
	'if (!<eq("<nodeName><i>", "that.<nodeName><i>")>) {
	'	return false;
	'}<}>"
	;
	

str generate_bodyOf_inlineValue(int n, int m) =
	"return <nodeOf(n, m+1, argsInsertKeyValAt(n, m, m+1, "mask", keyName, valName))>;"
when m == 0;
	
default str generate_bodyOf_inlineValue(int n, int m) =
	"<intercalate(" else ", [ "if (mask \< <keyPosName><i>) { return <nodeOf(n, m+1, argsInsertKeyValAt(n, m, i, "mask", keyName, valName))>; }" | i <- [1..m+1] ])> else {
	'	return <nodeOf(n, m+1, argsInsertKeyValAt(n, m, m+1, "mask", keyName, valName))>;
	'}"
	;