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

void main() {
	//classStrings = [ generateClassString(n) | n <- [0..6] ];
	classStrings = 
		generateVerboseNodeString() + 
		generateLeafNodeString() + 
		[ generateIndexNodeClassString(n) | n <- [0..nMax+1] ];
	writeFile(|project://DSCG/gen/org/eclipse/imp/pdb/facts/util/AbstractSpecialisedTrieMap.java|, classStrings);
}
	
str generateClassString(n) =  
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
		
str posNodeArgsUnmodified(int n) 
	= intercalate(", ", ["<nodePosName><i>, <nodeName><i>"  | i <- [1..n+1]])
	;

str posNodeArgsReplaced(int n, int j, str newPos, str newName) 
	= intercalate(", ", ["<if (i == j) {><newPos>, <newName><} else {><nodePosName><i>, <nodeName><i><}>" | i <- [1..n+1]])
	;

str posNodeArgsInsertAt(int n, 0, str newPos, str newName) 
	= "<newPos>, <newName>, " + posNodeArgsUnmodified(n)
	;
	
str posNodeArgsInsertAt(int n, int j, str newPos, str newName) 
	= "<newPos>, <newName>, " + posNodeArgsUnmodified(n)
		when j == n+1;

str posNodeArgsInsertAt(int n, int j, str newPos, str newName) 
	= intercalate(", ", ["<if (i == j) {><newPos>, <newName>, <}><nodePosName><i>, <nodeName><i>" | i <- [1..n+1]])
	;	

str posNodeArgsRemoved(int n, int j) 
	= intercalate(", ", ["<nodePosName><i>, <nodeName><i>" | i <- [1..n+1], i != j])
	;
	
str generate_bodyOf_updated(0)
	= "return Result.modified(new LeafNode\<K, V\>(<keyName>, <keyName>Hash, <valName>));"
	;

str generate_bodyOf_updated(int n) {	
	updated_clause = str (int i, bool max) { return 
	"		<if (!max) {>if (mask \< <nodePosName><i>) {
	'			final VerboseNode\<K, V\> nodeNew = new LeafNode(key, keyHash, val);
	'			final VerboseNode\<K, V\> thisNew = new Index<n+1>Node(<posNodeArgsInsertAt(n, i, "mask", "nodeNew")>);
	'			result = Result.modified(thisNew);
	'		} else <}>if (mask == <nodePosName><i>) {
	'			final VerboseNode\<K, V\> subNode = <nodeName><i>;
	'
	'			final Result\<K, V, ? extends VerboseNode\<K, V\>\> subNodeResult = subNode.updated(
	'							mutator, key, keyHash, val, shift + BIT_PARTITION_SIZE, cmp);
	'
	'			if (subNodeResult.isModified()) {
	'				final VerboseNode\<K, V\> thisNew = new Index<n>Node(<posNodeArgsReplaced(n, i, "mask", "subNodeResult.getNode()")>);
	'				
	'				if (subNodeResult.hasReplacedValue()) {
	'					result = Result.updated(thisNew, subNodeResult.getReplacedValue());
	'				} else {
	'					result = Result.modified(thisNew);
	'				}
	'			} else {
	'				result = Result.unchanged(this);
	'			}
	'		}
	"; 
	};
	
	return 
	"		final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);
	'		final Result\<K, V, ? extends VerboseNode\<K, V\>\> result;		
	'		
	'		<intercalate(" else ", [ updated_clause(i, n == nMax)| i <- [1..n+1]])> else {
	'			<if (n != nMax) {>final VerboseNode\<K, V\> nodeNew = new LeafNode(key, keyHash, val);
	'			final VerboseNode\<K, V\> thisNew = new Index<n+1>Node(<posNodeArgsUnmodified(n)>, mask, nodeNew);
	'			result = Result.modified(thisNew);<} else {>result = Result.unchanged(this);<}>
	'		}
	'		
	'		return result;";	
}	

str generate_bodyOf_removed(0)
	= "return Result.unchanged(this);"
	;

str generate_bodyOf_removed(int n) {	
	removed_clause = str (int i) { return 
	"		if (mask == <nodePosName><i>) {
	'			final VerboseNode\<K, V\> subNode = <nodeName><i>;
	'
	'			final Result\<K, V, ? extends VerboseNode\<K, V\>\> subNodeResult = subNode.removed(
	'							mutator, key, keyHash, shift + BIT_PARTITION_SIZE, cmp);
	'
	'			if (subNodeResult.isModified()) {
	'				if (this.arity() == 1 && (subNodeResult.getNode().nodeArity() == 0)) {
	'					// escalate (singleton or empty) result
	'					result = subNodeResult;
	'				} else {
	'					final VerboseNode\<K, V\> thisNew = new Index<n-1>Node(<posNodeArgsRemoved(n, i)>);
	'					result = Result.modified(thisNew);
	'				}
	'			} else {
	'				result = Result.unchanged(this);
	'			}
	'		}
	"; 
	};
	
	return 
	"		final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);
	'		final Result\<K, V, ? extends VerboseNode\<K, V\>\> result;		
	'		
	'		<intercalate(" else ", [ removed_clause(i)| i <- [1..n+1]])> else {
	'			result = Result.unchanged(this);
	'		}
	'
	'		return result;";
}
		
str generate_bodyOf_containsKey(0) 
	= "return false;"
	;

str generate_bodyOf_containsKey(int n) 
	= "final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);\n\n"	
	+ intercalate(" else ", ["if(mask == <nodePosName><i>) { return <nodeName><i>.containsKey(key, keyHash, shift + BIT_PARTITION_SIZE, cmp); }" | i <- [1..n+1]])
	+ " else { return false; }"
	;
	
str generate_bodyOf_findByKey(0) 
	= "return Optional.empty();"
	;

str generate_bodyOf_findByKey(int n) 
	= "final byte mask = (byte) ((keyHash \>\>\> shift) & BIT_PARTITION_MASK);\n\n"	
	+ intercalate(" else ", ["if(mask == <nodePosName><i>) { return <nodeName><i>.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE, cmp); }" | i <- [1..n+1]])
	+ " else { return Optional.empty(); }"
	;	
			
str generateIndexNodeClassString(n) =
	"private static final class Index<n>Node\<K, V\> extends VerboseNode\<K, V\> {
	'	<for (i <- [1..n+1]) {>
	'	private final byte <nodePosName><i>;
	'	private final VerboseNode\<K, V\> <nodeName><i>;
	'	<}>	
	
	'	Index<n>Node(<for (i <- [1..n+1]) {>final byte <nodePosName><i>, final VerboseNode\<K, V\> <nodeName><i><if (i != n) {>, <}><}>) {					
	'		<intercalate("\n\n", ["this.<nodePosName><i> = <nodePosName><i>; this.<nodeName><i> = <nodeName><i>;" | i <- [1..n+1]])>
	'	}	
	
	'	@Override
	'	Result\<K, V, ? extends VerboseNode\<K, V\>\> updated(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, V val, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_updated(n)>
	'	}

	'	@Override
	'	Result\<K, V, ? extends VerboseNode\<K, V\>\> removed(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_removed(n)>
	'	}

	'	@Override
	'	boolean containsKey(Object key, int keyHash, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_containsKey(n)>
	'	}

	'	@Override
	'	Optional\<java.util.Map.Entry\<K, V\>\> findByKey(Object key, int keyHash, int shift,
	'					Comparator\<Object\> cmp) {
	'		<generate_bodyOf_findByKey(n)>
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
	
str generate_bodyOf_getNode(int n) = 	
	"		switch(index) {
	'			<for (i <- [1..n+1]) {>case <i-1>:
	'				return <nodeName><i>;
	'			<}>default:
	'				throw new IllegalStateException(\"Index out of range.\");
	'			}"
	;
	
str generateVerboseNodeString() = 
	"private static abstract class VerboseNode\<K, V\> extends AbstractNode\<K, V\> {

		@SuppressWarnings(\"unchecked\")
		static final AbstractNode EMPTY_INDEX_NODE = new IndexNode(0, new AbstractNode[0], 0);

		@SuppressWarnings(\"unchecked\")
		static \<K, V\> VerboseNode\<K, V\> mergeNodes(VerboseNode\<K, V\> node0, int hash0,
						VerboseNode\<K, V\> node1, int hash1, int shift) {
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
	"private static final class LeafNode\<K, V\> extends VerboseNode\<K, V\> implements Map.Entry\<K, V\> {

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
		boolean hasValues() {
			return true;
		}

		@Override
		SupplierIterator\<K, V\> valueIterator() {
			return ArrayKeyValueIterator.of(new Object[] { key, val }, 0, 2);
		}

		@Override
		int valueArity() {
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