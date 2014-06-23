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
module dscg::GenerateTrie_CompactNode

import List;

import dscg::Common;

str generateCompactNodeClassString(DataStructure ds, rel[Option,bool] setup) {
	members = [ field("int", "bitmap"), field("int", "valmap") ];
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	className = "Compact<toString(ds)>Node";

	Position positionStyle = positionBitmap();

	int n = 0; // TODO: remove
	int m = 0; // TODO: remove


//		@Override
//		abstract Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator, K key, int keyHash, V val, int shift);
//
//		@Override
//		abstract Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator, K key, int keyHash, V val, int shift, Comparator\<Object\> cmp);
//
//		@Override
//		abstract Result<ResultGenerics(ds)> removed(AtomicReference\<Thread\> mutator, K key, int hash, int shift);
//
//		@Override
//		abstract Result<ResultGenerics(ds)> removed(AtomicReference\<Thread\> mutator, K key, int hash, int shift, Comparator\<Object\> cmp);

	return
	"private static abstract class <className><Generics(ds)> extends Abstract<toString(ds)>Node<Generics(ds)> {
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

		abstract <CompactNode(ds)><Generics(ds)> convertToGenericNode();

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
		abstract <CompactNode(ds)><Generics(ds)> getNode(int index);

		@Override
		abstract Iterator\<? extends <CompactNode(ds)><Generics(ds)>\> nodeIterator();

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
	'	abstract <CompactNode(ds)><Generics(ds)> copyAndSetValue(AtomicReference\<Thread\> mutator, int index, V <valName>);
	<}>	
	
	'	abstract <CompactNode(ds)><Generics(ds)> copyAndInsertValue(AtomicReference\<Thread\> mutator, int bitpos, K <keyName>, V <valName>);
	
	'	abstract <CompactNode(ds)><Generics(ds)> copyAndRemoveValue(AtomicReference\<Thread\> mutator, int bitpos);

	'	abstract <CompactNode(ds)><Generics(ds)> copyAndSetNode(AtomicReference\<Thread\> mutator, int index, <CompactNode(ds)><Generics(ds)> <nodeName>);

	'	abstract <CompactNode(ds)><Generics(ds)> copyAndRemoveNode(AtomicReference\<Thread\> mutator, int bitpos);

	'	abstract <CompactNode(ds)><Generics(ds)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator, int bitpos, <CompactNode(ds)><Generics(ds)> <nodeName>);
	
	'	abstract <CompactNode(ds)><Generics(ds)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator, int bitpos, <CompactNode(ds)><Generics(ds)> <nodeName>);

		@SuppressWarnings(\"unchecked\")
		static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> mergeNodes(K key0, int keyHash0, V val0, K key1,
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
				final <CompactNode(ds)><Generics(ds)> node = mergeNodes(key0, keyHash0, val0, key1, keyHash1,
								val1, shift + BIT_PARTITION_SIZE);

				<generate_bodyOf_mergeOnNextLevel(setup, positionStyle)>
			}
		}

		static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> mergeNodes(<CompactNode(ds)><Generics(ds)> node0,
						int keyHash0, K key1, int keyHash1, V val1, int shift) {
			final int mask0 = (keyHash0 \>\>\> shift) & BIT_PARTITION_MASK;
			final int mask1 = (keyHash1 \>\>\> shift) & BIT_PARTITION_MASK;

			if (mask0 != mask1) {
				// both nodes fit on same level
				<generate_bodyOf_mergeNodeAndValue(setup, positionStyle)>
			} else {
				// values fit on next level
				final <CompactNode(ds)><Generics(ds)> node = mergeNodes(node0, keyHash0, key1, keyHash1, val1,
								shift + BIT_PARTITION_SIZE);

				<generate_bodyOf_mergeOnNextLevel(setup, positionStyle)>
			}
		}	

	'	static final CompactMapNode EMPTY_INPLACE_INDEX_NODE;

	'	static {
	'		<if (isOptionEnabled(setup,useSpecialization())) {>EMPTY_INPLACE_INDEX_NODE = new Map0To0Node\<\>(null, 0, 0);<} else {>EMPTY_INPLACE_INDEX_NODE = new BitmapIndexedMapNode\<\>(null, 0, 0, new Object[] {}, (byte) 0);<}>	
	'	};
	
	'	static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> valNodeOf(AtomicReference\<Thread\> mutator,
	'					int bitmap, int valmap, Object[] nodes, byte payloadArity) {
	'		return new BitmapIndexedMapNode\<\>(mutator, bitmap, valmap, nodes, payloadArity);
	'	}	

	'	// TODO: consolidate and remove
	'	static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> valNodeOf(AtomicReference\<Thread\> mutator) {
	'		return valNodeOf(mutator, 0, 0);
	'	}

	<if (isOptionEnabled(setup,useSpecialization())) {>
		<for(j <- [0..nMax+1], i <- [0..nMax+1], (i + j) <= nBound + 1 && !(i == nBound + 1)) {>
			<generate_valNodeOf_factoryMethod_bitmap(i, j, ds, setup)>
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
	'		<generate_bodyOf_SpecializedBitmapPositionNode_containsKey(n, m, ds, setup, equalityDefault)>
	'	}

	'	@Override
	'	boolean containsKey(Object key, int keyHash, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_containsKey(n, m, ds, setup, equalityComparator)>
	'	}

	'	@Override
	'	Optional<KeyOrMapEntryGenerics(ds)> findByKey(Object key, int keyHash, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_findByKey(n, m, ds, setup, equalityDefault)>
	'	}

	'	@Override
	'	Optional<KeyOrMapEntryGenerics(ds)> findByKey(Object key, int keyHash, int shift,
	'					Comparator\<Object\> cmp) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_findByKey(n, m, ds, setup, equalityComparator)>
	'	}

	'	@Override
	'	Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, V<if (ds == \set()) {>oid<}> val, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_updated(n, m, ds, setup, equalityDefault)>
	'	}

	'	@Override
	'	Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, V<if (ds == \set()) {>oid<}> val, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_updated(n, m, ds, setup, equalityComparator)>
	'	}

	'	@Override
	'	Result<ResultGenerics(ds)> removed(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_removed(n, m, ds, setup, equalityDefault)>
	'	}

	'	@Override
	'	Result<ResultGenerics(ds)> removed(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_removed(n, m, ds, setup, equalityComparator)>
	'	}
	
	'}
	"
	;
}

str generate_bodyOf_mergeTwoValues(rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"if (mask0 \< mask1) {
	'	return valNodeOf(null, (byte) mask0, key0, val0, (byte) mask1, key1, val1);
	'} else {
	'	return valNodeOf(null, (byte) mask1, key1, val1, (byte) mask0, key0, val0);
	'}";

str generate_bodyOf_mergeTwoValues(rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"final int valmap = 1 \<\< mask0 | 1 \<\< mask1;
	'
	'if (mask0 \< mask1) {
	'	return valNodeOf(null, valmap, valmap, key0, val0, key1, val1);
	'} else {
	'	return valNodeOf(null, valmap, valmap, key1, val1, key0, val0);
	'}";	
	
str generate_bodyOf_mergeTwoValues(rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"final int valmap = 1 \<\< mask0 | 1 \<\< mask1;
	'
	'if (mask0 \< mask1) {
	'	return valNodeOf(null, valmap, valmap, new Object[] { key0, val0, key1, val1 }, (byte) 2);
	'} else {
	'	return valNodeOf(null, valmap, valmap, new Object[] { key1, val1, key0, val0 }, (byte) 2);
	'}";	

default str generate_bodyOf_mergeTwoValues(Option _, Position _) { throw "something went wrong"; }

str generate_bodyOf_mergeOnNextLevel(rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"return valNodeOf(null, (byte) mask0, node);";

str generate_bodyOf_mergeOnNextLevel(rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"final int bitmap = 1 \<\< mask0;
	'return valNodeOf(null, bitmap, 0, node);";		
	
str generate_bodyOf_mergeOnNextLevel(rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"final int bitmap = 1 \<\< mask0;
	'return valNodeOf(null, bitmap, 0, new Object[] { node }, (byte) 0);";	

default str generate_bodyOf_mergeOnNextLevel(Option _, Position _) { throw "something went wrong"; }

str generate_bodyOf_mergeNodeAndValue(rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"// store values before node
	'return valNodeOf(null, (byte) mask1, key1, val1, (byte) mask0, node0);";

str generate_bodyOf_mergeNodeAndValue(rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"final int bitmap = 1 \<\< mask0 | 1 \<\< mask1;
	'final int valmap = 1 \<\< mask1;
	'
	'// store values before node
	'return valNodeOf(null, bitmap, valmap, key1, val1, node0);";		
	
str generate_bodyOf_mergeNodeAndValue(rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"final int bitmap = 1 \<\< mask0 | 1 \<\< mask1;
	'final int valmap = 1 \<\< mask1;
	'
	'// store values before node
	'return valNodeOf(null, bitmap, valmap, new Object[] { key1, val1, node0 }, (byte) 1);";			

default str generate_bodyOf_mergeNodeAndValue(Option _, Position _) { throw "something went wrong"; }


str generate_bodyOf_SpecializedBitmapPositionNode_containsKey(_, _, _, rel[Option,bool] setup, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;	
	
default str generate_bodyOf_SpecializedBitmapPositionNode_containsKey(int n, int m, DataStructure ds, rel[Option,bool] setup, str(str, str) eq) = 
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
	
	
str generate_bodyOf_SpecializedBitmapPositionNode_findByKey(_, _, _, rel[Option,bool] setup, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;		
	
default str generate_bodyOf_SpecializedBitmapPositionNode_findByKey(int n, int m, DataStructure ds, rel[Option,bool] setup, str(str, str) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'final int bitpos = (1 \<\< mask);

	'if ((valmap & bitpos) != 0) { // inplace value
	'	// final int valIndex = valIndex(bitpos);
	'
	'	if (<eq("getKey(keyIndex(bitpos))", keyName)>) {
	'		final K _key = getKey(keyIndex(bitpos));
	'		final V _val = getValue(valIndex(bitpos));
	'
	'		final Map.Entry<Generics(ds)> entry = entryOf(_key, _val);
	'		return Optional.of(entry);
	'	}
	'
	'	return Optional.empty();
	'}
	'
	'if ((bitmap & bitpos) != 0) { // node (not value)
	'	final <AbstractNode(ds)><Generics(ds)> subNode = getNode(nodeIndex(bitpos));
	'
	'	return subNode.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'}
	'
	'return Optional.empty();"
	;
	
	
str generate_bodyOf_SpecializedBitmapPositionNode_updated(_, _, _, rel[Option,bool] setup, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;	
	
default str generate_bodyOf_SpecializedBitmapPositionNode_updated(int n, int m, DataStructure ds, rel[Option,bool] setup, str(str, str) eq) = 
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
	'		final <CompactNode(ds)><Generics(ds)> thisNew = copyAndSetValue(mutator, valIndex(bitpos), val);
	'
	'		return Result.updated(thisNew, currentVal);<}>
	'	} else {
	'		final <CompactNode(ds)><Generics(ds)> nodeNew = mergeNodes(getKey(keyIndex(bitpos)), getKey(keyIndex(bitpos)).hashCode(),<if (ds == \map()) {> getValue(valIndex(bitpos)),<}> key, keyHash,<if (ds == \map()) {> val,<}> shift + BIT_PARTITION_SIZE);
	'
	'		final <CompactNode(ds)><Generics(ds)> thisNew = copyAndMigrateFromInlineToNode(mutator, bitpos, nodeNew);
	'
	'		return Result.modified(thisNew);
	'	}
	'} else if ((bitmap & bitpos) != 0) { // node (not value)
	'	final <CompactNode(ds)><Generics(ds)> subNode = getNode(nodeIndex(bitpos));
	'
	'	final Result<ResultGenerics(ds)> <nestedResult> = subNode.updated(mutator, key, keyHash, val, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'
	'	if (!<nestedResult>.isModified()) {
	'		return Result.unchanged(this);
	'	}
	'
	'	final <CompactNode(ds)><Generics(ds)> thisNew = copyAndSetNode(mutator, nodeIndex(bitpos), <nestedResult>.getNode());
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
	'	final <CompactNode(ds)><Generics(ds)> thisNew = copyAndInsertValue(mutator, bitpos, key<if (ds == \map()) {>, val<}>);
	'
	'	return Result.modified(thisNew);
	'}";
	
	
str generate_bodyOf_SpecializedBitmapPositionNode_removed(_, _, _, rel[Option,bool] setup, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;			
		
str removed_in_subnode_with_newsize0_block(DataStructure ds, rel[Option,bool] setup:{_*, <useSpecialization(),true>}) = 
	"if (this.arity() == <nBound + 1>) {
	'	// remove node and convert
	'	final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveNode(mutator, bitpos).convertToGenericNode();
	'
	'	return Result.modified(thisNew);
	'}";

default str removed_in_subnode_with_newsize0_block(DataStructure ds, rel[Option,bool] setup) = 
	"if (this.payloadArity() == 0 && this.nodeArity() == 1) {
	'	// escalate (singleton or empty) result
	'	return <nestedResult>;
	'}";
		
default str generate_bodyOf_SpecializedBitmapPositionNode_removed(int n, int m, DataStructure ds, rel[Option,bool] setup, str(str, str) eq) =
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	final int bitpos = (1 \<\< mask);

	if ((valmap & bitpos) != 0) { // inplace value
		final int valIndex = valIndex(bitpos);

		if (<eq("getKey(valIndex)", keyName)>) {			
			<removed_value_block(ds, setup)> else {
				final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveValue(mutator, bitpos);
	
				return Result.modified(thisNew);
			}
		} else {		
			return Result.unchanged(this);
		}
	} else if ((bitmap & bitpos) != 0) { // node (not value)
		final <CompactNode(ds)><Generics(ds)> subNode = getNode(nodeIndex(bitpos));
		final Result<ResultGenerics(ds)> <nestedResult> = subNode.removed(
						mutator, key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefault)) {>, <cmpName><}>);

		if (!<nestedResult>.isModified()) {
			return Result.unchanged(this);
		}

		final <CompactNode(ds)><Generics(ds)> subNodeNew = <nestedResult>.getNode();

		switch (subNodeNew.sizePredicate()) {
		case 0: {
			<removed_in_subnode_with_newsize0_block(ds, setup)> else {
				// remove node
				final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveNode(mutator, bitpos);

				return Result.modified(thisNew);
			}
		}
		case 1: {
			<if (isOptionEnabled(setup,useSpecialization())) {>// inline value (move to front)
				final <CompactNode(ds)><Generics(ds)> thisNew = copyAndMigrateFromNodeToInline(mutator, bitpos, subNodeNew);
	
				return Result.modified(thisNew);<} else {>if (this.payloadArity() == 0 && this.nodeArity() == 1) {
				// escalate (singleton or empty) result
				return <nestedResult>;
			} else {
				// inline value (move to front)
				final <CompactNode(ds)><Generics(ds)> thisNew = copyAndMigrateFromNodeToInline(mutator, bitpos, subNodeNew);
	
				return Result.modified(thisNew);
			}<}>
		}
		default: {
			// modify current node (set replacement node)
			final <CompactNode(ds)><Generics(ds)> thisNew = copyAndSetNode(mutator, bitpos, subNodeNew);
	
			return Result.modified(thisNew);
		}
		}		
	}

	return Result.unchanged(this);";
	
str removed_value_block(DataStructure ds, rel[Option,bool] setup:{_*, <useSpecialization(),true>}) =
	"if (this.arity() == <nBound + 1>) {
	'	final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveValue(mutator, bitpos).convertToGenericNode();
	'
	'	return Result.modified(thisNew);
	'}";
	
default str removed_value_block(DataStructure ds, rel[Option,bool] setup) =
	"if (this.payloadArity() == 2 && this.nodeArity() == 0) {
	'	/*
	'	 * Create new node with remaining pair. The new node
	'	 * will a) either become the new root returned, or b)
	'	 * unwrapped and inlined during returning.
	'	 */
	'	final <CompactNode(ds)><Generics(ds)> thisNew;
	'	final int newValmap = (shift == 0) ? this.valmap & ~bitpos
	'					: 1 \<\< (keyHash & BIT_PARTITION_MASK);
	'
	'	if (valIndex == 0) {
	'		thisNew = <CompactNode(ds)>.<Generics(ds)> valNodeOf(mutator, newValmap,
	'						newValmap, getKey(1), getValue(1));
	'	} else {
	'		thisNew = <CompactNode(ds)>.<Generics(ds)> valNodeOf(mutator, newValmap,
	'						newValmap, getKey(0), getValue(0));
	'	}
	'
	'	return Result.modified(thisNew);
	'}";
	
	
// bit-shifting with signed integers in Java
int oneShiftedLeftBy(int count) = toInt(pow(2, count)) when count >= 0 && count <= 30;
int oneShiftedLeftBy(31) = -2147483648;
default int oneShiftedLeftBy(int count) { throw "Not supported!"; }

str generate_valNodeOf_factoryMethod_bitmap(0, 0) { 
	// TODO: remove code duplication
	members = generateMembers_bitmap(0, 0);
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	return
	"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> valNodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
	'	return EMPTY_INPLACE_INDEX_NODE;
	'}"
	;
}

str generate_valNodeOf_factoryMethod_bitmap(n:1, m:0, rel[Option,bool] setup:{_*, compactionViaFieldToMethod()}) {
	// TODO: remove code duplication
	members = generateMembers_bitmap(n, m, ds);
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	className = "<toString(ds)><m>To<n>Node";

	return
	"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> valNodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
	'	switch(bitmap) {
	'	<for (i <- [1..nMax+1]) {>case <oneShiftedLeftBy(i-1)>:
	'		return new <toString(ds)><m>To<n>NodeAtMask<i-1>\<\>(<intercalate(", ", mapper(constructorArgs, use))>);
	'	<}>default:
	'		throw new IllegalStateException(\"Index out of range.\");
	'	}
	'}"
	;
}

default str generate_valNodeOf_factoryMethod_bitmap(int n, int m, DataStructure ds, rel[Option,bool] setup) {
	// TODO: remove code duplication
	members = generateMembers_bitmap(n, m, ds);
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + members;

	className = "<toString(ds)><m>To<n>Node";

	if ((n + m) <= nBound) {		
		return
		"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> valNodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
		'	return new <className>\<\>(<intercalate(", ", mapper(constructorArgs, use))>);
		'}
		"; 
	} else if ((n + m) == nBound + 1 && (n + m) < nMax) {
		list[Argument] argsForArray = generateMembers_bitmap(n, m, ds) - [ field("int", "bitmap"), field("int", "valmap") ];

		return
		"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> valNodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
		'	return valNodeOf(mutator, bitmap, valmap, new Object[] { <use(argsForArray)> }, (byte) <m>);
		'}
		";
	} else {
		throw "Arguments out of bounds.";
	}
}


list[Argument] generateMembers_bitmap(int n, int m, DataStructure ds) 
	= [ bitmap, valmap ]
	+ [ key(i), val(i) | i <- [1..m+1]] 
	+ [ \node(ds, i)   | i <- [1..n+1]]
	;
