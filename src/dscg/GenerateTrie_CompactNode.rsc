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

// TODO: remove!!!
str emptyTrieNodeConstantName = "EMPTY_NODE";

str generateCompactNodeClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix) {
	abstractMembers = [ bitmapMethod, valmapMethod ];
	concreteMembers = [];
	
	members = abstractMembers + concreteMembers;	
	
	constructorArgs = asFieldList(
		  field("AtomicReference\<Thread\>", "mutator") 
		+ members);

	str className = "Compact<toString(ds)>Node";
	str hashCollisionClassName = "HashCollision<toString(ds)>Node<classNamePostfix>"; 

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
	'	protected static final int BIT_PARTITION_SIZE = <bitPartitionSize>;
	'	protected static final int BIT_PARTITION_MASK = 0b<for (i <- [1..bitPartitionSize+1]) {>1<}>;
	'
		<___bitmapField(bitPartitionSize).\type> <bitmapField.name>() {
			throw new UnsupportedOperationException(); 
		}

		<___valmapField(bitPartitionSize).\type> <valmapField.name>() {
			throw new UnsupportedOperationException();
		}

		static final byte SIZE_EMPTY = 0b00;
		static final byte SIZE_ONE = 0b01;
		static final byte SIZE_MORE_THAN_ONE = 0b10;

		<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {>
		abstract <CompactNode(ds)><Generics(ds)> convertToGenericNode();
		<}>

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
		abstract <key().\type> headKey();

		<if (ds == \map()) {>
		/**
		 * Returns the first value stored within this node.
		 * 
		 * @return first value
		 */
		abstract <val().\type> headVal();
		<}>

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
	'	abstract <CompactNode(ds)><Generics(ds)> copyAndSetValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <dec(val())>);
	<}>	
	
	'	abstract <CompactNode(ds)><Generics(ds)> copyAndInsertValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <dec(payloadTuple(ts))>);
	
	'	abstract <CompactNode(ds)><Generics(ds)> copyAndRemoveValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>);

	'	abstract <CompactNode(ds)><Generics(ds)> copyAndSetNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> <nodeName>);

	'	abstract <CompactNode(ds)><Generics(ds)> copyAndRemoveNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>);

	'	abstract <CompactNode(ds)><Generics(ds)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> <nodeName>);
	
	'	abstract <CompactNode(ds)><Generics(ds)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> <nodeName>);

		@SuppressWarnings(\"unchecked\")
		static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> mergeNodes(<dec(payloadTuple(ts, 0))>, int keyHash0, <dec(payloadTuple(ts, 1))>, int keyHash1, int shift) {
			assert !(<equalityDefaultForArguments(key("key0"), key("key1"))>);

			if (keyHash0 == keyHash1) {
				return new <hashCollisionClassName><InferredGenerics()>(keyHash0, (<key().\type>[]) new <if (isPrimitive(key().\type)) {><key().\type><} else {>Object<}>[] { key0, key1 }
								<if (ds == \map()) {>, (<val().\type>[]) new <if (isPrimitive(val().\type)) {><val().\type><} else {>Object<}>[] { val0, val1 }<}>);
			}

			final int mask0 = (keyHash0 \>\>\> shift) & BIT_PARTITION_MASK;
			final int mask1 = (keyHash1 \>\>\> shift) & BIT_PARTITION_MASK;

			if (mask0 != mask1) {
				// both nodes fit on same level
				<generate_bodyOf_mergeTwoValues(ts, setup, positionStyle)>
			} else {
				// values fit on next level
				final <CompactNode(ds)><Generics(ds)> node = mergeNodes(<use(payloadTuple(ts, 0))>, keyHash0, <use(payloadTuple(ts, 1))>, keyHash1, shift + BIT_PARTITION_SIZE);

				<generate_bodyOf_mergeOnNextLevel(ts, setup, positionStyle)>
			}
		}

		static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> mergeNodes(<CompactNode(ds)><Generics(ds)> node0, int keyHash0, <dec(payloadTuple(ts, 1))>, int keyHash1, int shift) {
			final int mask0 = (keyHash0 \>\>\> shift) & BIT_PARTITION_MASK;
			final int mask1 = (keyHash1 \>\>\> shift) & BIT_PARTITION_MASK;

			if (mask0 != mask1) {
				// both nodes fit on same level
				<generate_bodyOf_mergeNodeAndValue(ts, setup, positionStyle)>
			} else {
				// values fit on next level
				final <CompactNode(ds)><Generics(ds)> node = mergeNodes(node0, keyHash0, <use(payloadTuple(ts, 1))>, keyHash1, shift + BIT_PARTITION_SIZE);

				<generate_bodyOf_mergeOnNextLevel(ts, setup, positionStyle)>
			}
		}	

	'	static final <CompactNode(ds)> <emptyTrieNodeConstantName>;

	'	static {
	'		<if (isOptionEnabled(setup,useSpecialization())) {><emptyTrieNodeConstantName> = new <toString(ds)>0To0Node<classNamePostfix><InferredGenerics()>(null, (<chunkSizeToPrimitive(bitPartitionSize)>) 0, (<chunkSizeToPrimitive(bitPartitionSize)>) 0);<} else {><emptyTrieNodeConstantName> = new BitmapIndexed<toString(ds)>Node<InferredGenerics()>(null, (<chunkSizeToPrimitive(bitPartitionSize)>) 0, (<chunkSizeToPrimitive(bitPartitionSize)>) 0, new Object[] {}, (byte) 0);<}>	
	'	};
	
	<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {>
	'	static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(AtomicReference\<Thread\> mutator,
	'					<dec(___bitmapField(bitPartitionSize))>, <dec(___valmapField(bitPartitionSize))>, Object[] nodes, byte payloadArity) {
	'		return new BitmapIndexed<toString(ds)>Node<InferredGenerics()>(mutator, <use(bitmapField)>, <use(valmapField)>, nodes, payloadArity);
	'	}
	<}>

	'	// TODO: consolidate and remove
	'	static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(AtomicReference\<Thread\> mutator) {
	'		return nodeOf(mutator, (<chunkSizeToPrimitive(bitPartitionSize)>) 0, (<chunkSizeToPrimitive(bitPartitionSize)>) 0);
	'	}

	<if (isOptionEnabled(setup,useSpecialization())) {>
		<for(j <- [0..nMax+1], i <- [0..nMax+1], ((i + j) <= nMax && (i + j) <= nBound + 1 && !(i == nBound + 1))) {>
			<generate_valNodeOf_factoryMethod_bitmap(i, j, ts, setup, classNamePostfix)>
		<}>
	<}>

	'	final int dataIndex(<dec(___bitposField(bitPartitionSize))>) {
	'		return <integerOrLongObject(bitPartitionSize)>.bitCount(<useSafeUnsigned(___valmapMethod(bitPartitionSize))> & (bitpos - 1));
	'	}

	'	final int nodeIndex(<dec(___bitposField(bitPartitionSize))>) {
	'		return <integerOrLongObject(bitPartitionSize)>.bitCount(<useSafeUnsigned(___bitmapMethod(bitPartitionSize))> & (bitpos - 1));
	'	}

		<key().\type> keyAt(<dec(___bitposField(bitPartitionSize))>) {
			return getKey(dataIndex(bitpos)); 
		}
	
		<if (ds == \map()) {>
		<val().\type> valAt(<dec(___bitposField(bitPartitionSize))>) {
			return getValue(dataIndex(bitpos)); 
		}
		<}>

		<CompactNode(ds)><Generics(ds)> nodeAt(<dec(___bitposField(bitPartitionSize))>) {
			return getNode(nodeIndex(bitpos)); 
		}

	'	@Override
	'	boolean containsKey(<dec(key())>, int keyHash, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_containsKey(n, m, ts, setup, equalityDefaultForArguments)>
	'	}

	'	@Override
	'	boolean containsKey(<dec(key())>, int keyHash, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_containsKey(n, m, ts, setup, equalityComparatorForArguments)>
	'	}

	'	@Override
	'	Optional<KeyOrMapEntryGenerics(ds)> findByKey(<dec(key())>, int keyHash, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_findByKey(n, m, ts, setup, equalityDefaultForArguments)>
	'	}

	'	@Override
	'	Optional<KeyOrMapEntryGenerics(ds)> findByKey(<dec(key())>, int keyHash, int shift,
	'					Comparator\<Object\> cmp) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_findByKey(n, m, ts, setup, equalityComparatorForArguments)>
	'	}

	'	@Override
	'	Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator, <dec(payloadTuple(ts))>, int keyHash, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_updated(n, m, ts, setup, equalityDefaultForArguments)>
	'	}

	'	@Override
	'	Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator, <dec(payloadTuple(ts))>, int keyHash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_updated(n, m, ts, setup, equalityComparatorForArguments)>
	'	}

	'	@Override
	'	Result<ResultGenerics(ds)> removed(AtomicReference\<Thread\> mutator, <dec(key())>, int keyHash, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_removed(n, m, ts, setup, equalityDefaultForArguments)>
	'	}

	'	@Override
	'	Result<ResultGenerics(ds)> removed(AtomicReference\<Thread\> mutator, <dec(key())>, int keyHash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_removed(n, m, ts, setup, equalityComparatorForArguments)>
	'	}
	
	'}
	
	private static abstract class <className_compactNode(ts, setup, true, true)><Generics(ds)> extends Compact<toString(ds)>Node<Generics(ds)> {

		private <dec(___bitmapField(bitPartitionSize))>;
		private <dec(___valmapField(bitPartitionSize))>;

		<className_compactNode(ts, setup, true, true)>(final AtomicReference\<Thread\> mutator, <dec(___bitmapField(bitPartitionSize))>, <dec(___valmapField(bitPartitionSize))>) {
			this.<bitmapField.name> = <bitmapField.name>;
			this.<valmapField.name> = <valmapField.name>;
		}

		@Override
		public <___bitmapField(bitPartitionSize).\type> <bitmapField.name>() {
			return <bitmapField.name>;
		}

		@Override
		public <___valmapField(bitPartitionSize).\type> <valmapField.name>() {
			return <valmapField.name>;
		}

	}

	private static abstract class <className_compactNode(ts, setup, true, false)><Generics(ds)> extends Compact<toString(ds)>Node<Generics(ds)> {

		private <dec(___bitmapField(bitPartitionSize))>;

		<className_compactNode(ts, setup, true, false)>(final AtomicReference\<Thread\> mutator, <dec(___bitmapField(bitPartitionSize))>, <dec(___valmapField(bitPartitionSize))>) {
			this.<bitmapField.name> = <bitmapField.name>;
		}

		@Override
		public <___bitmapField(bitPartitionSize).\type> <bitmapField.name>() {
			return <bitmapField.name>;
		}

		@Override
		public <___valmapField(bitPartitionSize).\type> <valmapField.name>() {
			return 0;
		}

	}

	private static abstract class <className_compactNode(ts, setup, false, true)><Generics(ds)> extends Compact<toString(ds)>Node<Generics(ds)> {

		private <dec(___valmapField(bitPartitionSize))>;

		<className_compactNode(ts, setup, false, true)>(final AtomicReference\<Thread\> mutator, <dec(___bitmapField(bitPartitionSize))>, <dec(___valmapField(bitPartitionSize))>) {
			this.<valmapField.name> = <valmapField.name>;
		}

		@Override
		public <___bitmapField(bitPartitionSize).\type> <bitmapField.name>() {
			return 0;
		}

		@Override
		public <___valmapField(bitPartitionSize).\type> <valmapField.name>() {
			return <valmapField.name>;
		}

	}
	
	private static abstract class <className_compactNode(ts, setup, false, false)><Generics(ds)> extends Compact<toString(ds)>Node<Generics(ds)> {

		<className_compactNode(ts, setup, false, false)>(final AtomicReference\<Thread\> mutator, <dec(___bitmapField(bitPartitionSize))>, <dec(___valmapField(bitPartitionSize))>) {
		}

		@Override
		public <___bitmapField(bitPartitionSize).\type> <bitmapField.name>() {
			return 0;
		}

		@Override
		public <___valmapField(bitPartitionSize).\type> <valmapField.name>() {
			return 0;
		}

	}	
	"
	;
	
}

str generate_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"if (mask0 \< mask1) {
	'	return nodeOf(null, (byte) mask0, <use(payloadTuple(ts, 0))>, (byte) mask1, <use(payloadTuple(ts, 1))>);
	'} else {
	'	return nodeOf(null, (byte) mask1, <use(payloadTuple(ts, 1))>, (byte) mask0, <use(payloadTuple(ts, 0))>);
	'}";

str generate_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"<dec(___valmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask0 | 1L \<\< mask1);
	'
	'if (mask0 \< mask1) {
	'	return nodeOf(null, (<chunkSizeToPrimitive(bitPartitionSize)>) 0, <use(valmapField)>, <use(payloadTuple(ts, 0))>, <use(payloadTuple(ts, 1))>);
	'} else {
	'	return nodeOf(null, (<chunkSizeToPrimitive(bitPartitionSize)>) 0, <use(valmapField)>, <use(payloadTuple(ts, 1))>, <use(payloadTuple(ts, 0))>);
	'}";	
	
str generate_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"<dec(___valmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask0 | 1L \<\< mask1);
	'
	'if (mask0 \< mask1) {
	'	return nodeOf(null, (<chunkSizeToPrimitive(bitPartitionSize)>) 0, <use(valmapField)>, new Object[] { <use(payloadTuple(ts, 0))>, <use(payloadTuple(ts, 1))> }, (byte) 2);
	'} else {
	'	return nodeOf(null, (<chunkSizeToPrimitive(bitPartitionSize)>) 0, <use(valmapField)>, new Object[] { <use(payloadTuple(ts, 1))>, <use(payloadTuple(ts, 0))> }, (byte) 2);
	'}";	

default str generate_bodyOf_mergeTwoValues(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }

str generate_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"return nodeOf(null, (byte) mask0, node);";

str generate_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"<dec(___bitmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask0);
	'return nodeOf(null, <use(bitmapField)>, (<chunkSizeToPrimitive(bitPartitionSize)>) 0, node);";		
	
str generate_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"<dec(___bitmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask0);
	'return nodeOf(null, <use(bitmapField)>, (<chunkSizeToPrimitive(bitPartitionSize)>) 0, new Object[] { node }, (byte) 0);";	

default str generate_bodyOf_mergeOnNextLevel(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }

str generate_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"// store values before node
	'return nodeOf(null, (byte) mask1, <use(payloadTuple(ts, 1))>, (byte) mask0, node0);";

str generate_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"<dec(___bitmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask0);
	'<dec(___valmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask1);
	'
	'// store values before node
	'return nodeOf(null, <use(bitmapField)>, <use(valmapField)>, <use(payloadTuple(ts, 1))>, node0);";		
	
str generate_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"<dec(___bitmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask0);
	'<dec(___valmapField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask1);
	'
	'// store values before node
	'return nodeOf(null, <use(bitmapField)>, <use(valmapField)>, new Object[] { <use(payloadTuple(ts, 1))>, node0 }, (byte) 1);";			

default str generate_bodyOf_mergeNodeAndValue(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }


str generate_bodyOf_SpecializedBitmapPositionNode_containsKey(_, _, _, rel[Option,bool] setup, str(Argument, Argument) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;	
	
default str generate_bodyOf_SpecializedBitmapPositionNode_containsKey(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"final int mask = (<keyName>Hash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(___bitposField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask);
	'
	'if ((<use(valmapMethod)> & bitpos) != 0) {
	'	return <eq(key("keyAt(bitpos)"), key())>;
	'}
	'
	'if ((<use(bitmapMethod)> & bitpos) != 0) {
	'	return nodeAt(bitpos).containsKey(<keyName>, <keyName>Hash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);
	'}
	'
	'return false;"
	;
	
	
str generate_bodyOf_SpecializedBitmapPositionNode_findByKey(_, _, _, rel[Option,bool] setup, str(Argument, Argument) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;		
	
str generate_bodyOf_SpecializedBitmapPositionNode_findByKey(int n, int m, ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(___bitposField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask);

	'if ((<use(valmapMethod)> & bitpos) != 0) { // inplace value
	'	if (<eq(key("keyAt(bitpos)"), key())>) {
	'		<dec(key("_key"))> = keyAt(bitpos);
	'		<dec(val("_val"))> = valAt(bitpos);
	'
	'		final Map.Entry<GenericsExpanded(ds)> entry = (java.util.Map.Entry<GenericsExpanded(ds)>) entryOf(_key, _val);
	'		return Optional.of(entry);
	'	}
	'
	'	return Optional.empty();
	'}
	'
	'if ((<use(bitmapMethod)> & bitpos) != 0) { // node (not value)
	'	final <AbstractNode(ds)><Generics(ds)> subNode = nodeAt(bitpos);
	'
	'	return subNode.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);
	'}
	'
	'return Optional.empty();"
	;
	
str generate_bodyOf_SpecializedBitmapPositionNode_findByKey(int n, int m, ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(___bitposField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask);

	'if ((<use(valmapMethod)> & bitpos) != 0) { // inplace value
	'	if (<eq(key("keyAt(bitpos)"), key())>) {
	'		<dec(key("_key"))> = keyAt(bitpos);

	'		return Optional.of(<use(key("_key"))>);
	'	}
	'
	'	return Optional.empty();
	'}
	'
	'if ((<use(bitmapMethod)> & bitpos) != 0) { // node (not value)
	'	final <AbstractNode(ds)><Generics(ds)> subNode = nodeAt(bitpos);
	'
	'	return subNode.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);
	'}
	'
	'return Optional.empty();"
	;
	
default str generate_bodyOf_SpecializedBitmapPositionNode_findByKey(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) { throw "Ahhh"; }		
	
str generate_bodyOf_SpecializedBitmapPositionNode_updated(_, _, _, rel[Option,bool] setup, str(Argument, Argument) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;	
	
default str generate_bodyOf_SpecializedBitmapPositionNode_updated(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(___bitposField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask);
	'
	'if ((<use(valmapMethod)> & bitpos) != 0) { // inplace value
	'	<dec(key("currentKey"))> = keyAt(bitpos);
	'
	'	if (<eq(key("currentKey"), key())>) {
	'		<if (ds == \set()) {>return Result.unchanged(this);<} else {><dec(val("currentVal"))> = valAt(bitpos);
	'
	'		if (<eq(val("currentVal"), val())>) {
	'			return Result.unchanged(this);
	'		}
	'
	'		// update mapping
	'		final <CompactNode(ds)><Generics(ds)> thisNew = copyAndSetValue(mutator, bitpos, val);
	'
	'		return Result.updated(thisNew, currentVal);<}>
	'	} else {
	'		final <CompactNode(ds)><Generics(ds)> nodeNew = mergeNodes(keyAt(bitpos), <hashCode(key("keyAt(bitpos)"))>,<if (ds == \map()) {> valAt(bitpos),<}> key, keyHash,<if (ds == \map()) {> val,<}> shift + BIT_PARTITION_SIZE);
	'
	'		final <CompactNode(ds)><Generics(ds)> thisNew = copyAndMigrateFromInlineToNode(mutator, bitpos, nodeNew);
	'
	'		return Result.modified(thisNew);
	'	}
	'} else if ((<use(bitmapMethod)> & bitpos) != 0) { // node (not value)
	'	final <CompactNode(ds)><Generics(ds)> subNode = nodeAt(bitpos);
	'
	'	final Result<ResultGenerics(ds)> <nestedResult> = subNode.updated(mutator, <use(payloadTuple(ts))>, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);
	'
	'	if (!<nestedResult>.isModified()) {
	'		return Result.unchanged(this);
	'	}
	'
	'	final <CompactNode(ds)><Generics(ds)> thisNew = copyAndSetNode(mutator, bitpos, <nestedResult>.getNode());
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
	
	
str generate_bodyOf_SpecializedBitmapPositionNode_removed(_, _, _, rel[Option,bool] setup, str(Argument, Argument) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;			
		
str removed_in_subnode_with_newsize0_block(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}) = 
	"if (this.arity() == <nBound + 1>) {
	'	// remove node and convert
	'	final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveNode(mutator, bitpos).convertToGenericNode();
	'
	'	return Result.modified(thisNew);
	'}";

default str removed_in_subnode_with_newsize0_block(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = 
	"if (this.payloadArity() == 0 && this.nodeArity() == 1) {
	'	// escalate (singleton or empty) result
	'	return <nestedResult>;
	'}";
		
default str generate_bodyOf_SpecializedBitmapPositionNode_removed(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) =
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	<dec(___bitposField(bitPartitionSize))> = (<chunkSizeToPrimitive(bitPartitionSize)>) (1L \<\< mask);

	if ((<use(valmapMethod)> & bitpos) != 0) { // inplace value
		if (<eq(key("keyAt(bitpos)"), key())>) {
			<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {><removed_value_block(ts, setup)> else {<}>
				final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveValue(mutator, bitpos);
	
				return Result.modified(thisNew);
			<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {>}<}>
		} else {		
			return Result.unchanged(this);
		}
	} else if ((<use(bitmapMethod)> & bitpos) != 0) { // node (not value)
		final <CompactNode(ds)><Generics(ds)> subNode = nodeAt(bitpos);
		final Result<ResultGenerics(ds)> <nestedResult> = subNode.removed(
						mutator, key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);

		if (!<nestedResult>.isModified()) {
			return Result.unchanged(this);
		}

		final <CompactNode(ds)><Generics(ds)> subNodeNew = <nestedResult>.getNode();

		switch (subNodeNew.sizePredicate()) {
		case 0: {
			<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {><removed_in_subnode_with_newsize0_block(ts, setup)> else {<}>
				// remove node
				final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveNode(mutator, bitpos);

				return Result.modified(thisNew);
			<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {>}<}>
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
	
str removed_value_block(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}) =
	"if (this.arity() == <nBound + 1>) {
	'	final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveValue(mutator, bitpos).convertToGenericNode();
	'
	'	return Result.modified(thisNew);
	'}";
	
default str removed_value_block(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) =
	"if (this.payloadArity() == 2 && this.nodeArity() == 0) {
	'	/*
	'	 * Create new node with remaining pair. The new node
	'	 * will a) either become the new root returned, or b)
	'	 * unwrapped and inlined during returning.
	'	 */
	'	final <CompactNode(ds)><Generics(ds)> thisNew;
	'	final int newValmap = (shift == 0) ? this.valmap ^ bitpos
	'					: 1L \<\< (keyHash & BIT_PARTITION_MASK);
	'
	'	if (valIndex == 0) {
	'		thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator, newValmap,
	'						newValmap, getKey(1), getValue(1));
	'	} else {
	'		thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator, newValmap,
	'						newValmap, getKey(0), getValue(0));
	'	}
	'
	'	return Result.modified(thisNew);
	'}";
	
	
// bit-shifting with signed integers in Java
int oneShiftedLeftBy(int count) = toInt(pow(2, count)) when count >= 0 && count <= 30;
int oneShiftedLeftBy(31) = -2147483648;
default int oneShiftedLeftBy(int count) { throw "Not supported!"; }

str generate_valNodeOf_factoryMethod_bitmap(n:0, m:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix) { 
	// TODO: remove code duplication
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + metadataArguments(n, m, ts) + contentArguments(n, m, ts);

	return
	"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
	'	return <emptyTrieNodeConstantName>;
	'}"
	;
}

str generate_valNodeOf_factoryMethod_bitmap(n:1, m:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, compactionViaFieldToMethod()}, str classNamePostfix) {
	// TODO: remove code duplication
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + metadataArguments(n, m, ts) + contentArguments(n, m, ts);

	specializedClassName = "<toString(ds)><m>To<n>Node<classNamePostfix>";

	return
	"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
	'	switch(<use(bitmapMethod)>) {
	'	<for (i <- [1..nMax+1]) {>case <oneShiftedLeftBy(i-1)>:
	'		return new <toString(ds)><m>To<n>NodeAtMask<i-1><InferredGenerics()>(<intercalate(", ", mapper(constructorArgs, use))>);
	'	<}>default:
	'		throw new IllegalStateException(\"Index out of range.\");
	'	}
	'}"
	;
}

default str generate_valNodeOf_factoryMethod_bitmap(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix) {
	// TODO: remove code duplication
	constructorArgs = field("AtomicReference\<Thread\>", "mutator") + metadataArguments(n, m, ts) + contentArguments(n, m, ts);

	specializedClassName = "<toString(ds)><m>To<n>Node<classNamePostfix>";

	if ((n + m) <= nBound) {		
		return
		"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return new <specializedClassName><InferredGenerics()>(<intercalate(", ", mapper(constructorArgs, use))>);
		'}
		"; 
	} else if ((n + m) == nBound + 1 && (n + m) < nMax) {
		list[Argument] argsForArray = contentArguments(n, m, ts);

		return
		"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return nodeOf(mutator, <bitmapField.name>, <valmapField.name>, new Object[] { <use(argsForArray)> }, (byte) <m>);
		'}
		";
	} else {
		throw "Arguments out of bounds (n = <n>, m = <m>).";
	}
}
