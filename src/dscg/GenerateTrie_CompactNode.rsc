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
		  field(specific("AtomicReference\<Thread\>"), "mutator") 
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
		<toString(___bitmapField(bitPartitionSize).\type)> <bitmapField.name>() {
			throw new UnsupportedOperationException(); 
		}

		<toString(___valmapField(bitPartitionSize).\type)> <valmapField.name>() {
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
		abstract <toString(key().\type)> headKey();

		<if (ds == \map()) {>
		/**
		 * Returns the first value stored within this node.
		 * 
		 * @return first value
		 */
		abstract <toString(val().\type)> headVal();
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
	
	'	abstract <CompactNode(ds)><Generics(ds)> copyAndInsertValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <dec(payloadTuple(ts, setup))>);
	
	'	abstract <CompactNode(ds)><Generics(ds)> copyAndRemoveValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>);

	'	abstract <CompactNode(ds)><Generics(ds)> copyAndSetNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> <nodeName>);

	'	<CompactNode(ds)><Generics(ds)> copyAndInsertNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> <nodeName>) {
	'		throw new UnsupportedOperationException();	
	'	}
	
	'	<CompactNode(ds)><Generics(ds)> copyAndRemoveNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>) {	
	'		throw new UnsupportedOperationException();	
	'	}

	'	<CompactNode(ds)><Generics(ds)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> <nodeName>) {
	'		throw new UnsupportedOperationException();	
	'	}
		
	'	<CompactNode(ds)><Generics(ds)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> <nodeName>) {
	'		throw new UnsupportedOperationException();	
	'	}	

		@SuppressWarnings(\"unchecked\")
		static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> mergeNodes(<dec(payloadTuple(ts, setup, 0))>, int keyHash0, <dec(payloadTuple(ts, setup, 1))>, int keyHash1, int shift) {
			assert !(<equalityDefaultForArguments(key("key0"), key("key1"))>);

			if (keyHash0 == keyHash1) {
				return new <hashCollisionClassName><InferredGenerics()>(keyHash0, (<toString(key().\type)>[]) new <if (isPrimitive(key().\type)) {><toString(key().\type)><} else {>Object<}>[] { key0, key1 }
								<if (ds == \map()) {>, (<toString(val().\type)>[]) new <if (isPrimitive(val().\type)) {><toString(val().\type)><} else {>Object<}>[] { val0, val1 }<}>);
			}

			final int mask0 = (keyHash0 \>\>\> shift) & BIT_PARTITION_MASK;
			final int mask1 = (keyHash1 \>\>\> shift) & BIT_PARTITION_MASK;

			if (mask0 != mask1) {
				// both nodes fit on same level
				<generate_bodyOf_mergeTwoValues(ts, setup, positionStyle)>
			} else {
				// values fit on next level
				final <CompactNode(ds)><Generics(ds)> node = mergeNodes(<use(payloadTuple(ts, setup, 0))>, keyHash0, <use(payloadTuple(ts, setup, 1))>, keyHash1, shift + BIT_PARTITION_SIZE);

				<generate_bodyOf_mergeOnNextLevel(ts, setup, positionStyle)>
			}
		}

		static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> mergeNodes(<CompactNode(ds)><Generics(ds)> node0, int keyHash0, <dec(payloadTuple(ts, setup, 1))>, int keyHash1, int shift) {
			final int mask0 = (keyHash0 \>\>\> shift) & BIT_PARTITION_MASK;
			final int mask1 = (keyHash1 \>\>\> shift) & BIT_PARTITION_MASK;

			if (mask0 != mask1) {
				// both nodes fit on same level
				<generate_bodyOf_mergeNodeAndValue(ts, setup, positionStyle)>
			} else {
				// values fit on next level
				final <CompactNode(ds)><Generics(ds)> node = mergeNodes(node0, keyHash0, <use(payloadTuple(ts, setup, 1))>, keyHash1, shift + BIT_PARTITION_SIZE);

				<generate_bodyOf_mergeOnNextLevel(ts, setup, positionStyle)>
			}
		}	

	'	static final <CompactNode(ds)> <emptyTrieNodeConstantName>;

	'	static {
	'		<if (isOptionEnabled(setup,useSpecialization())) {><emptyTrieNodeConstantName> = new <toString(ds)>0To0Node<classNamePostfix><InferredGenerics()>(null, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0);<} else {><emptyTrieNodeConstantName> = new BitmapIndexed<toString(ds)>Node<InferredGenerics()>(null, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, new Object[] {}, (byte) 0);<}>	
	'	};
	
	<if (!isOptionEnabled(setup,useSpecialization()) || nBound < nMax) {>
	'	static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(AtomicReference\<Thread\> mutator,
	'					<dec(___bitmapField(bitPartitionSize))>, <dec(___valmapField(bitPartitionSize))>, Object[] nodes, byte payloadArity) {
	'		return new BitmapIndexed<toString(ds)>Node<InferredGenerics()>(mutator, <use(bitmapField)>, <use(valmapField)>, nodes, payloadArity);
	'	}
	<}>
	
	<if (!isOptionEnabled(setup,useSpecialization()) || nBound < nMax) {>
	'	@SuppressWarnings(\"unchecked\")
	'	static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(AtomicReference\<Thread\> mutator) {
	'		return <emptyTrieNodeConstantName>;
	'	}
	'
	'	static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(AtomicReference\<Thread\> mutator, <dec(___bitmapField(bitPartitionSize))>, <dec(___valmapField(bitPartitionSize))>, <dec(payloadTuple(ts, setup))>) {
	'		assert <use(bitmapField)> == 0;
	'		return nodeOf(mutator, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, <use(valmapField)>, new Object[] { <use(payloadTuple(ts, setup))> }, (byte) 1);
	'	}
	<} else {>
	'	// TODO: consolidate and remove
	'	static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(AtomicReference\<Thread\> mutator) {
	'		return nodeOf(mutator, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0);
	'	}
	<}>

	<generate_specializationFactoryMethods(ts, setup, classNamePostfix)>
	
	'	final int dataIndex(<dec(___bitposField(bitPartitionSize))>) {
	'		return <integerOrLongObject(bitPartitionSize)>.bitCount(<useSafeUnsigned(___valmapMethod(bitPartitionSize))> & (bitpos - 1));
	'	}

	'	final int nodeIndex(<dec(___bitposField(bitPartitionSize))>) {
	'		return <integerOrLongObject(bitPartitionSize)>.bitCount(<useSafeUnsigned(___bitmapMethod(bitPartitionSize))> & (bitpos - 1));
	'	}

		<toString(key().\type)> keyAt(<dec(___bitposField(bitPartitionSize))>) {
			return getKey(dataIndex(bitpos)); 
		}
	
		<if (ds == \map()) {>
		<toString(val().\type)> valAt(<dec(___bitposField(bitPartitionSize))>) {
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
	'	Optional<MapsToGenerics(ds)> findByKey(<dec(key())>, int keyHash, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_findByKey(n, m, ts, setup, equalityDefaultForArguments)>
	'	}

	'	@Override
	'	Optional<MapsToGenerics(ds)> findByKey(<dec(key())>, int keyHash, int shift,
	'					Comparator\<Object\> cmp) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_findByKey(n, m, ts, setup, equalityComparatorForArguments)>
	'	}

	'	@Override
	'	Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator, <dec(payloadTuple(ts, setup))>, int keyHash, int shift) {
	'		<generate_bodyOf_SpecializedBitmapPositionNode_updated(n, m, ts, setup, equalityDefaultForArguments)>
	'	}

	'	@Override
	'	Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator, <dec(payloadTuple(ts, setup))>, int keyHash, int shift, Comparator\<Object\> cmp) {
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

	'	/**
	'	 * @return 0 \<= mask \<= 2^BIT_PARTITION_SIZE - 1
	'	 */
	'	static byte recoverMask(<toString(chunkSizeToPrimitive(bitPartitionSize))> map, byte i_th) {
	'		assert 1 \<= i_th && i_th \<= <nMax>;
	'		
	'		byte cnt1 = 0;
	'		byte mask = 0;
	'		
	'		while (mask \< <nMax>) {
	'			if ((map & 0x01) == 0x01) {
	'				cnt1 += 1;
	'		
	'				if (cnt1 == i_th) {
	'					return mask;
	'				}
	'			}
	'		
	'			map = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (map \>\> 1);
	'			mask += 1;
	'		}
	'			
	'		assert cnt1 != i_th;
	'		throw new RuntimeException(\"Called with invalid arguments.\");
	'	}

	'	@Override
	'	public String toString() {
	'		final StringBuilder bldr = new StringBuilder();
	'		bldr.append(\'[\');
	'
	'		for (byte i = 0; i \< payloadArity(); i++) {
	'			final byte pos = recoverMask(<use(valmapMethod)>, (byte) (i + 1));
	'			bldr.append(String.format(\"@%d: <intercalate("=", times("%s", size(payloadTuple(ts, setup))))>\", pos, <use(invoke_get_for_payloadTuple(ts, setup, field("i")))>));
	'
	'			if (!((i + 1) == payloadArity())) {
	'				bldr.append(\", \");
	'			}
	'		}
	'
	'		if (payloadArity() \> 0 && nodeArity() \> 0) {
	'			bldr.append(\", \");
	'		}
	'
	'		for (byte i = 0; i \< nodeArity(); i++) {
	'			final byte pos = recoverMask(<use(bitmapMethod)>, (byte) (i + 1));
	'			bldr.append(String.format(\"@%d: %s\", pos, getNode(i)));
	'
	'			if (!((i + 1) == nodeArity())) {
	'				bldr.append(\", \");
	'			}
	'		}
	'
	'		bldr.append(\']\');
	'		return bldr.toString();
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
		public <toString(___bitmapField(bitPartitionSize).\type)> <bitmapField.name>() {
			return <bitmapField.name>;
		}

		@Override
		public <toString(___valmapField(bitPartitionSize).\type)> <valmapField.name>() {
			return <valmapField.name>;
		}

	}

	<if (isOptionEnabled(setup,useSpecialization())) {>
	private static abstract class <className_compactNode(ts, setup, true, false)><Generics(ds)> extends Compact<toString(ds)>Node<Generics(ds)> {

		private <dec(___bitmapField(bitPartitionSize))>;

		<className_compactNode(ts, setup, true, false)>(final AtomicReference\<Thread\> mutator, <dec(___bitmapField(bitPartitionSize))>, <dec(___valmapField(bitPartitionSize))>) {
			this.<bitmapField.name> = <bitmapField.name>;
		}

		@Override
		public <toString(___bitmapField(bitPartitionSize).\type)> <bitmapField.name>() {
			return <bitmapField.name>;
		}

		@Override
		public <toString(___valmapField(bitPartitionSize).\type)> <valmapField.name>() {
			return 0;
		}

	}

	private static abstract class <className_compactNode(ts, setup, false, true)><Generics(ds)> extends Compact<toString(ds)>Node<Generics(ds)> {

		private <dec(___valmapField(bitPartitionSize))>;

		<className_compactNode(ts, setup, false, true)>(final AtomicReference\<Thread\> mutator, <dec(___bitmapField(bitPartitionSize))>, <dec(___valmapField(bitPartitionSize))>) {
			this.<valmapField.name> = <valmapField.name>;
		}

		@Override
		public <toString(___bitmapField(bitPartitionSize).\type)> <bitmapField.name>() {
			return 0;
		}

		@Override
		public <toString(___valmapField(bitPartitionSize).\type)> <valmapField.name>() {
			return <valmapField.name>;
		}

	}
	
	private static abstract class <className_compactNode(ts, setup, false, false)><Generics(ds)> extends Compact<toString(ds)>Node<Generics(ds)> {

		<className_compactNode(ts, setup, false, false)>(final AtomicReference\<Thread\> mutator, <dec(___bitmapField(bitPartitionSize))>, <dec(___valmapField(bitPartitionSize))>) {
		}

		@Override
		public <toString(___bitmapField(bitPartitionSize).\type)> <bitmapField.name>() {
			return 0;
		}

		@Override
		public <toString(___valmapField(bitPartitionSize).\type)> <valmapField.name>() {
			return 0;
		}

	}
	<}>
	"
	;
	
}

str generate_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"if (mask0 \< mask1) {
	'	return nodeOf(null, (byte) mask0, <use(payloadTuple(ts, setup, 0))>, (byte) mask1, <use(payloadTuple(ts, setup, 1))>);
	'} else {
	'	return nodeOf(null, (byte) mask1, <use(payloadTuple(ts, setup, 1))>, (byte) mask0, <use(payloadTuple(ts, setup, 0))>);
	'}";

str generate_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"<dec(___valmapField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask0 | 1L \<\< mask1);
	'
	'if (mask0 \< mask1) {
	'	return nodeOf(null, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, <use(valmapField)>, <use(payloadTuple(ts, setup, 0))>, <use(payloadTuple(ts, setup, 1))>);
	'} else {
	'	return nodeOf(null, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, <use(valmapField)>, <use(payloadTuple(ts, setup, 1))>, <use(payloadTuple(ts, setup, 0))>);
	'}";	
	
str generate_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"<dec(___valmapField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask0 | 1L \<\< mask1);
	'
	'if (mask0 \< mask1) {
	'	return nodeOf(null, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, <use(valmapField)>, new Object[] { <use(payloadTuple(ts, setup, 0))>, <use(payloadTuple(ts, setup, 1))> }, (byte) 2);
	'} else {
	'	return nodeOf(null, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, <use(valmapField)>, new Object[] { <use(payloadTuple(ts, setup, 1))>, <use(payloadTuple(ts, setup, 0))> }, (byte) 2);
	'}";	

default str generate_bodyOf_mergeTwoValues(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }

str generate_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"return nodeOf(null, (byte) mask0, node);";

str generate_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"<dec(___bitmapField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask0);
	'return nodeOf(null, <use(bitmapField)>, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, node);";		
	
str generate_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"<dec(___bitmapField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask0);
	'return nodeOf(null, <use(bitmapField)>, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, new Object[] { node }, (byte) 0);";	

default str generate_bodyOf_mergeOnNextLevel(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }

str generate_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"// store values before node
	'return nodeOf(null, (byte) mask1, <use(payloadTuple(ts, setup, 1))>, (byte) mask0, node0);";

str generate_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"<dec(___bitmapField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask0);
	'<dec(___valmapField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask1);
	'
	'// store values before node
	'return nodeOf(null, <use(bitmapField)>, <use(valmapField)>, <use(payloadTuple(ts, setup, 1))>, node0);";		
	
str generate_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"<dec(___bitmapField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask0);
	'<dec(___valmapField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask1);
	'
	'// store values before node
	'return nodeOf(null, <use(bitmapField)>, <use(valmapField)>, new Object[] { <use(payloadTuple(ts, setup, 1))>, node0 }, (byte) 1);";			

default str generate_bodyOf_mergeNodeAndValue(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }


str generate_bodyOf_SpecializedBitmapPositionNode_containsKey(_, _, _, rel[Option,bool] setup, str(Argument, Argument) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;	
	
default str generate_bodyOf_SpecializedBitmapPositionNode_containsKey(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"final int mask = (<keyName>Hash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(___bitposField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask);
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
	
str generate_bodyOf_SpecializedBitmapPositionNode_findByKey(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(___bitposField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask);

	'if ((<use(valmapMethod)> & bitpos) != 0) { // inplace value
	'	if (<eq(key("keyAt(bitpos)"), key())>) {
	'		<dec(val("_val"))> = valAt(bitpos);
	'
	'		return Optional.of(<use(key("_val"))>);
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
when ds == \map() || ds == \vector()
	;
	
str generate_bodyOf_SpecializedBitmapPositionNode_findByKey(int n, int m, ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(___bitposField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask);

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
	'<dec(___bitposField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask);
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
	'		final <CompactNode(ds)><Generics(ds)> nodeNew = mergeNodes(keyAt(bitpos), <if (ds == \map()) {> valAt(bitpos),<}><hashCode(key("keyAt(bitpos)"))>, key, <if (ds == \map()) {> val,<}> keyHash, shift + BIT_PARTITION_SIZE);
	'
	'		<if (isOptionEnabled(setup,useSpecialization())) {>
	'		final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveValue(mutator, bitpos).copyAndInsertNode(mutator, bitpos, nodeNew);
	'		<} else {>
	'		final <CompactNode(ds)><Generics(ds)> thisNew = copyAndMigrateFromInlineToNode(mutator, bitpos, nodeNew);
	'		<}>
	'
	'		return Result.modified(thisNew);
	'	}
	'} else if ((<use(bitmapMethod)> & bitpos) != 0) { // node (not value)
	'	final <CompactNode(ds)><Generics(ds)> subNode = nodeAt(bitpos);
	'
	'	final Result<ResultGenerics(ds)> <nestedResult> = subNode.updated(mutator, <use(payloadTuple(ts, setup))>, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);
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
		
default str generate_bodyOf_SpecializedBitmapPositionNode_removed(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) =
	"final int mask = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	<dec(___bitposField(bitPartitionSize))> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask);

	if ((<use(valmapMethod)> & bitpos) != 0) { // inplace value
		if (<eq(key("keyAt(bitpos)"), key())>) {
			<removed_value_block(ts, setup)> else {	
				final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveValue(mutator, bitpos);
	
				return Result.modified(thisNew);					
			}
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
		
		if (subNodeNew.sizePredicate() == 0) {
			throw new IllegalStateException(\"Sub-node must have at least one element.\"); 
		}						
		assert subNodeNew.sizePredicate() \> 0;

		switch (subNodeNew.sizePredicate()) {
		case 1: {
			<if (isOptionEnabled(setup,useSpecialization())) {>// inline value (move to front)
				// final <CompactNode(ds)><Generics(ds)> thisNew = copyAndMigrateFromNodeToInline(mutator, bitpos, subNodeNew);
				final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveNode(mutator, bitpos).copyAndInsertValue(mutator, bitpos, subNodeNew.getKey(0)<if (ds == \map()) {>, subNodeNew.getValue(0)<}>);	
	
				return Result.modified(thisNew);<} else {>if (this.payloadArity() == 0 && this.nodeArity() == 1) {
				// escalate (singleton or empty) result
				return <nestedResult>;
			} else {
				// inline value (move to front)
				final <CompactNode(ds)><Generics(ds)> thisNew = copyAndMigrateFromNodeToInline(mutator, bitpos, subNodeNew);
				// final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveNode(mutator, bitpos).copyAndInsertValue(mutator, bitpos, subNodeNew.getKey(0)<if (ds == \map()) {>, subNodeNew.getValue(0)<}>);
	
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
	
//str removed_value_block(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}) =
//	"if (this.arity() == <nBound + 1>) {
//	'	final <CompactNode(ds)><Generics(ds)> thisNew = copyAndRemoveValue(mutator, bitpos).convertToSpecificNode();
//	'
//	'	return Result.modified(thisNew);
//	'}";	
	
default str removed_value_block(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) =
	"if (this.payloadArity() == 2 && this.nodeArity() == 0) {
	'	/*
	'	 * Create new node with remaining pair. The new node
	'	 * will a) either become the new root returned, or b)
	'	 * unwrapped and inlined during returning.
	'	 */
	'	final <CompactNode(ds)><Generics(ds)> thisNew;
	'	final <toString(chunkSizeToPrimitive(bitPartitionSize))> newDataMap = (shift == 0) ? (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (<use(valmapMethod)> ^ bitpos)
	'					: (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< (keyHash & BIT_PARTITION_MASK));
	'
	'	if (dataIndex(bitpos) == 0) {
	'		thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0,
	'						newDataMap, getKey(1)<if (ds == \map()) {>, getValue(1)<}>);
	'	} else {
	'		thisNew = <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0,
	'						newDataMap, getKey(0)<if (ds == \map()) {>, getValue(0)<}>);
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
	constructorArgs = field(specific("AtomicReference\<Thread\>"), "mutator") + metadataArguments(n, m, ts, setup) + contentArguments(n, m, ts, setup);

	return
	"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
	'	return <emptyTrieNodeConstantName>;
	'}"
	;
}

str generate_valNodeOf_factoryMethod_bitmap(n:1, m:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, compactionViaFieldToMethod()}, str classNamePostfix) {
	// TODO: remove code duplication
	constructorArgs = field(specific("AtomicReference\<Thread\>"), "mutator") + metadataArguments(n, m, ts, setup) + contentArguments(n, m, ts, setup);

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
	constructorArgs = field(specific("AtomicReference\<Thread\>"), "mutator") + metadataArguments(n, m, ts, setup) + contentArguments(n, m, ts, setup);

	specializedClassName = "<toString(ds)><m>To<n>Node<classNamePostfix>";

	if ((n + m) <= nBound) {		
		return
		"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return new <specializedClassName><InferredGenerics()>(<intercalate(", ", mapper(constructorArgs, use))>);
		'}
		"; 
	} else if ((n + m) == nBound + 1 && (n + m) < nMax) {
		list[Argument] argsForArray = contentArguments(n, m, ts, setup);

		return
		"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return nodeOf(mutator, <bitmapField.name>, <valmapField.name>, new Object[] { <use(argsForArray)> }, (byte) <m>);
		'}
		";
	} else {
		throw "Arguments out of bounds (n = <n>, m = <m>).";
	}
}




str generate_valNodeOf_factoryMethod_bitmap_untyped(mn:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix, int n = 0, int m = 0) { 
	// TODO: remove code duplication
	constructorArgs = field(specific("AtomicReference\<Thread\>"), "mutator") + metadataArguments(n, m, ts, setup) + contentArguments(n, m, ts, setup);

	return
	"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
	'	return <emptyTrieNodeConstantName>;
	'}"
	;
}

//str generate_valNodeOf_factoryMethod_bitmap_untyped(n:1, m:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, compactionViaFieldToMethod()}, str classNamePostfix) {
//	// TODO: remove code duplication
//	constructorArgs = field(specific("AtomicReference\<Thread\>"), "mutator") + metadataArguments(n, m, ts, setup) + contentArguments(n, m, ts, setup);
//
//	specializedClassName = "<toString(ds)><m>To<n>Node<classNamePostfix>";
//
//	return
//	"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
//	'	switch(<use(bitmapMethod)>) {
//	'	<for (i <- [1..nMax+1]) {>case <oneShiftedLeftBy(i-1)>:
//	'		return new <toString(ds)><m>To<n>NodeAtMask<i-1><InferredGenerics()>(<intercalate(", ", mapper(constructorArgs, use))>);
//	'	<}>default:
//	'		throw new IllegalStateException(\"Index out of range.\");
//	'	}
//	'}"
//	;
//}

default str generate_valNodeOf_factoryMethod_bitmap_untyped(int mn, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix, int n = mn, int m = 0) {
	// TODO: remove code duplication
	constructorArgs = field(specific("AtomicReference\<Thread\>"), "mutator") + metadataArguments(n, m, ts, setup) + contentArguments(n, m, ts, setup);

	specializedClassName = "<toString(ds)><m>To<n>Node<classNamePostfix>";

	if ((mn) <= tupleLength(ds) * nBound) {		
		return
		"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return new <specializedClassName><InferredGenerics()>(<intercalate(", ", mapper(constructorArgs, use))>);
		'}
		"; 
	} else if ((mn) > tupleLength(ds) * nBound && (mn) <= tupleLength(ds) * nBound + tupleLength(ds) && (mn) < tupleLength(ds) * nMax) {
		list[Argument] argsForArray = contentArguments(n, m, ts, setup);

		return
		"static final <Generics(ds)> <CompactNode(ds)><Generics(ds)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return nodeOf(mutator, <bitmapField.name>, <valmapField.name>, new Object[] { <use(argsForArray)> }, (byte) <integerOrLongObject(bitPartitionSize)>.bitCount(<useSafeUnsigned(___valmapField(bitPartitionSize))>));
		'}
		";
	} else {
		throw "Arguments out of bounds (n = <n>, m = <m>).";
	}
}






str generate_specializationFactoryMethods(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix) = 
	"
	<for(j <- [0..nMax+1], i <- [0..nMax+1], ((i + j) <= nMax && (i + j) <= nBound + 1 && !(i == nBound + 1))) {>
		<generate_valNodeOf_factoryMethod_bitmap(i, j, ts, setup, classNamePostfix)>
	<}>
	"
when isOptionEnabled(setup,useSpecialization()) && !isOptionEnabled(setup,useUntypedVariables())
	;
	
/**
 * More complicated slot count expression.
 * sort(toList({ n + 2 * m | n <- [0..32+1], m <- [0..32+1], ((n + m) <= 32)}))
 */
str generate_specializationFactoryMethods(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix) = 
	"
	<for(mn <- [0.. tupleLength(ds) * nMax + 1], mn <= tupleLength(ds) * nBound + tupleLength(ds)) {>
		<generate_valNodeOf_factoryMethod_bitmap_untyped(mn, ts, setup, classNamePostfix)>
	<}>
	"
when isOptionEnabled(setup,useSpecialization()) && isOptionEnabled(setup,useUntypedVariables())
	;
		
default str generate_specializationFactoryMethods(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix) = "";





list[Argument] invoke_get_for_payloadTuple(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, Argument idx) = [ key("getKey(<use(idx)>)"), val("getValue(<use(idx)>)") ];
list[Argument] invoke_get_for_payloadTuple(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, Argument idx) = [ key("getKey(<use(idx)>)") ];


