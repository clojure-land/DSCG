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

str generateCompactNodeClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) {
	abstractMembers = [ bitmapMethod, valmapMethod ];
	concreteMembers = [];
	
	members = abstractMembers + concreteMembers;	
	
	constructorArgs = asFieldList(
		  ts.mutator 
		+ members);

	str className = "<ts.CompactNodeStr>";
	str hashCollisionClassName = "HashCollision<toString(ds)>Node<ts.classNamePostfix>"; 

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
	"private static abstract class <className><ts.GenericsStr> extends Abstract<toString(ds)>Node<ts.GenericsStr> {
	'
	'	protected static final int BIT_PARTITION_SIZE = <bitPartitionSize>;
	'	protected static final int BIT_PARTITION_MASK = 0b<for (i <- [1..bitPartitionSize+1]) {>1<}>;
	'
		<toString(ts.bitmapField.\type)> <bitmapField.name>() {
			throw new UnsupportedOperationException(); 
		}

		<toString(ts.valmapField.\type)> <valmapField.name>() {
			throw new UnsupportedOperationException();
		}

		static final byte SIZE_EMPTY = 0b00;
		static final byte SIZE_ONE = 0b01;
		static final byte SIZE_MORE_THAN_ONE = 0b10;

		<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {>
		abstract <ts.CompactNodeStr><ts.GenericsStr> convertToGenericNode();
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
		@Deprecated
		<toString(key().\type)> headKey() {
			return getKey(0);
		}

		<if (ds == \map()) {>
		/**
		 * Returns the first value stored within this node.
		 * 
		 * @return first value
		 */
		@Deprecated 
		<toString(val().\type)> headVal() {
			return getValue(0);
		}
		<}>

		@Override
		abstract <ts.CompactNodeStr><ts.GenericsStr> getNode(int index);

	'	@Deprecated
	'	@Override
	'	Iterator\<? extends <ts.CompactNodeStr><ts.GenericsStr>\> nodeIterator() {
	'		throw new UnsupportedOperationException();	
	'	}

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
	'	abstract <ts.CompactNodeStr><ts.GenericsStr> copyAndSetValue(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>, <dec(val())>);
	<}>	
	
	'	abstract <ts.CompactNodeStr><ts.GenericsStr> copyAndInsertValue(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>, <dec(ts.payloadTuple)>);
	
	'	abstract <ts.CompactNodeStr><ts.GenericsStr> copyAndRemoveValue(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>);

	'	abstract <ts.CompactNodeStr><ts.GenericsStr> copyAndSetNode(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>, <ts.CompactNodeStr><ts.GenericsStr> <nodeName>);

	'	<ts.CompactNodeStr><ts.GenericsStr> copyAndInsertNode(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>, <ts.CompactNodeStr><ts.GenericsStr> <nodeName>) {
	'		throw new UnsupportedOperationException();	
	'	}
	
	'	<ts.CompactNodeStr><ts.GenericsStr> copyAndRemoveNode(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>) {	
	'		throw new UnsupportedOperationException();	
	'	}

	'	<ts.CompactNodeStr><ts.GenericsStr> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>, <ts.CompactNodeStr><ts.GenericsStr> <nodeName>) {
	'		throw new UnsupportedOperationException();	
	'	}
		
	'	<ts.CompactNodeStr><ts.GenericsStr> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>, <ts.CompactNodeStr><ts.GenericsStr> <nodeName>) {
	'		throw new UnsupportedOperationException();	
	'	}
	
	'	<ts.CompactNodeStr><ts.GenericsStr> removeInplaceValueAndConvertToSpecializedNode(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>) {
	'			throw new UnsupportedOperationException();
	'	}

		@SuppressWarnings(\"unchecked\")
		static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> mergeNodes(<dec(payloadTuple(ts, setup, 0))>, int keyHash0, <dec(payloadTuple(ts, setup, 1))>, int keyHash1, int shift) {
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
				final <ts.CompactNodeStr><ts.GenericsStr> node = mergeNodes(<use(payloadTuple(ts, setup, 0))>, keyHash0, <use(payloadTuple(ts, setup, 1))>, keyHash1, shift + BIT_PARTITION_SIZE);

				<generate_bodyOf_mergeOnNextLevel(ts, setup, positionStyle)>
			}
		}

		static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> mergeNodes(<ts.CompactNodeStr><ts.GenericsStr> node0, int keyHash0, <dec(payloadTuple(ts, setup, 1))>, int keyHash1, int shift) {
			final int mask0 = (keyHash0 \>\>\> shift) & BIT_PARTITION_MASK;
			final int mask1 = (keyHash1 \>\>\> shift) & BIT_PARTITION_MASK;

			if (mask0 != mask1) {
				// both nodes fit on same level
				<generate_bodyOf_mergeNodeAndValue(ts, setup, positionStyle)>
			} else {
				// values fit on next level
				final <ts.CompactNodeStr><ts.GenericsStr> node = mergeNodes(node0, keyHash0, <use(payloadTuple(ts, setup, 1))>, keyHash1, shift + BIT_PARTITION_SIZE);

				<generate_bodyOf_mergeOnNextLevel(ts, setup, positionStyle)>
			}
		}	

	'	static final <ts.CompactNodeStr> <emptyTrieNodeConstantName>;

	'	static {
	'		<if (isOptionEnabled(setup,useSpecialization())) {><emptyTrieNodeConstantName> = new <toString(ds)>0To0Node<ts.classNamePostfix><InferredGenerics()>(null, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0);<} else {><emptyTrieNodeConstantName> = new BitmapIndexed<toString(ds)>Node<InferredGenerics()>(null, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, new Object[] {}, (byte) 0);<}>	
	'	};
	
	<if (!isOptionEnabled(setup,useSpecialization()) || nBound < nMax) {>
	'	static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> nodeOf(AtomicReference\<Thread\> mutator,
	'					<dec(ts.bitmapField)>, <dec(ts.valmapField)>, Object[] nodes, byte payloadArity) {
	'		return new BitmapIndexed<toString(ds)>Node<InferredGenerics()>(mutator, <use(bitmapField)>, <use(valmapField)>, nodes, payloadArity);
	'	}
	<}>
	
	<if (!isOptionEnabled(setup,useSpecialization()) || nBound < nMax) {>
	'	@SuppressWarnings(\"unchecked\")
	'	static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> nodeOf(AtomicReference\<Thread\> mutator) {
	'		return <emptyTrieNodeConstantName>;
	'	}
	<} else {>
	'	// TODO: consolidate and remove
	'	static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> nodeOf(AtomicReference\<Thread\> mutator) {
	'		return nodeOf(mutator, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0);
	'	}
	<}>

	<if (!isOptionEnabled(setup,useSpecialization())) {>
	'	static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> nodeOf(AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>, <dec(ts.payloadTuple)>) {
	'		assert <use(bitmapField)> == 0;
	'		return nodeOf(mutator, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, <use(valmapField)>, new Object[] { <use(ts.payloadTuple)> }, (byte) 1);
	'	}
	<}>


	<generate_specializationFactoryMethods(ts, setup)>
	
	'	final int dataIndex(<dec(ts.bitposField)>) {
	'		return <integerOrLongObject(bitPartitionSize)>.bitCount(<useSafeUnsigned(___valmapMethod(bitPartitionSize))> & (bitpos - 1));
	'	}

	'	final int nodeIndex(<dec(ts.bitposField)>) {
	'		return <integerOrLongObject(bitPartitionSize)>.bitCount(<useSafeUnsigned(___bitmapMethod(bitPartitionSize))> & (bitpos - 1));
	'	}

		<toString(key().\type)> keyAt(<dec(ts.bitposField)>) {
			return getKey(dataIndex(bitpos)); 
		}
	
		<if (ds == \map()) {>
		<toString(val().\type)> valAt(<dec(ts.bitposField)>) {
			return getValue(dataIndex(bitpos)); 
		}
		<}>

		<ts.CompactNodeStr><ts.GenericsStr> nodeAt(<dec(ts.bitposField)>) {
			return getNode(nodeIndex(bitpos)); 
		}

	<implOrOverride(ts.AbstractNode_containsKey, 		generate_bodyOf_SpecializedBitmapPositionNode_containsKey(n, m, ts, setup, equalityDefaultForArguments		))>
	<implOrOverride(ts.AbstractNode_containsKeyEquiv,	generate_bodyOf_SpecializedBitmapPositionNode_containsKey(n, m, ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.AbstractNode_findByKey, 		generate_bodyOf_SpecializedBitmapPositionNode_findByKey(n, m, ts, setup, equalityDefaultForArguments	))>
	<implOrOverride(ts.AbstractNode_findByKeyEquiv,	generate_bodyOf_SpecializedBitmapPositionNode_findByKey(n, m, ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.AbstractNode_updated, 		generate_bodyOf_SpecializedBitmapPositionNode_updated(n, m, ts, setup, equalityDefaultForArguments		))>
	<implOrOverride(ts.AbstractNode_updatedEquiv,	generate_bodyOf_SpecializedBitmapPositionNode_updated(n, m, ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.AbstractNode_removed, 		generate_bodyOf_SpecializedBitmapPositionNode_removed(n, m, ts, setup, equalityDefaultForArguments		))>
	<implOrOverride(ts.AbstractNode_removedEquiv,	generate_bodyOf_SpecializedBitmapPositionNode_removed(n, m, ts, setup, equalityComparatorForArguments	))>

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
	'			bldr.append(String.format(\"@%d: <intercalate("=", times("%s", size(ts.payloadTuple)))>\", pos, <use(invoke_get_for_payloadTuple(ts, setup, field("i")))>));
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
	
	private static abstract class <className_compactNode(ts, setup, true, true)><ts.GenericsStr> extends <ts.CompactNodeStr><ts.GenericsStr> {

		private <dec(ts.bitmapField)>;
		private <dec(ts.valmapField)>;

		<className_compactNode(ts, setup, true, true)>(final AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>) {
			this.<bitmapField.name> = <bitmapField.name>;
			this.<valmapField.name> = <valmapField.name>;
		}

		@Override
		public <toString(ts.bitmapField.\type)> <bitmapField.name>() {
			return <bitmapField.name>;
		}

		@Override
		public <toString(ts.valmapField.\type)> <valmapField.name>() {
			return <valmapField.name>;
		}

	}

	<if (isOptionEnabled(setup,useSpecialization())) {>
	private static abstract class <className_compactNode(ts, setup, true, false)><ts.GenericsStr> extends <ts.CompactNodeStr><ts.GenericsStr> {

		private <dec(ts.bitmapField)>;

		<className_compactNode(ts, setup, true, false)>(final AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>) {
			this.<bitmapField.name> = <bitmapField.name>;
		}

		@Override
		public <toString(ts.bitmapField.\type)> <bitmapField.name>() {
			return <bitmapField.name>;
		}

		@Override
		public <toString(ts.valmapField.\type)> <valmapField.name>() {
			return 0;
		}

	}

	private static abstract class <className_compactNode(ts, setup, false, true)><ts.GenericsStr> extends <ts.CompactNodeStr><ts.GenericsStr> {

		private <dec(ts.valmapField)>;

		<className_compactNode(ts, setup, false, true)>(final AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>) {
			this.<valmapField.name> = <valmapField.name>;
		}

		@Override
		public <toString(ts.bitmapField.\type)> <bitmapField.name>() {
			return 0;
		}

		@Override
		public <toString(ts.valmapField.\type)> <valmapField.name>() {
			return <valmapField.name>;
		}

	}
	
	private static abstract class <className_compactNode(ts, setup, false, false)><ts.GenericsStr> extends <ts.CompactNodeStr><ts.GenericsStr> {

		<className_compactNode(ts, setup, false, false)>(final AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>) {
		}

		@Override
		public <toString(ts.bitmapField.\type)> <bitmapField.name>() {
			return 0;
		}

		@Override
		public <toString(ts.valmapField.\type)> <valmapField.name>() {
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
	"<dec(ts.valmapField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask0 | 1L \<\< mask1);
	'
	'if (mask0 \< mask1) {
	'	return nodeOf(null, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, <use(valmapField)>, <use(payloadTuple(ts, setup, 0))>, <use(payloadTuple(ts, setup, 1))>);
	'} else {
	'	return nodeOf(null, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, <use(valmapField)>, <use(payloadTuple(ts, setup, 1))>, <use(payloadTuple(ts, setup, 0))>);
	'}";	
	
str generate_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"<dec(ts.valmapField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask0 | 1L \<\< mask1);
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
	"<dec(ts.bitmapField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask0);
	'return nodeOf(null, <use(bitmapField)>, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, node);";		
	
str generate_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"<dec(ts.bitmapField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask0);
	'return nodeOf(null, <use(bitmapField)>, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0, new Object[] { node }, (byte) 0);";	

default str generate_bodyOf_mergeOnNextLevel(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }

str generate_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"// store values before node
	'return nodeOf(null, (byte) mask1, <use(payloadTuple(ts, setup, 1))>, (byte) mask0, node0);";

str generate_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"<dec(ts.bitmapField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask0);
	'<dec(ts.valmapField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask1);
	'
	'// store values before node
	'return nodeOf(null, <use(bitmapField)>, <use(valmapField)>, <use(payloadTuple(ts, setup, 1))>, node0);";		
	
str generate_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"<dec(ts.bitmapField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask0);
	'<dec(ts.valmapField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask1);
	'
	'// store values before node
	'return nodeOf(null, <use(bitmapField)>, <use(valmapField)>, new Object[] { <use(payloadTuple(ts, setup, 1))>, node0 }, (byte) 1);";			

default str generate_bodyOf_mergeNodeAndValue(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }


str generate_bodyOf_SpecializedBitmapPositionNode_containsKey(_, _, _, rel[Option,bool] setup, str(Argument, Argument) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;	
	
default str generate_bodyOf_SpecializedBitmapPositionNode_containsKey(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"<dec(ts.mask)> = (<keyName>Hash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(ts.bitposField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask);
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
	"<dec(ts.mask)> = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(ts.bitposField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask);

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
	'	final <AbstractNode(ds)><ts.GenericsStr> subNode = nodeAt(bitpos);
	'
	'	return subNode.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);
	'}
	'
	'return Optional.empty();"
when ds == \map() || ds == \vector()
	;
	
str generate_bodyOf_SpecializedBitmapPositionNode_findByKey(int n, int m, ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"<dec(ts.mask)> = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(ts.bitposField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask);

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
	'	final <AbstractNode(ds)><ts.GenericsStr> subNode = nodeAt(bitpos);
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
	
default str generate_bodyOf_SpecializedBitmapPositionNode_updated(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) {
	
	Argument subNode 	= \node(ds, "subNode");
	Argument subNodeNew = \node(ds, "subNodeNew");

	return  
	"<dec(ts.mask)> = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	'<dec(ts.bitposField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask);
	'
	'if ((<use(valmapMethod)> & bitpos) != 0) { // inplace value
	'	<dec(field(primitive("int"), "dataIndex"))> = dataIndex(bitpos);
		<dec(key("currentKey"))> = getKey(dataIndex);
	'
	'	if (<eq(key("currentKey"), key())>) {
	'		<if (ds == \set()) {>return this;<} else {><dec(val("currentVal"))> = getValue(dataIndex);
	'
	'		if (<eq(val("currentVal"), val())>) {
	'			return this;
	'		} else {
	'			// update mapping
	'			details.updated(currentVal);
	'			return copyAndSetValue(mutator, bitpos, val);
	'		}<}>
	'	} else {
	'		<if (ds == \map()) {><dec(val("currentVal"))> = getValue(dataIndex);<}>
	'		final <ts.CompactNodeStr><ts.GenericsStr> subNodeNew = mergeNodes(currentKey, <if (ds == \map()) {> currentVal,<}><hashCode(key("currentKey"))>, key, <if (ds == \map()) {> val,<}> keyHash, shift + BIT_PARTITION_SIZE);
	'
	'		<if (isOptionEnabled(setup,useSpecialization())) {>
	'		// final <ts.CompactNodeStr><ts.GenericsStr> thisNew = copyAndRemoveValue(mutator, bitpos).copyAndInsertNode(mutator, bitpos, nodeNew);
	'		// final <ts.CompactNodeStr><ts.GenericsStr> thisNew = copyAndMigrateFromInlineToNode(mutator, bitpos, nodeNew);
	'		
	'		details.modified();
	'		return copyAndMigrateFromInlineToNode(mutator, bitpos, subNodeNew);
	'		<} else {>
	'		details.modified();
	'		return copyAndMigrateFromInlineToNode(mutator, bitpos, subNodeNew);
	'		<}>
	'	}
	'} else if ((<use(bitmapMethod)> & bitpos) != 0) { // node (not value)
	'	<dec(subNode)> = nodeAt(bitpos);
	'	<dec(subNodeNew)> = <use(subNode)>.updated(mutator, <use(ts.payloadTuple)>, keyHash, shift + BIT_PARTITION_SIZE, <use(ts.details)><if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);
	'
	'	if (<use(ts.details)>.isModified()) {
	'		return copyAndSetNode(mutator, bitpos, <use(subNodeNew)>);
	'	} else {
	'		return this;
	'	}
	'} else {
	'	// no value
	'	<use(ts.details)>.modified();
	'	return copyAndInsertValue(mutator, bitpos, key<if (ds == \map()) {>, val<}>);
	'}"
	;
}
	
	
str generate_bodyOf_SpecializedBitmapPositionNode_removed(_, _, _, rel[Option,bool] setup, str(Argument, Argument) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(setup,methodsWithComparator()) || (eq == equalityDefault))
	;			
		
default str generate_bodyOf_SpecializedBitmapPositionNode_removed(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) {

	Argument subNode 	= \node(ds, "subNode");
	Argument subNodeNew = \node(ds, "subNodeNew");

	return
	"<dec(ts.mask)> = (keyHash \>\>\> shift) & BIT_PARTITION_MASK;
	<dec(ts.bitposField)> = (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< mask);

	if ((<use(valmapMethod)> & bitpos) != 0) { // inplace value
		<dec(field(primitive("int"), "dataIndex"))> = dataIndex(bitpos);
		
		if (<eq(key("getKey(dataIndex)"), key())>) {
			<if (ds == \map()) {><dec(val("currentVal"))> = getValue(dataIndex); details.updated(currentVal);<} else {>details.modified();<}>			
		
			<removed_value_block1(ts, setup)> else <if (isOptionEnabled(setup,useSpecialization())) {><removed_value_block2(ts, setup)> else<}> {					
				return copyAndRemoveValue(mutator, bitpos);
			}
		} else {		
			return this;
		}
	} else if ((<use(bitmapMethod)> & bitpos) != 0) { // node (not value)
		<dec(subNode)> = nodeAt(bitpos);
		<dec(subNodeNew)> = subNode.removed(
						mutator, key, keyHash, shift + BIT_PARTITION_SIZE, details<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);

		if (!<use(ts.details)>.isModified()) {
			return this;
		}
		
		switch (subNodeNew.sizePredicate()) {
		case 0: {
			throw new IllegalStateException(\"Sub-node must have at least one element.\"); 
		}
		case 1: {
			<if (isOptionEnabled(setup,useSpecialization())) {>// inline value (move to front)
				details.modified();
				return copyAndMigrateFromNodeToInline(mutator, bitpos, subNodeNew);<} else {>if (this.payloadArity() == 0 && this.nodeArity() == 1) {
				// escalate (singleton or empty) result
				return <use(subNodeNew)>;
			} else {
				// inline value (move to front)
				return copyAndMigrateFromNodeToInline(mutator, bitpos, subNodeNew);
			}<}>
		}
		default: {
			// modify current node (set replacement node)
			return copyAndSetNode(mutator, bitpos, subNodeNew);
		}
		}		
	}

	return this;";
}
	
default str removed_value_block2(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) =
	"if (this.arity() == <nBound + 1>) {
	'	return removeInplaceValueAndConvertToSpecializedNode(mutator, bitpos);
	'}";
	
default str removed_value_block1(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) =
	"if (this.payloadArity() == 2 && this.nodeArity() == 0) {
	'	/*
	'	 * Create new node with remaining pair. The new node
	'	 * will a) either become the new root returned, or b)
	'	 * unwrapped and inlined during returning.
	'	 */
	'	final <toString(chunkSizeToPrimitive(bitPartitionSize))> newDataMap = (shift == 0) ? (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (<use(valmapMethod)> ^ bitpos)
	'					: (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (1L \<\< (keyHash & BIT_PARTITION_MASK));
	'
	'	if (dataIndex == 0) {
	'		return <ts.CompactNodeStr>.<ts.GenericsStr> nodeOf(mutator, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0,
	'						newDataMap, getKey(1)<if (ds == \map()) {>, getValue(1)<}>);
	'	} else {
	'		return <ts.CompactNodeStr>.<ts.GenericsStr> nodeOf(mutator, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) 0,
	'						newDataMap, getKey(0)<if (ds == \map()) {>, getValue(0)<}>);
	'	}
	'}";
	
	
// bit-shifting with signed integers in Java
int oneShiftedLeftBy(int count) = toInt(pow(2, count)) when count >= 0 && count <= 30;
int oneShiftedLeftBy(31) = -2147483648;
default int oneShiftedLeftBy(int count) { throw "Not supported!"; }

str generate_valNodeOf_factoryMethod_bitmap(n:0, m:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) { 
	// TODO: remove code duplication
	constructorArgs = ts.mutator + metadataArguments(n, m, ts, setup) + contentArguments(n, m, ts, setup);

	return
	"static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
	'	return <emptyTrieNodeConstantName>;
	'}"
	;
}

str generate_valNodeOf_factoryMethod_bitmap(n:1, m:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, compactionViaFieldToMethod()}) {
	// TODO: remove code duplication
	constructorArgs = ts.mutator + metadataArguments(n, m, ts, setup) + contentArguments(n, m, ts, setup);

	specializedClassName = "<toString(ds)><m>To<n>Node<ts.classNamePostfix>";

	return
	"static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
	'	switch(<use(bitmapMethod)>) {
	'	<for (i <- [1..nMax+1]) {>case <oneShiftedLeftBy(i-1)>:
	'		return new <toString(ds)><m>To<n>NodeAtMask<i-1><InferredGenerics()>(<intercalate(", ", mapper(constructorArgs, use))>);
	'	<}>default:
	'		throw new IllegalStateException(\"Index out of range.\");
	'	}
	'}"
	;
}

default str generate_valNodeOf_factoryMethod_bitmap(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) {
	// TODO: remove code duplication
	constructorArgs = ts.mutator + metadataArguments(n, m, ts, setup) + contentArguments(n, m, ts, setup);

	specializedClassName = "<toString(ds)><m>To<n>Node<ts.classNamePostfix>";

	if ((n + m) <= nBound) {		
		return
		"static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return new <specializedClassName><InferredGenerics()>(<intercalate(", ", mapper(constructorArgs, use))>);
		'}
		"; 
	} else if ((n + m) == nBound + 1 && (n + m) < nMax) {
		list[Argument] argsForArray = contentArguments(n, m, ts, setup);

		return
		"static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return nodeOf(mutator, <bitmapField.name>, <valmapField.name>, new Object[] { <use(argsForArray)> }, (byte) <m>);
		'}
		";
	} else {
		throw "Arguments out of bounds (n = <n>, m = <m>).";
	}
}




str generate_valNodeOf_factoryMethod_bitmap_untyped(mn:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, int n = 0, int m = 0) { 
	// TODO: remove code duplication
	constructorArgs = ts.mutator + metadataArguments(n, m, ts, setup) + contentArguments(n, m, ts, setup);

	return
	"static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
	'	return <emptyTrieNodeConstantName>;
	'}"
	;
}

//str generate_valNodeOf_factoryMethod_bitmap_untyped(n:1, m:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, compactionViaFieldToMethod()}) {
//	// TODO: remove code duplication
//	constructorArgs = ts.mutator + metadataArguments(n, m, ts, setup) + contentArguments(n, m, ts, setup);
//
//	specializedClassName = "<toString(ds)><m>To<n>Node<ts.classNamePostfix>";
//
//	return
//	"static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
//	'	switch(<use(bitmapMethod)>) {
//	'	<for (i <- [1..nMax+1]) {>case <oneShiftedLeftBy(i-1)>:
//	'		return new <toString(ds)><m>To<n>NodeAtMask<i-1><InferredGenerics()>(<intercalate(", ", mapper(constructorArgs, use))>);
//	'	<}>default:
//	'		throw new IllegalStateException(\"Index out of range.\");
//	'	}
//	'}"
//	;
//}

default str generate_valNodeOf_factoryMethod_bitmap_untyped(int mn, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, int n = mn, int m = 0) {
	// TODO: remove code duplication
	constructorArgs = ts.mutator + metadataArguments(n, m, ts, setup) + contentArguments(n, m, ts, setup);

	specializedClassName = "<toString(ds)><m>To<n>Node<ts.classNamePostfix>";

	if ((mn) <= tupleLength(ds) * nBound) {		
		return
		"static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return new <specializedClassName><InferredGenerics()>(<intercalate(", ", mapper(constructorArgs, use))>);
		'}
		"; 
	} else if ((mn) > tupleLength(ds) * nBound && (mn) <= tupleLength(ds) * nBound + tupleLength(ds) && (mn) < tupleLength(ds) * nMax) {
		list[Argument] argsForArray = contentArguments(n, m, ts, setup);

		return
		"static final <ts.GenericsStr> <ts.CompactNodeStr><ts.GenericsStr> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return nodeOf(mutator, <bitmapField.name>, <valmapField.name>, new Object[] { <use(argsForArray)> }, (byte) <integerOrLongObject(bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapField)>));
		'}
		";
	} else {
		throw "Arguments out of bounds (n = <n>, m = <m>).";
	}
}






str generate_specializationFactoryMethods(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = 
	//  && !(i == nBound + 1)

	"
	<for(j <- [0..nMax+1], i <- [0..nMax+1], ((i + j) <= nMax && (i + j) <= nBound + 1)) {>
		<generate_valNodeOf_factoryMethod_bitmap(i, j, ts, setup)>
	<}>
	"
when isOptionEnabled(setup,useSpecialization()) && !isOptionEnabled(setup,useUntypedVariables())
	;
	
/**
 * More complicated slot count expression.
 * sort(toList({ n + 2 * m | n <- [0..32+1], m <- [0..32+1], ((n + m) <= 32)}))
 */
str generate_specializationFactoryMethods(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = 
	"
	<for(mn <- [0.. tupleLength(ds) * nMax + 1], mn <= tupleLength(ds) * nBound + tupleLength(ds)) {>
		<generate_valNodeOf_factoryMethod_bitmap_untyped(mn, ts, setup)>
	<}>
	"
when isOptionEnabled(setup,useSpecialization()) && isOptionEnabled(setup,useUntypedVariables())
	;
		
default str generate_specializationFactoryMethods(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = "";





list[Argument] invoke_get_for_payloadTuple(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, Argument idx) = [ key("getKey(<use(idx)>)"), val("getValue(<use(idx)>)") ];
list[Argument] invoke_get_for_payloadTuple(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, Argument idx) = [ key("getKey(<use(idx)>)") ];


