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
module dscg::GenerateTrie_BitmapIndexedNode

import List;
import dscg::Common;
import dscg::ArrayUtils;

str generateBitmapIndexedNodeClassString(TrieSpecifics ts) {

	className = "BitmapIndexed<toString(ts.ds)>Node";

	return
	"private static final class <className><Generics(ts.ds, ts.tupleTypes)> extends <className_compactNode(ts, ts.setup, true, true)><Generics(ts.ds, ts.tupleTypes)> {
		private AtomicReference\<Thread\> mutator;

		private Object[] nodes;
		final private byte payloadArity;
		
		<className>(AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>, Object[] nodes, byte payloadArity) {
			super(mutator, <use(bitmapField)>, <use(valmapField)>);
			
			assert (<use(tupleLengthConstant)> * <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapField)>) + <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.bitmapField)>) == nodes.length);

			this.mutator = mutator;

			this.nodes = nodes;
			this.payloadArity = payloadArity;

			assert (payloadArity == <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapField)>));
			// assert (payloadArity() \>= 2 || nodeArity() \>= 1); // =
			// // SIZE_MORE_THAN_ONE

			// for (int i = 0; i \< <use(tupleLengthConstant)> * payloadArity; i++)
			// assert ((nodes[i] instanceof CompactNode) == false);
			//
			// for (int i = <use(tupleLengthConstant)> * payloadArity; i \< nodes.length; i++)
			// assert ((nodes[i] instanceof CompactNode) == true);

			// assert invariant
			<if (isOptionEnabled(ts.setup,useSpecialization()) && ts.nBound < ts.nMax) {>assert arity() \> <ts.nBound>;<}>assert nodeInvariant();
		}
		
		<if (!isPrimitive(ts.keyType)) {><toString(UNCHECKED_ANNOTATION)><}>
		@Override
		<toString(ts.keyType)> getKey(int index) {
			return (<toString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index];
		}
	
		<if (ts.ds == \map()) {>
		<if (!isPrimitive(ts.valType)) {><toString(UNCHECKED_ANNOTATION)><}>
		@Override
		<toString(ts.valType)> getValue(int index) {
			return (<toString(ts.valType)>) nodes[<use(tupleLengthConstant)> * index + 1];
		}
		<}>

		<if (ts.ds == \map()) {>
		<toString(UNCHECKED_ANNOTATION)>
		@Override
		Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)> getKeyValueEntry(int index) {
			return entryOf((<toString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index], (<toString(ts.valType)>) nodes[<use(tupleLengthConstant)> * index + 1]);
		}
		<}>

		<toString(UNCHECKED_ANNOTATION)>
		@Override
		public <CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> getNode(int index) {
			final int offset = <use(tupleLengthConstant)> * payloadArity;
			return (<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)>) nodes[offset + index];
		}

		<implOrOverride(ts.AbstractNode_payloadIterator, 
			"return ArrayKeyValueSupplierIterator.of(nodes, 0, <use(tupleLengthConstant)> * payloadArity);")>

		<toString(UNCHECKED_ANNOTATION)>
		@Override
		Iterator\<<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)>\> nodeIterator() {
			final int offset = <use(tupleLengthConstant)> * payloadArity;

			for (int i = offset; i \< nodes.length - offset; i++) {
				assert ((nodes[i] instanceof <AbstractNode(ts.ds)>) == true);
			}

			return (Iterator) ArrayIterator.of(nodes, offset, nodes.length - offset);
		}

		@Override
		boolean hasPayload() {
			return payloadArity != 0;
		}

		@Override
		int payloadArity() {
			return payloadArity;
		}

		@Override
		boolean hasNodes() {
			return <use(tupleLengthConstant)> * payloadArity != nodes.length;
		}

		@Override
		int nodeArity() {
			return nodes.length - <use(tupleLengthConstant)> * payloadArity;
		}

		<if (isOptionEnabled(ts.setup,useUntypedVariables())) {>
		@Override
		<toString(object())> getSlot(int index) {
			throw new UnsupportedOperationException();
		}
		<}>

		<if (isOptionEnabled(ts.setup, useStructuralEquality())) {>
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 0;
			result = prime * result + (<primitiveHashCode(___valmapMethod(ts.bitPartitionSize))>);
			result = prime * result + (<primitiveHashCode(___valmapMethod(ts.bitPartitionSize))>);
			result = prime * result + Arrays.hashCode(nodes);
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
			<className><QuestionMarkGenerics(ts.ds, ts.tupleTypes)> that = (<className><QuestionMarkGenerics(ts.ds, ts.tupleTypes)>) other;
			if (<use(bitmapMethod)> != that.<use(bitmapMethod)>) {
				return false;
			}
			if (<use(valmapMethod)> != that.<use(valmapMethod)>) {
				return false;
			}
			if (!Arrays.equals(nodes, that.nodes)) {
				return false;
			}
			return true;
		}
		<}>

		@Override
		byte sizePredicate() {
			<if (isOptionEnabled(ts.setup,useSpecialization())) {>return SIZE_MORE_THAN_ONE;<} else {>if (this.nodeArity() == 0 && this.payloadArity == 0) {
				return SIZE_EMPTY;
			} else if (this.nodeArity() == 0 && this.payloadArity == 1) {
				return SIZE_ONE;
			} else {
				return SIZE_MORE_THAN_ONE;
			}<}>
		}

		<implOrOverride(ts.CompactNode_convertToGenericNode, "return this;")>

		<if (ts.ds == \map()) {>
		@Override
		<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> copyAndSetValue(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>, <dec(val(ts.valType))>) {
			<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos) + 1;
			
			if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[<use(field(primitive("int"), "idx"))>] = val;
				return this;
			} else {
				<dec(field(asArray(object()), "src"))> = this.nodes;
				<arraycopyAndSetTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, [val(ts.valType)], field(primitive("int"), "idx"))>
				
				return nodeOf(mutator, <use(bitmapMethod)>, <use(valmapMethod)>, <use(field(asArray(object()), "dst"))>, payloadArity);
			}
		}
		<}>

		@Override
		<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> copyAndSetNode(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>, <CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> node) {
			<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);

			if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[<use(field(primitive("int"), "idx"))>] = node;
				return this;
			} else {
				<dec(field(asArray(object()), "src"))> = this.nodes;
				<arraycopyAndSetTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, [\node(ts.ds, ts.tupleTypes)], field(primitive("int"), "idx"))>
				
				return nodeOf(mutator, <use(bitmapMethod)>, <use(valmapMethod)>, <use(field(asArray(object()), "dst"))>, payloadArity);
			}
		}

		@Override
		<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> copyAndInsertValue(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>, <dec(ts.payloadTuple)>) {
			<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndInsertTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), ts.payloadTuple, field(primitive("int"), "idx"))>
			
			return nodeOf(mutator, <use(bitmapMethod)>, (<toString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<use(valmapMethod)> | bitpos), <use(field(asArray(object()), "dst"))>, (byte) (payloadArity + 1));
		}

		@Override
		<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> copyAndRemoveValue(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>) {
			<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndRemoveTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), field(primitive("int"), "idx"))>

			return nodeOf(mutator, <use(bitmapMethod)>, (<toString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<use(valmapMethod)> ^ bitpos), <use(field(asArray(object()), "dst"))>, (byte) (payloadArity - 1));
		}

		@Override
		<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator,
						<dec(ts.bitposField)>, <CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> node) {
			<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = <use(tupleLengthConstant)> * (payloadArity - 1) + nodeIndex(bitpos);
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndMigrateFromDataTupleToNodeTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), field(primitive("int"), "idxOld"), 1, field(primitive("int"), "idxNew"), [ \node(ts.ds, ts.tupleTypes) ])>

			return nodeOf(mutator, (<toString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<use(bitmapMethod)> | bitpos), (<toString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<use(valmapMethod)> ^ bitpos), <use(field(asArray(object()), "dst"))>, (byte) (payloadArity - 1));
		}

		@Override
		<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator,
						<dec(ts.bitposField)>, <CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> node) {
			<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = dataIndex(bitpos);

			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndMigrateFromNodeTupleToDataTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, field(primitive("int"), "idxOld"), tupleLength(ts.ds), field(primitive("int"), "idxNew"), headPayloadTuple(ts.ds, ts.tupleTypes, \node(ts.ds, ts.tupleTypes, "node")))>

			return nodeOf(mutator, (<toString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<use(bitmapMethod)> ^ bitpos), (<toString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<use(valmapMethod)> | bitpos), <use(field(asArray(object()), "dst"))>, (byte) (payloadArity + 1));
		}
		
		<implOrOverride(ts.CompactNode_removeInplaceValueAndConvertToSpecializedNode, generate_removeInplaceValueAndConvertToSpecializedNode(ts))>		
	'}";
}
	
list[Argument] headPayloadTuple(DataStructure ds:\map(), list[Type] tupleTypes:[keyType, valType, *_], Argument \node) = [ key(keyType, "<use(\node)>.getKey(0)"), val(valType, "<use(\node)>.getValue(0)") ];
list[Argument] headPayloadTuple(DataStructure ds:\set(), list[Type] tupleTypes:[keyType, *_], Argument \node) = [ key(keyType, "<use(\node)>.getKey(0)") ];	

default str generate_removeInplaceValueAndConvertToSpecializedNode(TrieSpecifics ts) =
	"	final int valIndex = dataIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = (<toString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)>);
	'	<dec(ts.valmapField)> = (<toString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)> ^ bitpos);
	'
	'	switch(payloadArity()) { // 0 \<= payloadArity \<= <ts.nBound+1> // or ts.nMax
	'	<for (m <- [1..ts.nBound+2], m <= ts.nMax, n <- [ts.nBound+1-m]) {>case <m>: {
	'		<for (i <- [1..m]) {><dec(key(ts.keyType, i), isFinal = false)>; <if(ts.ds == \map()){><dec(val(ts.valType, i), isFinal = false)>;<}><}>
	'
	'		switch(valIndex) {
	'		<for (i <- [1..m+1]) {>case <i-1>: {
				<for (<j,k> <- zip([1..m], [0..m] - [i-1])) {>
				<use(key(ts.keyType, j))> = getKey(<k>);
				<if(ts.ds == \map()){><use(val(ts.valType, j))> = getValue(<k>);<}><}>
				break;
	'		}<}>default:
	'				throw new IllegalStateException(\"Index out of range.\");
	'		}
	
	'		<for (i <- [1..n+1]) {><dec(\node(ts.ds, ts.tupleTypes, i))> = getNode(<i-1>);<}>
	
	'		return <nodeOf(n, m-1, use(metadataArguments(ts) + typedContentArguments(n, m-1, ts, ts.setup)))>;
			
	
	'	}<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");
	'	}"
	;
