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

str generateBitmapIndexedNodeClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) {

	className = "BitmapIndexed<toString(ds)>Node";

	return
	"private static final class <className><Generics(ds)> extends <className_compactNode(ts, setup, true, true)><Generics(ds)> {
		private AtomicReference\<Thread\> mutator;

		private Object[] nodes;
		final private byte payloadArity;
		
		<className>(AtomicReference\<Thread\> mutator, <dec(___bitmapField(bitPartitionSize))>, <dec(___valmapField(bitPartitionSize))>, Object[] nodes, byte payloadArity) {
			super(mutator, <use(bitmapField)>, <use(valmapField)>);
			
			assert (<use(tupleLengthConstant)> * <integerOrLongObject(bitPartitionSize)>.bitCount(<useSafeUnsigned(___valmapField(bitPartitionSize))>) + <integerOrLongObject(bitPartitionSize)>.bitCount(<useSafeUnsigned(___bitmapField(bitPartitionSize))>) == nodes.length);

			this.mutator = mutator;

			this.nodes = nodes;
			this.payloadArity = payloadArity;

			assert (payloadArity == <integerOrLongObject(bitPartitionSize)>.bitCount(<useSafeUnsigned(___valmapField(bitPartitionSize))>));
			// assert (payloadArity() \>= 2 || nodeArity() \>= 1); // =
			// // SIZE_MORE_THAN_ONE

			// for (int i = 0; i \< <use(tupleLengthConstant)> * payloadArity; i++)
			// assert ((nodes[i] instanceof CompactNode) == false);
			//
			// for (int i = <use(tupleLengthConstant)> * payloadArity; i \< nodes.length; i++)
			// assert ((nodes[i] instanceof CompactNode) == true);

			// assert invariant
			<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {>assert arity() \> <nBound>;<}>assert nodeInvariant();
		}
		
		<if (!isPrimitive(key())) {>@SuppressWarnings(\"unchecked\")<}>
		@Override
		<toString(key().\type)> getKey(int index) {
			return (<toString(key().\type)>) nodes[<use(tupleLengthConstant)> * index];
		}
	
		<if (ds == \map()) {>
		<if (!isPrimitive(val())) {>@SuppressWarnings(\"unchecked\")<}>
		@Override
		<toString(val().\type)> getValue(int index) {
			return (<toString(val().\type)>) nodes[<use(tupleLengthConstant)> * index + 1];
		}
		<}>

		<if (ds == \map()) {>
		@SuppressWarnings(\"unchecked\")
		@Override
		Map.Entry<GenericsExpanded(ds)> getKeyValueEntry(int index) {
			return entryOf((<toString(key().\type)>) nodes[<use(tupleLengthConstant)> * index], (<toString(val().\type)>) nodes[<use(tupleLengthConstant)> * index + 1]);
		}
		<}>

		@SuppressWarnings(\"unchecked\")
		@Override
		public <CompactNode(ds)><Generics(ds)> getNode(int index) {
			final int offset = <use(tupleLengthConstant)> * payloadArity;
			return (<CompactNode(ds)><Generics(ds)>) nodes[offset + index];
		}

		@Override
		SupplierIterator<SupplierIteratorGenerics(ds)> payloadIterator() {
			return ArrayKeyValueIterator.of(nodes, 0, <use(tupleLengthConstant)> * payloadArity);
		}

		@SuppressWarnings(\"unchecked\")
		@Override
		Iterator\<<CompactNode(ds)><Generics(ds)>\> nodeIterator() {
			final int offset = <use(tupleLengthConstant)> * payloadArity;

			for (int i = offset; i \< nodes.length - offset; i++) {
				assert ((nodes[i] instanceof <AbstractNode(ds)>) == true);
			}

			return (Iterator) ArrayIterator.of(nodes, offset, nodes.length - offset);
		}

		<if (!isPrimitive(key())) {>@SuppressWarnings(\"unchecked\")<}>
		@Override
		<toString(key().\type)> headKey() {
			assert hasPayload();
			return (<toString(key().\type)>) nodes[0];
		}

		<if (ds == \map()) {>
		<if (!isPrimitive(val())) {>@SuppressWarnings(\"unchecked\")<}>
		@Override
		<toString(val().\type)> headVal() {
			assert hasPayload();
			return (<toString(val().\type)>) nodes[1];
		}
		<}>

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

		<if (isOptionEnabled(setup,useUntypedVariables())) {>
		@Override
		<toString(object())> getSlot(int index) {
			throw new UnsupportedOperationException();
		}
		<}>

		<if (isOptionEnabled(setup, useStructuralEquality())) {>
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 0;
			result = prime * result + (<primitiveHashCode(___valmapMethod(bitPartitionSize))>);
			result = prime * result + (<primitiveHashCode(___valmapMethod(bitPartitionSize))>);
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
			<className><QuestionMarkGenerics(ds)> that = (<className><QuestionMarkGenerics(ds)>) other;
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
			<if (isOptionEnabled(setup,useSpecialization())) {>return SIZE_MORE_THAN_ONE;<} else {>if (this.nodeArity() == 0 && this.payloadArity == 0) {
				return SIZE_EMPTY;
			} else if (this.nodeArity() == 0 && this.payloadArity == 1) {
				return SIZE_ONE;
			} else {
				return SIZE_MORE_THAN_ONE;
			}<}>
		}

		<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {>
		@Override
		<CompactNode(ds)><Generics(ds)> convertToGenericNode() {
			return this;
		}
		<}>

		<if (ds == \map()) {>
		@Override
		<CompactNode(ds)><Generics(ds)> copyAndSetValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <dec(val())>) {
			<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos) + 1;
			
			if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[<use(field(primitive("int"), "idx"))>] = val;
				return this;
			} else {
				<dec(field(asArray(object()), "src"))> = this.nodes;
				<arraycopyAndSetTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, [val()], field(primitive("int"), "idx"))>
				
				return nodeOf(mutator, <use(bitmapMethod)>, <use(valmapMethod)>, <use(field(asArray(object()), "dst"))>, payloadArity);
			}
		}
		<}>

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndSetNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> node) {
			<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);

			if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[<use(field(primitive("int"), "idx"))>] = node;
				return this;
			} else {
				<dec(field(asArray(object()), "src"))> = this.nodes;
				<arraycopyAndSetTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, [\node(ds)], field(primitive("int"), "idx"))>
				
				return nodeOf(mutator, <use(bitmapMethod)>, <use(valmapMethod)>, <use(field(asArray(object()), "dst"))>, payloadArity);
			}
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndInsertValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <dec(payloadTuple(ts, setup))>) {
			<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndInsertTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ds), payloadTuple(ts, setup), field(primitive("int"), "idx"))>
			
			return nodeOf(mutator, <use(bitmapMethod)>, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (<use(valmapMethod)> | bitpos), <use(field(asArray(object()), "dst"))>, (byte) (payloadArity + 1));
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndRemoveValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>) {
			<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndRemoveTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ds), field(primitive("int"), "idx"))>

			return nodeOf(mutator, <use(bitmapMethod)>, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (<use(valmapMethod)> ^ bitpos), <use(field(asArray(object()), "dst"))>, (byte) (payloadArity - 1));
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator,
						<dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> node) {
			<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = <use(tupleLengthConstant)> * (payloadArity - 1) + nodeIndex(bitpos);
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndMigrateFromDataTupleToNodeTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ds), field(primitive("int"), "idxOld"), 1, field(primitive("int"), "idxNew"), [ \node(ds) ])>

			return nodeOf(mutator, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (<use(bitmapMethod)> | bitpos), (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (<use(valmapMethod)> ^ bitpos), <use(field(asArray(object()), "dst"))>, (byte) (payloadArity - 1));
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator,
						<dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> node) {
			<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = dataIndex(bitpos);

			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndMigrateFromNodeTupleToDataTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, field(primitive("int"), "idxOld"), tupleLength(ds), field(primitive("int"), "idxNew"), headPayloadTuple(ts, setup, \node(ds, "node")))>

			return nodeOf(mutator, (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (<use(bitmapMethod)> ^ bitpos), (<toString(chunkSizeToPrimitive(bitPartitionSize))>) (<use(valmapMethod)> | bitpos), <use(field(asArray(object()), "dst"))>, (byte) (payloadArity + 1));
		}
	'}";
}
	
list[Argument] headPayloadTuple(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, Argument \node) = [ key("<use(\node)>.headKey()"), val("<use(\node)>.headVal()") ];
list[Argument] headPayloadTuple(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, Argument \node) = [ key("<use(\node)>.headKey()") ];	