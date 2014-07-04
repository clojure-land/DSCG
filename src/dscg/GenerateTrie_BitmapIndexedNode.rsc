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

Argument tupleLengthConstant = field("int", "TUPLE_LENGTH");

str generateBitmapIndexedNodeClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) {

	className = "BitmapIndexed<toString(ds)>Node";

	return
	"private static final class <className><Generics(ds)> extends <className_compactNode(ts, setup, true, true)><Generics(ds)> {
		private AtomicReference\<Thread\> mutator;

		private Object[] nodes;
		final private byte payloadArity;
		
		private static final int TUPLE_LENGTH = <tupleLength(ds)>;

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
			assert nodeInvariant();
		}
		
		<if (!isPrimitive(key())) {>@SuppressWarnings(\"unchecked\")<}>
		@Override
		<key().\type> getKey(int index) {
			return (<key().\type>) nodes[<use(tupleLengthConstant)> * index];
		}
	
		<if (ds == \map()) {>
		<if (!isPrimitive(val())) {>@SuppressWarnings(\"unchecked\")<}>
		@Override
		<val().\type> getValue(int index) {
			return (<val().\type>) nodes[<use(tupleLengthConstant)> * index + 1];
		}
		<}>

		<if (ds == \map()) {>
		@SuppressWarnings(\"unchecked\")
		@Override
		Map.Entry<GenericsExpanded(ds)> getKeyValueEntry(int index) {
			return entryOf((<key().\type>) nodes[<use(tupleLengthConstant)> * index], (<val().\type>) nodes[<use(tupleLengthConstant)> * index + 1]);
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
		<key().\type> headKey() {
			assert hasPayload();
			return (<key().\type>) nodes[0];
		}

		<if (ds == \map()) {>
		<if (!isPrimitive(val())) {>@SuppressWarnings(\"unchecked\")<}>
		@Override
		<val().\type> headVal() {
			assert hasPayload();
			return (<val().\type>) nodes[1];
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
		public String toString() {
			final StringBuilder bldr = new StringBuilder();
			bldr.append(\'[\');

			for (byte i = 0; i \< payloadArity(); i++) {
				final byte pos = recoverMask(<use(valmapMethod)>, (byte) (i + 1));
				bldr.append(String.format(\"@%d: <intercalate("=", times("%s", size(payloadTuple(ts))))>\", pos, <use(invoke_get_for_payloadTuple(ts, field("i")))>));

				if (!((i + 1) == payloadArity())) {
					bldr.append(\", \");
				}
			}

			if (payloadArity() \> 0 && nodeArity() \> 0) {
				bldr.append(\", \");
			}

			for (byte i = 0; i \< nodeArity(); i++) {
				final byte pos = recoverMask(<use(bitmapMethod)>, (byte) (i + 1));
				bldr.append(String.format(\"@%d: %s\", pos, getNode(i)));

				if (!((i + 1) == nodeArity())) {
					bldr.append(\", \");
				}
			}

			bldr.append(\']\');
			return bldr.toString();
		}

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
			<dec(field("int", "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos) + 1;
			
			if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[<use(field("int", "idx"))>] = val;
				return this;
			} else {
				<dec(field("Object[]", "src"))> = this.nodes;
				<arraycopyAndSetTuple(field("Object[]", "src"), field("Object[]", "dst"), 1, [val()], field("int", "idx"))>
				
				return nodeOf(mutator, <use(bitmapMethod)>, <use(valmapMethod)>, <use(field("Object[]", "dst"))>, payloadArity);
			}
		}
		<}>

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndSetNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> node) {
			<dec(field("int", "idx"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);

			if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[<use(field("int", "idx"))>] = node;
				return this;
			} else {
				<dec(field("Object[]", "src"))> = this.nodes;
				<arraycopyAndSetTuple(field("Object[]", "src"), field("Object[]", "dst"), 1, [\node(ds)], field("int", "idx"))>
				
				return nodeOf(mutator, <use(bitmapMethod)>, <use(valmapMethod)>, <use(field("Object[]", "dst"))>, payloadArity);
			}
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndInsertValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <dec(payloadTuple(ts))>) {
			<dec(field("int", "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			
			<dec(field("Object[]", "src"))> = this.nodes;
			<arraycopyAndInsertTuple(field("Object[]", "src"), field("Object[]", "dst"), tupleLength(ds), payloadTuple(ts), field("int", "idx"))>
			
			return nodeOf(mutator, <use(bitmapMethod)>, (<chunkSizeToPrimitive(bitPartitionSize)>) (<use(valmapMethod)> | bitpos), <use(field("Object[]", "dst"))>, (byte) (payloadArity + 1));
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndRemoveNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>) {
			<dec(field("int", "idx"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);
			
			<dec(field("Object[]", "src"))> = this.nodes;
			<arraycopyAndRemoveTuple(field("Object[]", "src"), field("Object[]", "dst"), 1, field("int", "idx"))>
			
			return nodeOf(mutator, (<chunkSizeToPrimitive(bitPartitionSize)>) (<use(bitmapMethod)> ^ bitpos), <use(valmapMethod)>, <use(field("Object[]", "dst"))>, payloadArity);
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndRemoveValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>) {
			<dec(field("int", "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			
			<dec(field("Object[]", "src"))> = this.nodes;
			<arraycopyAndRemoveTuple(field("Object[]", "src"), field("Object[]", "dst"), tupleLength(ds), field("int", "idx"))>

			return nodeOf(mutator, <use(bitmapMethod)>, (<chunkSizeToPrimitive(bitPartitionSize)>) (<use(valmapMethod)> ^ bitpos), <use(field("Object[]", "dst"))>, (byte) (payloadArity - 1));
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator,
						<dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> node) {
			<dec(field("int", "idxOld"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			<dec(field("int", "idxNew"))> = <use(tupleLengthConstant)> * (payloadArity - 1) + nodeIndex(bitpos);
			
			<dec(field("Object[]", "src"))> = this.nodes;
			<arraycopyAndMigrateFromDataTupleToNodeTuple(field("Object[]", "src"), field("Object[]", "dst"), tupleLength(ds), field("int", "idxOld"), 1, field("int", "idxNew"), [ \node(ds) ])>

			return nodeOf(mutator, (<chunkSizeToPrimitive(bitPartitionSize)>) (<use(bitmapMethod)> | bitpos), (<chunkSizeToPrimitive(bitPartitionSize)>) (<use(valmapMethod)> ^ bitpos), <use(field("Object[]", "dst"))>, (byte) (payloadArity - 1));
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator,
						<dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> node) {
			<dec(field("int", "idxOld"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);
			<dec(field("int", "idxNew"))> = dataIndex(bitpos);

			<dec(field("Object[]", "src"))> = this.nodes;
			<arraycopyAndMigrateFromNodeTupleToDataTuple(field("Object[]", "src"), field("Object[]", "dst"), 1, field("int", "idxOld"), tupleLength(ds), field("int", "idxNew"), headPayloadTuple(ts, \node(ds, "node")))>

			return nodeOf(mutator, (<chunkSizeToPrimitive(bitPartitionSize)>) (<use(bitmapMethod)> ^ bitpos), (<chunkSizeToPrimitive(bitPartitionSize)>) (<use(valmapMethod)> | bitpos), <use(field("Object[]", "dst"))>, (byte) (payloadArity + 1));
		}
	'}";
}

// TODO: move to List.rsc?
list[&T] times(&T template, int count) 
	= [ template | i <- [1..count]];
	
list[Argument] invoke_get_for_payloadTuple(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound), Argument idx) = [ key("getKey(<use(idx)>)"), val("getValue(<use(idx)>)") ];
list[Argument] invoke_get_for_payloadTuple(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), Argument idx) = [ key("getKey(<use(idx)>)") ];

list[Argument] headPayloadTuple(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound), Argument \node) = [ key("<use(\node)>.headKey()"), val("<use(\node)>.headVal()") ];
list[Argument] headPayloadTuple(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), Argument \node) = [ key("<use(\node)>.headKey()") ];	