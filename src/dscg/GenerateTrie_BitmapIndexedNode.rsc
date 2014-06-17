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

import dscg::GenerateTrie;

str generateBitmapIndexedNodeClassString(DataStructure ds, set[Option] setup) {

	className = "BitmapIndexed<toString(ds)>Node";

	return
	"private static final class <className><Generics()> extends Compact<toString(ds)>Node<Generics()> {
		private AtomicReference\<Thread\> mutator;

		private Object[] nodes;
//		final private int bitmap;
//		final private int valmap;
		final private byte payloadArity;

		BitmapIndexedMapNode(AtomicReference\<Thread\> mutator, int bitmap, int valmap,
						Object[] nodes, byte payloadArity) {
			super(mutator, bitmap, valmap);
			
			assert (2 * Integer.bitCount(valmap) + Integer.bitCount(bitmap ^ valmap) == nodes.length);

			this.mutator = mutator;

			this.nodes = nodes;
//			this.bitmap = bitmap;
//			this.valmap = valmap;
			this.payloadArity = payloadArity;

			assert (payloadArity == Integer.bitCount(valmap));
			// assert (payloadArity() \>= 2 || nodeArity() \>= 1); // =
			// // SIZE_MORE_THAN_ONE

			// for (int i = 0; i \< 2 * payloadArity; i++)
			// assert ((nodes[i] instanceof CompactNode) == false);
			//
			// for (int i = 2 * payloadArity; i \< nodes.length; i++)
			// assert ((nodes[i] instanceof CompactNode) == true);

			// assert invariant
			assert nodeInvariant();
		}
		
		@SuppressWarnings(\"unchecked\")
		@Override
		K getKey(int index) {
			return (K) nodes[2 * index];
		}

		@SuppressWarnings(\"unchecked\")
		@Override
		V getValue(int index) {
			return (V) nodes[2 * index + 1];
		}

		@SuppressWarnings(\"unchecked\")
		@Override
		Map.Entry<Generics()> getKeyValueEntry(int index) {
			return entryOf((K) nodes[2 * index], (V) nodes[2 * index + 1]);
		}

		@SuppressWarnings(\"unchecked\")
		@Override
		public <CompactNode()><Generics()> getNode(int index) {
			final int offset = 2 * payloadArity;
			return (<CompactNode()><Generics()>) nodes[offset + index];
		}

		@Override
		SupplierIterator<Generics()> payloadIterator() {
			return ArrayKeyValueIterator.of(nodes, 0, 2 * payloadArity);
		}

		@SuppressWarnings(\"unchecked\")
		@Override
		Iterator\<<CompactNode()><Generics()>\> nodeIterator() {
			final int offset = 2 * payloadArity;

			for (int i = offset; i \< nodes.length - offset; i++) {
				assert ((nodes[i] instanceof AbstractMapNode) == true);
			}

			return (Iterator) ArrayIterator.of(nodes, offset, nodes.length - offset);
		}

		@SuppressWarnings(\"unchecked\")
		@Override
		K headKey() {
			assert hasPayload();
			return (K) nodes[0];
		}

		@SuppressWarnings(\"unchecked\")
		@Override
		V headVal() {
			assert hasPayload();
			return (V) nodes[1];
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
			return 2 * payloadArity != nodes.length;
		}

		@Override
		int nodeArity() {
			return nodes.length - 2 * payloadArity;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 0;
			result = prime * result + bitmap;
			result = prime * result + valmap;
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
			BitmapIndexedMapNode\<?, ?\> that = (BitmapIndexedMapNode\<?, ?\>) other;
			if (bitmap != that.bitmap) {
				return false;
			}
			if (valmap != that.valmap) {
				return false;
			}
			if (!Arrays.equals(nodes, that.nodes)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			final StringBuilder bldr = new StringBuilder();
			bldr.append(\'[\');

			for (byte i = 0; i \< payloadArity(); i++) {
				final byte pos = recoverMask(valmap, (byte) (i + 1));
				bldr.append(String.format(\"@%d: %s=%s\", pos, getKey(i), getValue(i)));

				if (!((i + 1) == payloadArity())) {
					bldr.append(\", \");
				}
			}

			if (payloadArity() \> 0 && nodeArity() \> 0) {
				bldr.append(\", \");
			}

			for (byte i = 0; i \< nodeArity(); i++) {
				final byte pos = recoverMask(bitmap ^ valmap, (byte) (i + 1));
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
			<if ({_*, useSpecialization()} := setup) {>return SIZE_MORE_THAN_ONE;<} else {>if (this.nodeArity() == 0 && this.payloadArity == 0) {
				return SIZE_EMPTY;
			} else if (this.nodeArity() == 0 && this.payloadArity == 1) {
				return SIZE_ONE;
			} else {
				return SIZE_MORE_THAN_ONE;
			}<}>
		}

		@Override
		<CompactNode()><Generics()> convertToGenericNode() {
			return this;
		}

		@Override
		<CompactNode()><Generics()> copyAndSetValue(AtomicReference\<Thread\> mutator, int index, V val) {
			final <CompactNode()><Generics()> thisNew;
			final int valIndex = 2 * index;
			
			if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[valIndex + 1] = val;
				thisNew = this;
			} else {
				final Object[] editableNodes = copyAndSet(this.nodes, valIndex + 1, val);

				thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, bitmap, valmap,
								editableNodes, payloadArity);
			}
			
			return thisNew;
		}

		@Override
		<CompactNode()><Generics()> copyAndInsertValue(AtomicReference\<Thread\> mutator, int bitpos, K key,
						V val) {			
			final int valIndex = 2 * Integer.bitCount(valmap & (bitpos - 1));
			final Object[] editableNodes = copyAndInsertPair(this.nodes, valIndex, key, val);
			
			final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, bitmap
							| bitpos, valmap | bitpos, editableNodes, (byte) (payloadArity + 1));

			return thisNew;
		}

		@Override
		<CompactNode()><Generics()> copyAndRemoveValue(AtomicReference\<Thread\> mutator, int bitpos) {
			final int valIndex = 2 * Integer.bitCount(valmap & (bitpos - 1));
			final Object[] editableNodes = copyAndRemovePair(this.nodes, valIndex);

			final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(
							mutator, this.bitmap & ~bitpos, this.valmap & ~bitpos,
							editableNodes, (byte) (payloadArity - 1));

			return thisNew;
		}

		@Override
		<CompactNode()><Generics()> copyAndSetNode(AtomicReference\<Thread\> mutator, int index,
						<CompactNode()><Generics()> node) {
			final int bitIndex = 2 * payloadArity + index;
			final <CompactNode()><Generics()> thisNew;

			// modify current node (set replacement node)
			if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[bitIndex] = node;
				thisNew = this;
			} else {
				final Object[] editableNodes = copyAndSet(this.nodes, bitIndex,
								node);

				thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, bitmap, valmap,
								editableNodes, payloadArity);
			}

			return thisNew;
		}

		@Override
		<CompactNode()><Generics()> copyAndRemoveNode(AtomicReference\<Thread\> mutator, int bitpos) {
			final int bitIndex = 2 * payloadArity + Integer.bitCount((bitmap ^ valmap) & (bitpos - 1));
			final Object[] editableNodes = copyAndRemovePair(this.nodes, bitIndex);

			final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(
							mutator, bitmap & ~bitpos, valmap, editableNodes,
							payloadArity);

			return thisNew;
		}

		@Override
		<CompactNode()><Generics()> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator,
						int bitpos, <CompactNode()><Generics()> node) {
//			final int bitIndex = 2 * payloadArity + Integer.bitCount((bitmap ^ valmap) & (bitpos - 1));
			final int valIndex = 2 * Integer.bitCount(valmap & (bitpos - 1));
			
			final int offset = 2 * (payloadArity - 1);
			final int index = Integer.bitCount(((bitmap | bitpos) ^ (valmap & ~bitpos))
							& (bitpos - 1));

			final Object[] editableNodes = copyAndMoveToBackPair(this.nodes, valIndex, offset
							+ index, node);

			final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(mutator, bitmap
							| bitpos, valmap & ~bitpos, editableNodes, (byte) (payloadArity - 1));

			return thisNew;
		}

		@Override
		<CompactNode()><Generics()> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator,
						int bitpos, <CompactNode()><Generics()> node) {
			final int bitIndex = 2 * payloadArity + Integer.bitCount((bitmap ^ valmap) & (bitpos - 1));
			final int valIndexNew = Integer.bitCount((valmap | bitpos) & (bitpos - 1));

			final Object[] editableNodes = copyAndMoveToFrontPair(this.nodes, bitIndex,
							valIndexNew, node.headKey(), node.headVal());

			final <CompactNode()><Generics()> thisNew = <CompactNode()>.<Generics()> valNodeOf(
							mutator, bitmap, valmap | bitpos, editableNodes,
							(byte) (payloadArity + 1));

			return thisNew;
		}
	'}";
}
