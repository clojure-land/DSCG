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
module dscg::GenerateTrie_Iterator

import dscg::Common;
import util::Math;

str generateIteratorClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = 
	"/**
	 * Iterator skeleton that uses a fixed stack in depth.
	 */
	private static abstract class Abstract<toString(ts.ds)>Iterator<GenericsStr(ts.tupleTypes)> {
		
		private static final int MAX_DEPTH = <ceil(32./ts.bitPartitionSize)>;
		
		protected int currentValueCursor;
		protected int currentValueLength;
		protected Abstract<toString(ts.ds)>Node<GenericsStr(ts.tupleTypes)> currentValueNode;

		private int currentStackLevel = -1;
		private final int[] nodeCursorsAndLengths = new int[MAX_DEPTH * 2];

		<toString(UNCHECKED_ANNOTATION())>
		Abstract<toString(ts.ds)>Node<GenericsStr(ts.tupleTypes)>[] nodes = new Abstract<toString(ts.ds)>Node[MAX_DEPTH];

		Abstract<toString(ts.ds)>Iterator(Abstract<toString(ts.ds)>Node<GenericsStr(ts.tupleTypes)> rootNode) {
			if (rootNode.hasNodes()) {
				currentStackLevel = 0;

				nodes[0] = rootNode;
				nodeCursorsAndLengths[0] = 0;
				nodeCursorsAndLengths[1] = rootNode.nodeArity();					
			}			

			if (rootNode.hasPayload()) {
				currentValueNode = rootNode;
				currentValueCursor = 0;
				currentValueLength = rootNode.payloadArity();		
			}
		}

		/*
		 * search for next node that contains values
		 */
		private boolean searchNextValueNode() {
			while (currentStackLevel \>= 0) {
				final int currentCursorIndex = currentStackLevel * 2;
				final int currentLengthIndex = currentCursorIndex + 1;

				final int nodeCursor = nodeCursorsAndLengths[currentCursorIndex];
				final int nodeLength = nodeCursorsAndLengths[currentLengthIndex];

				if (nodeCursor \< nodeLength) {
					final Abstract<toString(ts.ds)>Node<GenericsStr(ts.tupleTypes)> nextNode = nodes[currentStackLevel]
									.getNode(nodeCursor);
					nodeCursorsAndLengths[currentCursorIndex]++;

					if (nextNode.hasNodes()) {
						/*
						 * put node on next stack level for depth-first
						 * traversal
						 */
						final int nextStackLevel = ++currentStackLevel;
						final int nextCursorIndex = nextStackLevel * 2;
						final int nextLengthIndex = nextCursorIndex + 1;

						nodes[nextStackLevel] = nextNode;
						nodeCursorsAndLengths[nextCursorIndex] = 0;
						nodeCursorsAndLengths[nextLengthIndex] = nextNode.nodeArity();
					}

					if (nextNode.hasPayload()) {
						/*
						 * found next node that contains values
						 */
						currentValueNode = nextNode;
						currentValueCursor = 0;
						currentValueLength = nextNode.payloadArity();
						return true;
					}
				} else {
					currentStackLevel--;
				}
			}

			return false;		
		}

		public boolean hasNext() {
			if (currentValueCursor \< currentValueLength) {
				return true;
			} else {
				return searchNextValueNode();
			}
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}
	}

	protected static class <toString(ts.ds)>KeyIterator<GenericsStr(ts.tupleTypes)> extends Abstract<toString(ts.ds)>Iterator<GenericsStr(ts.tupleTypes)> implements
			<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)><} else {>Iterator\<<typeToString(primitiveToClass(ts.keyType))>\><}> {

		<toString(ts.ds)>KeyIterator(<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> rootNode) {
			super(rootNode);
		}

		@Override
		public <typeToString(primitiveToClass(ts.keyType))> next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			} else {
				return currentValueNode.getKey(currentValueCursor++);
			}
		}

		<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>
		@Override
		public <typeToString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> get() {
			throw new UnsupportedOperationException();
		}<}>		
	}

	<if (\map() := ds) {>
	protected static class <toString(ts.ds)>ValueIterator<GenericsStr(ts.tupleTypes)> extends Abstract<toString(ts.ds)>Iterator<GenericsStr(ts.tupleTypes)> implements
			<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>SupplierIterator<SupplierIteratorGenericsReversed(ts.ds, ts.tupleTypes)><} else {>Iterator\<<typeToString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))>\><}> {

		<toString(ts.ds)>ValueIterator(<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> rootNode) {
			super(rootNode);
		}

		@Override
		public <typeToString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			} else {
				return currentValueNode.getVal(currentValueCursor++);
			}
		}

		<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>
		@Override
		public <typeToString(primitiveToClass(dsAtFunction__domain_type(ts.ds, ts.tupleTypes)))> get() {
			throw new UnsupportedOperationException();
		}<}>
	}<}>

	<if (\map(multi = false) := ds) {>
	protected static class <toString(ts.ds)>EntryIterator<GenericsStr(ts.tupleTypes)> extends Abstract<toString(ts.ds)>Iterator<GenericsStr(ts.tupleTypes)> implements
			<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>SupplierIterator\<Map.Entry<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)>, <typeToString(primitiveToClass(ts.keyType))>\><} else {>Iterator\<Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\><}> {

		<toString(ts.ds)>EntryIterator(<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> rootNode) {
			super(rootNode);
		}

		@Override
		public Map.Entry<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)> next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			} else {
				return currentValueNode.getKeyValueEntry(currentValueCursor++);
			}
		}

		<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>
		@Override
		public <typeToString(primitiveToClass(ts.keyType))> get() {
			throw new UnsupportedOperationException();
		}<}>		
	}
	<}>
	
	<if (\map(multi = true) := ds) {>
	protected static class <toString(ts.ds)>TupleIterator\<K, V, T\> extends Abstract<toString(ts.ds)>Iterator\<K, V\> implements
			<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>SupplierIterator\<Map.Entry<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)>, <typeToString(primitiveToClass(ts.keyType))>\><} else {>Iterator\<T\><}> {

		final BiFunction\<K, V, T\> tupleOf;

		K currentKey = null;
		V currentValue = null;
		Iterator\<V\> currentSetIterator = Collections.emptyIterator();

		<toString(ts.ds)>TupleIterator(<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> rootNode, final BiFunction\<K, V, T\> tupleOf) {
			super(rootNode);
			this.tupleOf = tupleOf;
		}

		public boolean hasNext() {
			if (currentSetIterator.hasNext()) {
				return true; 				
			} else {
				if (super.hasNext()) {
					currentKey = currentValueNode.getKey(currentValueCursor);
					currentSetIterator = currentValueNode.getVal(currentValueCursor).iterator();
					currentValueCursor++;
					
					return true;
				} else {
					return false;
				}
			}
		}

		@Override		
		public T next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			} else {
				currentValue = currentSetIterator.next();
				return tupleOf.apply(currentKey, currentValue);
			}
		}

		<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>
		@Override
		public <typeToString(primitiveToClass(ts.keyType))> get() {
			throw new UnsupportedOperationException();
		}<}>		
	}
	<}>
	"
	;
