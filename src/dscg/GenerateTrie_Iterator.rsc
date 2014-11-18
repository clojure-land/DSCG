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
	private static abstract class Abstract<toString(ts.ds)>Iterator<Generics(ts.ds, ts.tupleTypes)> {
		
		// TODO: verify maximum deepness
		private static final int MAX_DEPTH = <2 + ceil(32/bitPartitionSize)>;
		
		protected int currentValueCursor;
		protected int currentValueLength;
		protected <AbstractNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> currentValueNode;

		private int currentStackLevel;
		private final int[] nodeCursorsAndLengths = new int[MAX_DEPTH * 2];

		<toString(UNCHECKED_ANNOTATION())>
		<AbstractNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)>[] nodes = new <AbstractNode(ts.ds)>[MAX_DEPTH];

		Abstract<toString(ts.ds)>Iterator(<AbstractNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> rootNode) {
			currentStackLevel = 0;

			currentValueNode = rootNode;
			currentValueCursor = 0;
			currentValueLength = rootNode.payloadArity();

			nodes[0] = rootNode;
			nodeCursorsAndLengths[0] = 0;
			nodeCursorsAndLengths[1] = rootNode.nodeArity();
		}

		public boolean hasNext() {
			if (currentValueCursor \< currentValueLength) {
				return true;
			} else {
				/*
				 * search for next node that contains values
				 */
				while (currentStackLevel \>= 0) {
					final int currentCursorIndex = currentStackLevel * 2;
					final int currentLengthIndex = currentCursorIndex + 1;

					final int nodeCursor = nodeCursorsAndLengths[currentCursorIndex];
					final int nodeLength = nodeCursorsAndLengths[currentLengthIndex];

					if (nodeCursor \< nodeLength) {
						final <AbstractNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> nextNode = nodes[currentStackLevel]
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
			}

			return false;
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}
	}
	
	private static final class <toString(ts.ds)>KeyIterator<Generics(ts.ds, ts.tupleTypes)> extends Abstract<toString(ts.ds)>Iterator<Generics(ts.ds, ts.tupleTypes)> implements
					SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)> {

		<toString(ts.ds)>KeyIterator(<AbstractNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> rootNode) {
			super(rootNode);
		}

		@Override
		public <toString(primitiveToClass(ts.keyType))> next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			} else {
				return currentValueNode.getKey(currentValueCursor++);
			}
		}

		@Override
		public <toString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> get() {
			throw new UnsupportedOperationException();
		}
	}

	<if (ds == \map()) {>
	private static final class <toString(ts.ds)>ValueIterator<Generics(ts.ds, ts.tupleTypes)> extends Abstract<toString(ts.ds)>Iterator<Generics(ts.ds, ts.tupleTypes)> implements
					SupplierIterator<SupplierIteratorGenericsReversed(ts.ds, ts.tupleTypes)> {

		<toString(ts.ds)>ValueIterator(<AbstractNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> rootNode) {
			super(rootNode);
		}

		@Override
		public <toString(primitiveToClass(ts.valType))> next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			} else {
				return currentValueNode.getValue(currentValueCursor++);
			}
		}

		@Override
		public <toString(primitiveToClass(ts.keyType))> get() {
			throw new UnsupportedOperationException();
		}
	}

	private static final class <toString(ts.ds)>EntryIterator<Generics(ts.ds, ts.tupleTypes)> extends Abstract<toString(ts.ds)>Iterator<Generics(ts.ds, ts.tupleTypes)> implements
					SupplierIterator\<Map.Entry<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)>, <toString(primitiveToClass(ts.keyType))>\> {

		<toString(ts.ds)>EntryIterator(<AbstractNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> rootNode) {
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

		@Override
		public <toString(primitiveToClass(ts.keyType))> get() {
			throw new UnsupportedOperationException();
		}
	}
	<}>
	"
	;