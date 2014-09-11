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
	private static abstract class Abstract<toString(ds)>Iterator<Generics(ds)> {
		
		// TODO: verify maximum deepness
		private static final int MAX_DEPTH = <2 + ceil(32/bitPartitionSize)>;
		
		protected int currentValueCursor;
		protected int currentValueLength;
		protected Abstract<toString(ds)>Node<Generics(ds)> currentValueNode;

		private int currentStackLevel;
		private final int[] nodeCursorsAndLengths = new int[MAX_DEPTH * 2];

		@SuppressWarnings(\"unchecked\")
		Abstract<toString(ds)>Node<Generics(ds)>[] nodes = new Abstract<toString(ds)>Node[MAX_DEPTH];

		Abstract<toString(ds)>Iterator(Abstract<toString(ds)>Node<Generics(ds)> rootNode) {
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
						final Abstract<toString(ds)>Node<Generics(ds)> nextNode = nodes[currentStackLevel]
										.getNode(nodeCursor);
						nodeCursorsAndLengths[currentCursorIndex]++;

						final int nextValueLength = nextNode.payloadArity();
						final int nextNodeLength = nextNode.nodeArity();

						if (nextNodeLength \> 0) {
							/*
							 * put node on next stack level for depth-first
							 * traversal
							 */
							final int nextStackLevel = ++currentStackLevel;
							final int nextCursorIndex = nextStackLevel * 2;
							final int nextLengthIndex = nextCursorIndex + 1;

							nodes[nextStackLevel] = nextNode;
							nodeCursorsAndLengths[nextCursorIndex] = 0;
							nodeCursorsAndLengths[nextLengthIndex] = nextNodeLength;
						}

						if (nextValueLength != 0) {
							/*
							 * found for next node that contains values
							 */
							currentValueNode = nextNode;
							currentValueCursor = 0;
							currentValueLength = nextValueLength;
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
	
	private static final class <toString(ds)>KeyIterator<Generics(ds)> extends Abstract<toString(ds)>Iterator<Generics(ds)> implements
					SupplierIterator<SupplierIteratorGenerics(ds)> {

		<toString(ds)>KeyIterator(<AbstractNode(ds)><Generics(ds)> rootNode) {
			super(rootNode);
		}

		@Override
		public <toString(primitiveToClass(key().\type))> next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			} else {
				return currentValueNode.getKey(currentValueCursor++);
			}
		}

		@Override
		public <toString(primitiveToClass(dsAtFunction__range_type(ds)))> get() {
			throw new UnsupportedOperationException();
		}
	}

	<if (ds == \map()) {>
	private static final class <toString(ds)>ValueIterator<Generics(ds)> extends Abstract<toString(ds)>Iterator<Generics(ds)> implements
					SupplierIterator<SupplierIteratorGenericsReversed(ds)> {

		<toString(ds)>ValueIterator(<AbstractNode(ds)><Generics(ds)> rootNode) {
			super(rootNode);
		}

		@Override
		public <toString(primitiveToClass(val().\type))> next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			} else {
				return currentValueNode.getValue(currentValueCursor++);
			}
		}

		@Override
		public <toString(primitiveToClass(key().\type))> get() {
			throw new UnsupportedOperationException();
		}
	}

	private static final class <toString(ds)>EntryIterator<Generics(ds)> extends Abstract<toString(ds)>Iterator<Generics(ds)> implements
					SupplierIterator\<Map.Entry<GenericsExpanded(ds)>, <toString(primitiveToClass(key().\type))>\> {

		<toString(ds)>EntryIterator(<AbstractNode(ds)><Generics(ds)> rootNode) {
			super(rootNode);
		}

		@Override
		public Map.Entry<GenericsExpanded(ds)> next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			} else {
				return currentValueNode.getKeyValueEntry(currentValueCursor++);
			}
		}

		@Override
		public <toString(primitiveToClass(key().\type))> get() {
			throw new UnsupportedOperationException();
		}
	}
	<}>
	"
	;