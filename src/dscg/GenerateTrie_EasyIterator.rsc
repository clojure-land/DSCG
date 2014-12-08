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
module dscg::GenerateTrie_EasyIterator

import dscg::Common;

str generateEasyIteratorClassString(TrieSpecifics ts, rel[Option,bool] setup) = 
	"/**
	 * Iterator that first iterates over inlined-values and then continues depth
	 * first recursively.
	 */
	private static class Trie<toString(ts.ds)><ts.classNamePostfix>Iterator<Generics(ts.ds, ts.tupleTypes)> implements SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)> {

		final Deque\<Iterator\<? extends <AbstractNode(ts.ds)>\>\> nodeIteratorStack;
		SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)> valueIterator;

		private boolean hasNext = false;

		Trie<toString(ts.ds)><ts.classNamePostfix>Iterator(<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> rootNode) {
			if (rootNode.hasPayload()) {
				valueIterator = rootNode.payloadIterator();
			} else {
				valueIterator = EmptySupplierIterator.emptyIterator();
			}

			nodeIteratorStack = new ArrayDeque\<\>();
			if (rootNode.hasNodes()) {
				nodeIteratorStack.push(rootNode.nodeIterator());
			}
		}

		@Override
		public boolean hasNext() {
			while (true) {
				if (valueIterator.hasNext()) {
					return hasNext = true;
				} else {
					if (nodeIteratorStack.isEmpty()) {
						return hasNext = false;
					} else {
						if (nodeIteratorStack.peek().hasNext()) {
							<AbstractNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> innerNode = nodeIteratorStack.peek().next();

							if (innerNode.hasPayload())
								valueIterator = innerNode.payloadIterator();

							if (innerNode.hasNodes()) {
								nodeIteratorStack.push(innerNode.nodeIterator());
							}
							continue;
						} else {
							nodeIteratorStack.pop();
							continue;
						}
					}
				}
			}
		}

		@Override
		public K next() {
			if (!hasNext)
				throw new NoSuchElementException();

			return valueIterator.next();
		}

		@Override
		public <toString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> get() {
			return valueIterator.get();
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}
	}"
	;