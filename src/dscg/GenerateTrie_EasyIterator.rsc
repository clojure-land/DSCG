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

str generateEasyIteratorClassString(DataStructure ds, rel[Option,bool] setup) = 
	"/**
	 * Iterator that first iterates over inlined-values and then continues depth
	 * first recursively.
	 */
	private static class Trie<toString(ds)>Iterator<Generics(ds)> implements SupplierIterator<SupplierIteratorGenerics(ds)> {

		final Deque\<Iterator\<? extends <CompactNode(ds)>\>\> nodeIteratorStack;
		SupplierIterator<SupplierIteratorGenerics(ds)> valueIterator;

		Trie<toString(ds)>Iterator(<CompactNode(ds)><Generics(ds)> rootNode) {
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
					return true;
				} else {
					if (nodeIteratorStack.isEmpty()) {
						return false;
					} else {
						if (nodeIteratorStack.peek().hasNext()) {
							<CompactNode(ds)><Generics(ds)> innerNode = nodeIteratorStack.peek().next();

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
			if (!hasNext())
				throw new NoSuchElementException();

			return valueIterator.next();
		}

		@Override
		public V get() {
			return valueIterator.get();
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}
	}"
	;