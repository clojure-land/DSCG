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
module dscg::GenerateTrie_NodeIterator

import dscg::Common;

str generateNodeIteratorClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix) {

	str nodeIteratorClassName = "Trie<toString(ds)><classNamePostfix>NodeIterator";	

	return 
	"/**
	 * Iterator that first iterates over inlined-values and then continues depth
	 * first recursively.
	 */
	private static class <nodeIteratorClassName><Generics(ts.ds, ts.tupleTypes)> implements Iterator\<<AbstractNode(ds)><Generics(ts.ds, ts.tupleTypes)>\> {

		final Deque\<Iterator\<? extends <AbstractNode(ds)><Generics(ts.ds, ts.tupleTypes)>\>\> nodeIteratorStack;

		<nodeIteratorClassName>(<AbstractNode(ds)><Generics(ts.ds, ts.tupleTypes)> rootNode) {
			nodeIteratorStack = new ArrayDeque\<\>();
			nodeIteratorStack.push(Collections.singleton(rootNode).iterator());
		}

		@Override
		public boolean hasNext() {
			while (true) {
				if (nodeIteratorStack.isEmpty()) {
					return false;
				} else {
					if (nodeIteratorStack.peek().hasNext()) {
						return true;
					} else {
						nodeIteratorStack.pop();
						continue;
					}
				}
			}
		}

		@Override
		public <AbstractNode(ds)><Generics(ts.ds, ts.tupleTypes)> next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			}

			<AbstractNode(ds)><Generics(ts.ds, ts.tupleTypes)> innerNode = nodeIteratorStack.peek().next();

			if (innerNode.hasNodes()) {
				nodeIteratorStack.push(innerNode.nodeIterator());
			}

			return innerNode;
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}
	}"
	;
}