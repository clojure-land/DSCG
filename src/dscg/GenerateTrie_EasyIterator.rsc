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
import util::Math;

str generateEasyIteratorClassString(TrieSpecifics ts) = 
	"/**
	 * Iterator that first iterates over inlined-values and then continues depth
	 * first recursively.
	 */
	private static class Trie<toString(ts.ds)><ts.classNamePostfix>Iterator<GenericsStr(ts.tupleTypes)> implements 
		<if (isOptionEnabled(ts, useSupplierIterator())) {>SupplierIterator<SupplierIteratorGenericsStr(ts)><} else {>Iterator\<<typeToString(expandedExactBoundCollectionTypes(ts)[0])>\><}> {

		Iterator\<? extends <AbstractNode(ts.ds)>\>[] nodeIteratorStack = null;
		int peek = -1;

		<if (isOptionEnabled(ts, useSupplierIterator())) {>SupplierIterator<SupplierIteratorGenericsStr(ts)><} else {>Iterator\<<typeToString(expandedExactBoundCollectionTypes(ts)[0])>\><}> currentValueIterator = null;
		Iterator\<? extends <AbstractNode(ts.ds)>\> currentNodeIterator = null;

		Trie<toString(ts.ds)><ts.classNamePostfix>Iterator(<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> rootNode) {
			if (rootNode.hasNodes()) {
				nodeIteratorStack = new Iterator[<2 + ceil(32/ts.bitPartitionSize)>];
			
				currentNodeIterator = rootNode.nodeIterator();
				peek += 1;
				nodeIteratorStack[peek] = currentNodeIterator;
			}

			if (rootNode.hasPayload()) {
				currentValueIterator = rootNode.payloadIterator();
			}
		}

		@Override
		public boolean hasNext() {
			if (currentValueIterator != null && currentValueIterator.hasNext()) {
				return true;
			} else {
				return searchNextValueIterator();
			}
		}
		
		private boolean searchNextValueIterator() {
			while (true) {
				if (currentNodeIterator != null && currentNodeIterator.hasNext()) {
					<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> innerNode = currentNodeIterator.next();

					if (innerNode.hasNodes()) {
						currentNodeIterator = innerNode.nodeIterator();
						peek += 1;
						nodeIteratorStack[peek] = currentNodeIterator;
					}

					if (innerNode.hasPayload()) {
						currentValueIterator = innerNode.payloadIterator();
						// return hasNext = true;
						return true;
					}
				} else {
					if (peek \<= 0)
						// return hasNext = false;
						return false;

					peek -= 1;
					currentNodeIterator = nodeIteratorStack[peek];
				}
			}
		}

		@Override
		public K next() {
			if (!hasNext())
				throw new NoSuchElementException();

			return currentValueIterator.next();
		}

		<if (isOptionEnabled(ts, useSupplierIterator())) {>
		@Override
		public <typeToString(primitiveToClass(dsAtFunction__range_type(ts)))> get() {
			return currentValueIterator.get();
		}<}>

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}
	}"
	;
