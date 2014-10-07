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
module dscg::GenerateTrie_AbstractNode

import dscg::Common;

str generateAbstractNodeClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) { 
	return 
	"protected static abstract class <AbstractNode(ds)><Generics(ds)> extends AbstractNode<UnifiedGenericsExpanded(ds)> {

		static final int TUPLE_LENGTH = <tupleLength(ds)>;

		<dec(ts.AbstractNode_containsKey)>
		<dec(ts.AbstractNode_containsKeyEquiv)>
	
		<dec(ts.AbstractNode_findByKey)>
		<dec(ts.AbstractNode_findByKeyEquiv)>

		<dec(ts.AbstractNode_updated)>
		<dec(ts.AbstractNode_updatedEquiv)>
		<dec(ts.AbstractNode_removed)>
		<dec(ts.AbstractNode_removedEquiv)>
		
		static final boolean isAllowedToEdit(AtomicReference\<Thread\> x, AtomicReference\<Thread\> y) {
			return x != null && y != null && (x == y || x.get() == y.get());
		}
						
		<dec(ts.AbstractNode_getNode)>
		
		<dec(ts.AbstractNode_hasNodes)>
		<dec(ts.AbstractNode_nodeArity)>

		@Deprecated
		<implOrOverride(ts.AbstractNode_nodeIterator, 
			"return new Iterator\<<AbstractNode(ds)><Generics(ds)>\>() {

				int nextIndex = 0;

				@Override
				public void remove() {
					throw new UnsupportedOperationException();
				}

				@Override
				public <AbstractNode(ds)><Generics(ds)> next() {
					if (!hasNext())
						throw new NoSuchElementException();
					return <AbstractNode(ds)>.this.getNode(nextIndex++);
				}

				@Override
				public boolean hasNext() {
					return nextIndex \< <AbstractNode(ds)>.this.nodeArity();
				}
			};", doOverride = false)>
	
	
		<dec(ts.AbstractNode_getKey)>
		<dec(ts.AbstractNode_getValue)>				
		<dec(ts.AbstractNode_getKeyValueEntry)>	
	
		<dec(ts.AbstractNode_hasPayload)>
		<dec(ts.AbstractNode_payloadArity)>

		@Deprecated
		<implOrOverride(ts.AbstractNode_payloadIterator, 
			"return new SupplierIterator<SupplierIteratorGenerics(ds)>() {

				int nextIndex = 0;

				@Override
				public <toString(primitiveToClass(dsAtFunction__range_type(ds)))> get() {
					if (nextIndex == 0 || nextIndex \> <AbstractNode(ds)>.this.payloadArity()) {
						throw new NoSuchElementException();
					}

					return <AbstractNode(ds)>.this.<dsAtFunction__range_getter_name(ds)>(nextIndex - 1);
				}

				@Override
				public void remove() {
					throw new UnsupportedOperationException();
				}

				@Override
				public <toString(primitiveToClass(key().\type))> next() {
					if (!hasNext())
						throw new NoSuchElementException();
					return <AbstractNode(ds)>.this.getKey(nextIndex++);
				}

				@Override
				public boolean hasNext() {
					return nextIndex \< <AbstractNode(ds)>.this.payloadArity();
				}
			};", doOverride = false)>

		<dec(ts.AbstractNode_getSlot)>
	
		/**
		 * The arity of this trie node (i.e. number of values and nodes stored
		 * on this level).
		 * 
		 * @return sum of nodes and values stored within
		 */
		<implOrOverride(ts.AbstractNode_arity, "return payloadArity() + nodeArity();", doOverride = false)>

		<implOrOverride(ts.AbstractNode_size, 
			"final SupplierIterator<SupplierIteratorGenerics(ds)> it = new <toString(ds)>KeyIterator<InferredGenerics()>(this);

			int size = 0;
			while (it.hasNext()) {
				size += 1;
				it.next();
			}

			return size;", doOverride = false)>
	}";
}