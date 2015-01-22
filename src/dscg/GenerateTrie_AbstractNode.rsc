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

str generateAbstractNodeClassString(TrieSpecifics ts) { 
	return 
	"protected static abstract class <AbstractNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> implements <ts.abstractAnyNodeClassName><UnifiedGenericsExpanded(ts.ds, ts.tupleTypes)> {

		static final int TUPLE_LENGTH = <tupleLength(ts.ds)>;

		<dec(ts.AbstractNode_containsKey, asAbstract = true)>
		<dec(ts.AbstractNode_containsKeyEquiv, asAbstract = true)>
	
		<dec(ts.AbstractNode_findByKey, asAbstract = true)>
		<dec(ts.AbstractNode_findByKeyEquiv, asAbstract = true)>

		<dec(ts.AbstractNode_updated, asAbstract = true)>
		<dec(ts.AbstractNode_updatedEquiv, asAbstract = true)>

		<dec(ts.AbstractNode_removed, asAbstract = true)>
		<dec(ts.AbstractNode_removedEquiv, asAbstract = true)>
		
		static final boolean isAllowedToEdit(AtomicReference\<Thread\> x, AtomicReference\<Thread\> y) {
			return x != null && y != null && (x == y || x.get() == y.get());
		}
						
		<dec(ts.AbstractNode_getNode, asAbstract = true)>
		
		<dec(ts.AbstractNode_hasNodes, asAbstract = true)>
		<dec(ts.AbstractNode_nodeArity, asAbstract = true)>

		@Deprecated
		<implOrOverride(ts.AbstractNode_nodeIterator, 
			"return new Iterator\<<AbstractNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)>\>() {

				int nextIndex = 0;
				final int nodeArity = <AbstractNode(ts.ds)>.this.nodeArity();

				@Override
				public void remove() {
					throw new UnsupportedOperationException();
				}

				@Override
				public <AbstractNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> next() {
					if (!hasNext())
						throw new NoSuchElementException();
					return <AbstractNode(ts.ds)>.this.getNode(nextIndex++);
				}

				@Override
				public boolean hasNext() {
					return nextIndex \< nodeArity;
				}
			};", doOverride = new())>
	
	
		<dec(ts.AbstractNode_getKey, asAbstract = true)>
		<dec(ts.AbstractNode_getValue, asAbstract = true)>
		<dec(ts.AbstractNode_getKeyValueEntry, asAbstract = true)>	
	
		<dec(ts.AbstractNode__getValueAsCollection, asAbstract = true)>
			
		<dec(ts.AbstractNode_hasPayload, asAbstract = true)>
		<dec(ts.AbstractNode_payloadArity, asAbstract = true)>

		@Deprecated
		<implOrOverride(ts.AbstractNode_payloadIterator, 
			"return new <if (isOptionEnabled(ts.setup, useSupplierIterator())) {>SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)><} else {>Iterator\<<toString(primitiveToClass(ts.keyType))>\><}>() {

				int nextIndex = 0;
				final int payloadArity = <AbstractNode(ts.ds)>.this.payloadArity();

				<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>
				@Override
				public <toString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> get() {
					if (nextIndex == 0 || nextIndex \> <AbstractNode(ts.ds)>.this.payloadArity()) {
						throw new NoSuchElementException();
					}

					return <AbstractNode(ts.ds)>.this.<dsAtFunction__range_getter_name(ts.ds)>(nextIndex - 1);
				}<}>

				@Override
				public void remove() {
					throw new UnsupportedOperationException();
				}

				@Override
				public <toString(primitiveToClass(ts.keyType))> next() {
					if (!hasNext())
						throw new NoSuchElementException();
					return <AbstractNode(ts.ds)>.this.getKey(nextIndex++);
				}

				@Override
				public boolean hasNext() {
					return nextIndex \< payloadArity;
				}
			};", doOverride = new())>

		<dec(ts.AbstractNode_getSlot, asAbstract = true)>
	
		<dec(ts.AbstractNode_hasSlots, asAbstract = true)>
		<dec(ts.AbstractNode_slotArity, asAbstract = true)>
	
		/**
		 * The arity of this trie node (i.e. number of values and nodes stored
		 * on this level).
		 * 
		 * @return sum of nodes and values stored within
		 */
		<implOrOverride(ts.AbstractNode_arity, "return payloadArity() + nodeArity();", doOverride = new())>

		<implOrOverride(ts.AbstractNode_size, 
			"final Iterator\<<toString(primitiveToClass(ts.keyType))>\> it = new <toString(ts.ds)>KeyIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(this);

			int size = 0;
			while (it.hasNext()) {
				size += 1;
				it.next();
			}

			return size;", doOverride = new())>
	}";
}