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

list[PredefOp] declaredMethodsByAbstractNode = [

	containsKey(),
	containsKey(customComparator = true),

	get(),
	get(customComparator = true),

	insertTuple(),
	insertTuple(customComparator = true),

	removeTuple(),
	removeTuple(customComparator = true),	
	
	/* TODO: isAllowedToEdit */
	
	hasNodes(),
	nodeArity(),
	getNode(),	
	nodeIterator(),
	
	hasPayload(),
	payloadArity(),
	
	getKey(),
	getValue(),
	getKeyValueEntry(),	
	getTuple(),
	payloadIterator(),
	
	hasSlots(),
	slotArity(),
	getSlot(),
	
	arity(),
	size()	
];

str generateAbstractNodeClassString(TrieSpecifics ts) { 
	return
	"";
}

str legacy_generateAbstractNodeClassString(TrieSpecifics ts) {
	return 
	"protected static abstract class <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> implements <ts.abstractAnyNodeClassName><UnifiedGenericsExpanded(ts.ds, ts.tupleTypes)> {

		static final int TUPLE_LENGTH = <tupleLength(ts.ds)>;

		<dec(getDef(ts, trieNode(abstractNode()), containsKey()), asAbstract = true)>
		<dec(getDef(ts, trieNode(abstractNode()), containsKey(customComparator = true)), asAbstract = true)>
	
		<dec(getDef(ts, trieNode(abstractNode()), get()), asAbstract = true)>
		<dec(getDef(ts, trieNode(abstractNode()), get(customComparator = true)), asAbstract = true)>
	
		<dec(getDef(ts, trieNode(abstractNode()), insertTuple()), asAbstract = true)>
		<dec(getDef(ts, trieNode(abstractNode()), insertTuple(customComparator = true)), asAbstract = true)>

		<dec(getDef(ts, trieNode(abstractNode()), removeTuple()), asAbstract = true)>
		<dec(getDef(ts, trieNode(abstractNode()), removeTuple(customComparator = true)), asAbstract = true)>
	
		static final boolean isAllowedToEdit(AtomicReference\<Thread\> x, AtomicReference\<Thread\> y) {
			return x != null && y != null && (x == y || x.get() == y.get());
		}
											
		<dec(getDef(ts, trieNode(abstractNode()), hasNodes()), asAbstract = true)>
		<dec(getDef(ts, trieNode(abstractNode()), nodeArity()), asAbstract = true)>
		</***/"">
		<dec(getDef(ts, trieNode(abstractNode()), getNode()), asAbstract = true)>		
						
		@Deprecated
		<implOrOverride(getDef(ts, trieNode(abstractNode()), nodeIterator()), 
			"return new Iterator\<<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)>\>() {

				int nextIndex = 0;
				final int nodeArity = <AbstractNode(ts.ds)>.this.nodeArity();

				@Override
				public void remove() {
					throw new UnsupportedOperationException();
				}

				@Override
				public <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> next() {
					if (!hasNext())
						throw new NoSuchElementException();
					return <AbstractNode(ts.ds)>.this.getNode(nextIndex++);
				}

				@Override
				public boolean hasNext() {
					return nextIndex \< nodeArity;
				}
			};", doOverride = new())>
	
	
		<dec(getDef(ts, trieNode(abstractNode()), hasPayload()), asAbstract = true)>
		<dec(getDef(ts, trieNode(abstractNode()), payloadArity()), asAbstract = true)>
		</***/"">
		<dec(getDef(ts, trieNode(abstractNode()), getKey()), asAbstract = true)>
		<dec(getDef(ts, trieNode(abstractNode()), getValue()), asAbstract = true)>
		<dec(getDef(ts, trieNode(abstractNode()), getKeyValueEntry()), asAbstract = true)>
		<dec(getDef(ts, trieNode(abstractNode()), getTuple()), asAbstract = true)>				

		@Deprecated
		<implOrOverride(getDef(ts, trieNode(abstractNode()), payloadIterator()), 
			"return new <if (isOptionEnabled(ts.setup, useSupplierIterator())) {>SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)><} else {>Iterator\<<typeToString(primitiveToClass(ts.keyType))>\><}>() {

				int nextIndex = 0;
				final int payloadArity = <AbstractNode(ts.ds)>.this.payloadArity();

				<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>
				@Override
				public <typeToString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> get() {
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
				public <typeToString(primitiveToClass(ts.keyType))> next() {
					if (!hasNext())
						throw new NoSuchElementException();
					return <AbstractNode(ts.ds)>.this.getKey(nextIndex++);
				}

				@Override
				public boolean hasNext() {
					return nextIndex \< payloadArity;
				}
			};", doOverride = new())>
	
	
		<dec(getDef(ts, trieNode(abstractNode()), hasSlots()), asAbstract = true)>
		<dec(getDef(ts, trieNode(abstractNode()), slotArity()), asAbstract = true)>	
		</***/"">
		<dec(getDef(ts, trieNode(abstractNode()), getSlot()), asAbstract = true)>	
		
		/**
		 * The arity of this trie node (i.e. number of values and nodes stored
		 * on this level).
		 * 
		 * @return sum of nodes and values stored within
		 */
		<implOrOverride(getDef(ts, trieNode(abstractNode()), arity()), 
			"return payloadArity() + nodeArity();", doOverride = new())>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), size()), 
			"final Iterator\<<typeToString(primitiveToClass(ts.keyType))>\> it = new <toString(ts.ds)>KeyIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(this);

			int size = 0;
			while (it.hasNext()) {
				size += 1;
				it.next();
			}

			return size;", doOverride = new())>
	}";
}