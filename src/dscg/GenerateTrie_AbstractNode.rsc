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

str generateAbstractNodeClassString(TrieSpecifics ts) 
	= generateJdtString(ts, jdt, abstractNode())
when jdt := abstractNode(ts, modifierList = [ "private", "abstract", "static" ]);

lrel[TrieNodeType from, PredefOp to] declares(TrieNodeType nodeType:abstractNode()) 
	= [ <nodeType,method> | method <- declaredMethodsByAbstractNode];

list[PredefOp] declaredMethodsByAbstractNode = [

	tupleLength(), // TODO: implement as static final field

	isAllowedToEdit(),

	containsKey(),
	containsKey(customComparator = true),

	get(),
	get(customComparator = true),

	insertTuple(),
	insertTuple(customComparator = true),

	removeTuple(),
	removeTuple(customComparator = true),	
		
	hasNodes(),
	nodeArity(),
	/***/
	getNode(),
	/***/
	nodeIterator(),
	
	hasPayload(),
	payloadArity(),
	/***/
	getKey(),
	getValue(),
	getKeyValueEntry(),	
	getTuple(),
	/***/
	payloadIterator(),
	
	hasSlots(),
	slotArity(),
	/***/
	getSlot(),
	/***/
	// slotIterator?
	
	arity(),
	size()	
];



data PredefOp = tupleLength();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::tupleLength())
	= function(\return(primitive("int")), "tupleLength", visibility = "protected");
	
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::tupleLength())
	= result(iconst(tupleLength(ts.ds)));



data PredefOp = arity();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::arity())
	= method(\return(primitive("int")), "arity");

str getDocumentation(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::arity()) = 
	"The arity of this trie node (i.e. number of values and nodes stored on this level).
	' 
	'@return sum of nodes and values stored within";

Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::arity())
	= result(plus(call(getDef(ts, artifact, payloadArity())), call(getDef(ts, artifact, nodeArity()))));
	


data PredefOp = size();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::size())
	= method(\return(primitive("int")), "size");
			
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::size()) =
	compoundExpr([
		exprFromString(
			"final Iterator\<<typeToString(primitiveToClass(ts.keyType))>\> it = new <toString(ts.ds)>KeyIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(this);

			int size = 0;
			while (it.hasNext()) {
				size += 1;
				it.next();
			}"),
		result(useExpr(var(primitive("int"), "size")))
	]);

	
	
data PredefOp = getNode();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getNode())
	= method(\return(jdtToType(abstractNode(ts))), "getNode", args = [ts.index]);



data PredefOp = nodeIterator();

// TODO: @Deprecated
// TODO: fix generics in return type
Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), nodeIterator())
	= method(\return(generic("Iterator\<? extends <AbstractNode(ts.ds)><ts.GenericsStr>\>")), "nodeIterator");

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::nodeIterator()) = 
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
	};";



data PredefOp = payloadIterator();

// TODO: @Deprecated
// TODO: fix generics in return type
Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), payloadIterator())
	= method(\return(generic(isOptionEnabled(ts.setup, useSupplierIterator()) ? "SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)>" : "Iterator\<<typeToString(primitiveToClass(ts.keyType))>\>")), "payloadIterator", isActive = !isOptionEnabled(ts.setup, useFixedStackIterator()));

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::payloadIterator()) = 
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
	};";