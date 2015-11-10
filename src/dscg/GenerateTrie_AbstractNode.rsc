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

default str generateAbstractNodeClassString(TrieSpecifics ts) 
	= generateJdtString(ts, jdt, abstractNode())
when jdt := abstractNode(ts, modifierList = [ "private", "abstract", "static" ]);

lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, TrieNodeType nodeType:abstractNode()) 
	= [ <nodeType,method> | method <- declaredMethodsByAbstractNode];

list[PredefOp] declaredMethodsByAbstractNode = [

	theUnsafe(),

	tupleLength(),

	isAllowedToEdit(),

	containsKey(isRare = false, customComparator = false),
	containsKey(isRare = false, customComparator = true),
	containsKey(isRare = true, customComparator = false),
	containsKey(isRare = true, customComparator = true),		

	get(),
	get(customComparator = true),

	insertTuple(false, false),
	insertTuple(false, true),
	/* EXPERIMENTAL */ insertTuple(true, false),
	/* EXPERIMENTAL */ insertTuple(true, true),

	removeTuple(),
	removeTuple(customComparator = true),	
		
	hasNodes(),
	nodeArity(),
	/***/
	getContent(ctNode()),
	/***/
	nodeIterator(),
	
	hasPayload(),
	payloadArity(isRare = false),
	payloadArity(isRare = true),
	/***/
	getContent(ctPayloadArg(0)),
	getContent(ctPayloadArg(1)),
	getKeyValueEntry(),	
	getTuple(),
	/***/
	payloadIterator(),
	
	getContent(ctPayloadArg(0, isRare = true)),
	getContent(ctPayloadArg(1, isRare = true)),
		
	hasSlots(),
	slotArity(),
	/***/
	getSlot(),
	/***/
	// slotIterator?
	untypedSlotArity(),
	
	
	arity(),
	size(),
	
	sizeEmpty(),
	sizeOne(),
	sizeMoreThanOne(),

	sizePredicate(),
	
	
	PredefOp::equals(),
	// PredefOp::hashCode(),
	PredefOp::toString()
];



data PredefOp = theUnsafe();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::theUnsafe())
	= property(\return(specific("sun.misc.Unsafe")), "unsafe", visibility = "protected", isStateful = true, isConstant = true, hasGetter = false, isActive = isOptionEnabled(ts.setup, useSunMiscUnsafe()));
	
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::theUnsafe()) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::theUnsafe()) = 
	"try {
		Field field = sun.misc.Unsafe.class.getDeclaredField(\"theUnsafe\");
		field.setAccessible(true);
		return (sun.misc.Unsafe) field.get(null);
	} catch (Exception e) {
		throw new RuntimeException(e);
	}";



data PredefOp = tupleLength();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::tupleLength())
	= property(\return(primitive("int")), "<camelCaseToUpperCaseWithUnderscores("tupleLength")>", visibility = "protected", isStateful = true, isConstant = true, hasGetter = false);
	
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::tupleLength()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::tupleLength())
	= result(iconst(tupleLength(ts.ds)));



data PredefOp = arity();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::arity())
	= method(\return(primitive("int")), "arity");

str getDocumentation(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::arity()) = 
	"The arity of this trie node (i.e. number of values and nodes stored on this level).
	' 
	'@return sum of nodes and values stored within";

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::arity()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::arity())
	= result(plus(call(getDef(ts, artifact, payloadArity())), call(getDef(ts, artifact, nodeArity()))));
	


data PredefOp = size();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::size())
	= method(\return(primitive("int")), "size");
			
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::size())  = true;
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

	
	
data PredefOp = nodeIterator();

// TODO: @Deprecated
// TODO: fix generics in return type
Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), nodeIterator())
	= method(\return(generic("Iterator\<? extends <AbstractNode(ts.ds)><ts.GenericsStr>\>")), "nodeIterator");

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::nodeIterator())  = true;
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



// data PredefOp = getContent(ContentType ct);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), op:getContent(ContentType ct))
	= method(\return(ts.ct2type[ct]), "<contentAccessorMethodName(ct)>", args = [ts.index]);



// data PredefOp = getKey();
//
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getKey())
//	= method(\return(ts.keyType), "getKey", args = [ts.index]);



//data PredefOp = getRareKey();
//
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getRareKey())
//	= method(\return(object()), "getRareKey", args = [ts.index], 
//		isActive = isOptionEnabled(ts.setup, useHeterogeneousEncoding())); // TODO: fix return type



// data PredefOp = getValue();
//
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getValue())
//	= method(\return(__payloadTupleArgAtColl(ts.ds, ts.tupleTypes, 1).\type), "getVal", args = [ts.index], isActive = \map() := ts.ds);



data PredefOp = getRareValue();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getRareValue())
	= method(\return(object()), "getRareVal", args = [ts.index], 
		isActive = isOptionEnabled(ts.setup, useHeterogeneousEncoding()) && \map() := ts.ds); // TODO: fix return type



data PredefOp = getKeyValueEntry();

Method getDef(TrieSpecifics ts, Artifact artifact, getKeyValueEntry())
	= method(\return(jdtToType(jul_Map_Entry(nodeTupleTypes(ts)))), "getKeyValueEntry", args = [ts.index], isActive = \map(multi = false) := ts.ds)
when trieNode(_) := artifact;	



data PredefOp = payloadIterator();

// TODO: @Deprecated
// TODO: fix generics in return type
Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), payloadIterator())
	= method(\return(generic(isOptionEnabled(ts.setup, useSupplierIterator()) ? "SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)>" : "Iterator\<<typeToString(primitiveToClass(ts.keyType))>\>")), "payloadIterator", isActive = !isOptionEnabled(ts.setup, useFixedStackIterator()));

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::payloadIterator()) = true;
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

	
	
data PredefOp = sizePredicate();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), sizePredicate())
	= method(\return(primitive("byte")), "sizePredicate");

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), sizePredicate()) = isOptionEnabled(ts.setup, useSunMiscUnsafe());

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), sizePredicate()) = 
	"if (this.nodeArity() == 0) {
	'	switch (this.payloadArity()) {
	'	case 0:
	'		return sizeEmpty();
	'	case 1:
	'		return sizeOne();
	'	default:
	'		return sizeMoreThanOne();
	'	}
	'} else {
	'	return sizeMoreThanOne();
	'}";


data PredefOp = sizeEmpty();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::sizeEmpty())
	= function(\return(primitive("byte")), "sizeEmpty");

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::sizeEmpty()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::sizeEmpty())
	= result(binaryLiteral("00"));


data PredefOp = sizeOne();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::sizeOne())
	= function(\return(primitive("byte")), "sizeOne");

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::sizeOne()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::sizeOne())
	= result(binaryLiteral("01"));


data PredefOp = sizeMoreThanOne();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::sizeMoreThanOne())
	= function(\return(primitive("byte")), "sizeMoreThanOne");

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::sizeMoreThanOne()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::sizeMoreThanOne())
	= result(binaryLiteral("10"));
	
	
data PredefOp = insertTuple(bool isRare, bool customComparator);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), op:insertTuple(isRare:_, customComparator:false))
	= method(\return(jdtToType(abstractNode(ts))), "<insertTupleMethodName(ts.ds, artifact)>", args = [ ts.mutator, labeledArgumentList(payloadTuple(), payloadTupleArgs(ts, isRare = op.isRare)), ts.keyHash, ts.shift, ts.details ]);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), op:insertTuple(isRare:_, customComparator:true))
	= method(\return(jdtToType(abstractNode(ts))), "<insertTupleMethodName(ts.ds, artifact)>", args = [ ts.mutator, labeledArgumentList(payloadTuple(), payloadTupleArgs(ts, isRare = op.isRare)), ts.keyHash, ts.shift, ts.details, ts.comparator], isActive = isOptionEnabled(ts.setup, methodsWithComparator()));
	