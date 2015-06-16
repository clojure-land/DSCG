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
module dscg::GenerateTrie_CompactNode

import List;

import dscg::Common;

default str generateCompactNodeClassString(TrieSpecifics ts) {  
	str result = "";
	
	booleanOptions = { true, false };

	JavaDataType jdt = compactNode(ts, modifierList = [ "private", "abstract", "static" ]);
	result += generateJdtString(ts, jdt, compactNode());
	
	for (bitmapCfg <- [ specializeByBitmap(n, v) | <n, v> <- booleanOptions * booleanOptions]) {
		JavaDataType jdt = compactNode(ts, compactNode(bitmapCfg), modifierList = [ "private", "abstract", "static" ]);
		result += generateJdtString(ts, jdt, compactNode(bitmapCfg));
	}
	
	return result;
}


data PredefOp = isRare1();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRare1())
	= function(\return(primitive("boolean")), "isRare", args = [ ts.stdObjectArg ]);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRare1()) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRare1())
	= "throw new UnsupportedOperationException(); // TODO: to implement";


data PredefOp = isRare2();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRare2())
	= function(\return(primitive("boolean")), "isRare", args = [ ts.stdObjectArg0, ts.stdObjectArg1 ]);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRare2()) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRare2())
	= "throw new UnsupportedOperationException(); // TODO: to implement";
	
	
data PredefOp = isRareBitpos();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRareBitpos())
	= function(\return(primitive("boolean")), "isRare", args = [ ts.bitposField ]);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRareBitpos()) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRareBitpos())
	= "throw new UnsupportedOperationException(); // TODO: to implement";	


data PredefOp = nodeMap();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeMap())
	= method(ts.bitmapField, ts.bitmapField.name);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::nodeMap())
	= property(ts.bitmapField, ts.bitmapField.name)
when bs.supportsNodes;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::nodeMap())
	= method(ts.bitmapField, ts.bitmapField.name)
when !bs.supportsNodes;

// Default Value for Property
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::nodeMap()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::nodeMap())
	= result(iconst(0));


data PredefOp = dataMap();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::dataMap())
	= method(ts.valmapField, ts.valmapField.name);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::dataMap())
	= property(ts.valmapField, ts.valmapField.name)
when bs.supportsValues;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::dataMap())
	= method(ts.valmapField, ts.valmapField.name)
when !bs.supportsValues;

// Default Value for Property
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::dataMap()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::dataMap())
	= result(iconst(0));


data PredefOp = hashCodeLength();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::hashCodeLength())
	= property(\return(primitive("int")), "hashCodeLength", isStateful = false, isConstant = true);
	
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::hashCodeLength()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::hashCodeLength())
	= result(iconst(32));


data PredefOp = bitPartitionSize();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::bitPartitionSize())
	= property(\return(primitive("int")), "bitPartitionSize", isStateful = false, isConstant = true);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::bitPartitionSize()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::bitPartitionSize())
	= result(iconst(ts.bitPartitionSize));


data PredefOp = bitPartitionMask();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::bitPartitionMask())
	= property(\return(primitive("int")), "bitPartitionMask", isStateful = false, isConstant = true);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::bitPartitionMask()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::bitPartitionMask())
	= result(binaryLiteralOfOnes(ts.bitPartitionSize));


data PredefOp = sizePredicate();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), sizePredicate())
	= method(\return(primitive("byte")), "sizePredicate");


data PredefOp = sizeEmpty();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::sizeEmpty())
	= function(\return(primitive("byte")), "sizeEmpty");

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::sizeEmpty()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::sizeEmpty())
	= result(binaryLiteral("00"));


data PredefOp = sizeOne();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::sizeOne())
	= function(\return(primitive("byte")), "sizeOne");

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::sizeOne()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::sizeOne())
	= result(binaryLiteral("01"));


data PredefOp = sizeMoreThanOne();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::sizeMoreThanOne())
	= function(\return(primitive("byte")), "sizeMoreThanOne");

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::sizeMoreThanOne()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::sizeMoreThanOne())
	= result(binaryLiteral("10"));


data PredefOp = nodeInvariant();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeInvariant())
	= method(\return(primitive("boolean")), "nodeInvariant");


data PredefOp = isTrieStructureValid();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeInvariant())
	= method(\return(primitive("boolean")), "nodeInvariant");


data PredefOp = emptyTrieNodeConstant();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::emptyTrieNodeConstant())
	= property(\return(jdtToType(compactNode(ts))), "emptyTrieNodeConstant", generics = ts.genericTupleTypes, isStateful = false, isConstant = true);
	
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::emptyTrieNodeConstant()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::emptyTrieNodeConstant())
	= result(exprFromString("EMPTY_NODE"));
		

data PredefOp = getNode();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), getNode())
	= method(\return(jdtToType(compactNode(ts))), "getNode", args = [ts.index]);


data PredefOp = nodeAt();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), nodeAt())
	= method(\return(jdtToType(compactNode(ts))), "nodeAt", args = [ts.index]);


data PredefOp = recoverMask();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::recoverMask())
	= function(\return(primitive("byte")), "recoverMask");
	

data PredefOp = toString();

/* 
 * visibility is enforced through Object.toString 
 */
Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::toString())
	= method(\return(specific("java.lang.String")), "toString", visibility = "public");


data PredefOp = nodeFactory_Array();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Array())
	= function(ts.compactNodeClassReturn, "nodeOf", generics = ts.genericTupleTypes, args = [ ts.mutator, ts.bitmapField, ts.valmapField, ts.BitmapIndexedNode_contentArray, ts.BitmapIndexedNode_payloadArity, ts.BitmapIndexedNode_nodeArity ], argsFilter = ts.argsFilter, isActive = !isOptionEnabled(ts.setup,useSpecialization()) || ts.nBound < ts.nMax);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Array()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Array()) 
	= result(call(ts.BitmapIndexedNode_constructor, inferredGenericsStr = InferredGenerics(ts.ds, ts.tupleTypes)));


data PredefOp = nodeFactory_Empty();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Empty())
	= function(ts.compactNodeClassReturn, "nodeOf", generics = ts.genericTupleTypes, args = [ ts.mutator ], argsFilter = ts.argsFilter, isActive = !isOptionEnabled(ts.setup,useSpecialization()) || ts.nBound < ts.nMax);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Empty()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Empty()) 
	= result(call(getDef(ts, trieNode(compactNode()), emptyTrieNodeConstant())));


data PredefOp = nodeFactory_Singleton();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Singleton())
	= function(ts.compactNodeClassReturn, "nodeOf", generics = ts.genericTupleTypes, args = [ ts.mutator, ts.bitmapField, ts.valmapField, *ts.payloadTuple ], argsFilter = ts.argsFilter, isActive = !isOptionEnabled(ts.setup,useSpecialization()));

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Singleton()) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Singleton()) =
	"assert <use(bitmapField)> == 0;	
	'return <toString(call(ts.nodeOf_BitmapIndexedNode, 
		argsOverride = (ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.bitmapField.\type, "0")),						
						ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { <use(nodeTupleArgs(ts))> }"),
						ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "1")),
						ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "0")))))>;";


//	<implOrOverride(ts.nodeOf_BitmapIndexedNode,
//			
//	
//	<if (!isOptionEnabled(ts.setup,useSpecialization()) || ts.nBound < ts.nMax) {>
//	'	<toString(UNCHECKED_ANNOTATION())>
//	'	static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(AtomicReference\<Thread\> mutator) {
//	'		return <emptyTrieNodeConstantName>;
//	'	}
//	<} else {>
//	'	// TODO: consolidate and remove
//	'	static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(AtomicReference\<Thread\> mutator) {
//	'		return nodeOf(mutator, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0);
//	'	}
//	<}>
//
//	<if (!isOptionEnabled(ts.setup,useSpecialization())) {>
//	'	static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>, <dec(nodeTupleArgs(ts))>) {
//	'		assert <use(bitmapField)> == 0;	
//	'		return <toString(call(ts.nodeOf_BitmapIndexedNode, 
//				argsOverride = (ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.bitmapField.\type, "0")),						
//								ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { <use(nodeTupleArgs(ts))> }"),
//								ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "1")),
//								ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "0")))))>;
//	'	}
//	<}>


lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, TrieNodeType nodeType:compactNode()) 
	= [ <nodeType,method> | method <- declaredMethodsByCompactNode]
	+ [ <nodeType,method> | method <- createNodeFactorySpecializationList(ts, nodeType)];

list[PredefOp] declaredMethodsByCompactNode = [

	// TODO: this is implementation specific ...
	// TODO: nodeOf() factory methods; also option to enable disable the use of factory methods.
	nodeFactory_Empty(),
	nodeFactory_Singleton(),
	nodeFactory_Array(),	

	hashCodeLength(), // TODO: implement as static final field
	bitPartitionSize(), // TODO: implement as static final field
	
	// TODO: implement as static final field
	// TODO: this is implementation specific
	bitPartitionMask(),
	
	mask(),
	bitpos(),
	
	nodeMap(),
	dataMap(),
	
	isRare1(),
	isRare2(),
	isRareBitpos(),
	
	sizeEmpty(),
	sizeOne(),
	sizeMoreThanOne(),

	sizePredicate(),
	
	getNode(), // redeclaration (more specific return type)	
	
	nodeInvariant(),
	isTrieStructureValid(),
	
	copyAndInsertNode(), // ???
	copyAndRemoveNode(), // ???
	copyAndSetValue(),
	copyAndInsertValue(),
	copyAndRemoveValue(),
	copyAndSetNode(),
	copyAndMigrateFromInlineToNode(),
	copyAndMigrateFromNodeToInline(),
	
	removeInplaceValueAndConvertToSpecializedNode(),	

	mergeTwoKeyValPairs(),
	mergeNodeAndKeyValPair(),

	emptyTrieNodeConstant(), // TODO: this is implementation specific

	index2(),
	index3(),
	
	dataIndex(),
	nodeIndex(),
	
	nodeAt(), // TODO: get rid of?
		
//	containsKey(),
//	containsKey(customComparator = true),
//
//	get(),
//	get(customComparator = true),
//
//	insertTuple(),
//	insertTuple(customComparator = true),
//
//	removeTuple(),
//	removeTuple(customComparator = true),	
		
	recoverMask(),
	toString()
	
];

lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, TrieNodeType nodeType:compactNode(BitmapSpecialization _)) 
	= [ <nodeType,nodeMap()>, <nodeType,dataMap()> ];

str emptyTrieNodeConstantName = "EMPTY_NODE";
str generateCompactNodeClassString(TrieSpecifics ts, bool isLegacy = true) {
	
	//TrieSpecifics ts = setArtifact(tsSuper, trieNode(compactNode()));
	
	abstractMembers = [ bitmapMethod, valmapMethod ];
	concreteMembers = [];
	
	members = abstractMembers + concreteMembers;	
	
	constructorArgs = asFieldList(
		  ts.mutator 
		+ members);

	str classNameStr = "<CompactNode(ts.ds)>"; 

	int n = 0; // TODO: remove
	int m = 0; // TODO: remove

	return
	"protected static abstract class <classNameStr><GenericsStr(ts.tupleTypes)> extends Abstract<toString(ts.ds)>Node<GenericsStr(ts.tupleTypes)> {
		
		<impl(ts, trieNode(compactNode()), hashCodeLength())>
		<impl(ts, trieNode(compactNode()), bitPartitionSize())>
		<impl(ts, trieNode(compactNode()), bitPartitionMask())>
		
		<impl(ts, trieNode(compactNode()), mask())>
		<impl(ts, trieNode(compactNode()), bitpos())>		
		
		<dec(getDef(ts, trieNode(compactNode()), nodeMap()), asAbstract = true)>
		<dec(getDef(ts, trieNode(compactNode()), dataMap()), asAbstract = true)>
		
		<impl(ts, trieNode(compactNode()), isRare1())>
		<impl(ts, trieNode(compactNode()), isRare2())>
		<impl(ts, trieNode(compactNode()), isRareBitpos())>		
		
		<dec(field(primitive("byte"), "SIZE_EMPTY"), 		constant(primitive("byte"), "0b00"), isStatic = true)>;
		<dec(field(primitive("byte"), "SIZE_ONE"), 			constant(primitive("byte"), "0b01"), isStatic = true)>;
		<dec(field(primitive("byte"), "SIZE_MORE_THAN_ONE"),constant(primitive("byte"), "0b10"), isStatic = true)>;

		/**
		 * Abstract predicate over a node\'s size. Value can be either
		 * {@value #SIZE_EMPTY}, {@value #SIZE_ONE}, or
		 * {@value #SIZE_MORE_THAN_ONE}.
		 * 
		 * @return size predicate
		 */
		<dec(getDef(ts, trieNode(compactNode()), sizePredicate()), 
			asAbstract = true)>

		@Override
		<dec(getDef(ts, trieNode(compactNode()), getNode()), 
			asAbstract = true)>

		boolean nodeInvariant() {
			boolean inv1 = (size() - payloadArity() \>= 2 * (arity() - payloadArity()));
			boolean inv2 = (this.arity() == 0) ? sizePredicate() == SIZE_EMPTY : true;
			boolean inv3 = (this.arity() == 1 && payloadArity() == 1) ? sizePredicate() == SIZE_ONE
							: true;
			boolean inv4 = (this.arity() \>= 2) ? sizePredicate() == SIZE_MORE_THAN_ONE : true;

			boolean inv5 = (this.nodeArity() \>= 0) && (this.payloadArity() \>= 0)
							&& ((this.payloadArity() + this.nodeArity()) == this.arity());

			return inv1 && inv2 && inv3 && inv4 && inv5;
		}

		<impl(ts, trieNode(compactNode()), copyAndInsertNode())>
		<impl(ts, trieNode(compactNode()), copyAndRemoveNode())>

		<dec(getDef(ts, trieNode(compactNode()), copyAndInsertNode()), asAbstract = true)>
		<dec(getDef(ts, trieNode(compactNode()), copyAndRemoveNode()), asAbstract = true)>
		<dec(getDef(ts, trieNode(compactNode()), copyAndSetValue()), asAbstract = true)>
		<dec(getDef(ts, trieNode(compactNode()), copyAndInsertValue()), asAbstract = true)>
		<dec(getDef(ts, trieNode(compactNode()), copyAndRemoveValue()), asAbstract = true)>
		<dec(getDef(ts, trieNode(compactNode()), copyAndSetNode()), asAbstract = true)>
		<dec(getDef(ts, trieNode(compactNode()), copyAndMigrateFromInlineToNode()), asAbstract = true)>
		<dec(getDef(ts, trieNode(compactNode()), copyAndMigrateFromNodeToInline()), asAbstract = true)>
		
		</* TODO: specialize removed(..) to remove this method from this interface */"">
		<impl(ts, trieNode(compactNode()), removeInplaceValueAndConvertToSpecializedNode())>

		<impl(ts, trieNode(compactNode()), mergeTwoKeyValPairs())>
		<impl(ts, trieNode(compactNode()), mergeNodeAndKeyValPair())>

		<impl(ts, trieNode(compactNode()), emptyTrieNodeConstant())>

	'	static final <CompactNode(ts.ds)> <emptyTrieNodeConstantName>;

	'	static {
	'		<if (isOptionEnabled(ts.setup,useSpecialization())) {>
				<emptyTrieNodeConstantName> = new <toString(ts.ds)>0To0Node<ts.classNamePostfix><InferredGenerics(ts.ds, ts.tupleTypes)>(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0);
			<} else {>
		 		<emptyTrieNodeConstantName> = <toString(call(ts.BitmapIndexedNode_constructor, 
					argsOverride = (ts.mutator: NULL(),								
								ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.bitmapField.\type, "0")), 
								ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.valmapField.\type, "0")),
								ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { }"),
								ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "0")),
								ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "0"))),
					inferredGenericsStr = "<InferredGenerics(ts.ds, ts.tupleTypes)>"))>;
				
			<}>	
	'	};
	
	<implOrOverride(ts.nodeOf_BitmapIndexedNode,
		"return <toString(call(ts.BitmapIndexedNode_constructor, inferredGenericsStr = InferredGenerics(ts.ds, ts.tupleTypes)))>;")>	
	
	<if (!isOptionEnabled(ts.setup,useSpecialization()) || ts.nBound < ts.nMax) {>
	'	<toString(UNCHECKED_ANNOTATION())>
	'	static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(AtomicReference\<Thread\> mutator) {
	'		return <emptyTrieNodeConstantName>;
	'	}
	<} else {>
	'	// TODO: consolidate and remove
	'	static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(AtomicReference\<Thread\> mutator) {
	'		return nodeOf(mutator, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0);
	'	}
	<}>

	<if (!isOptionEnabled(ts.setup,useSpecialization())) {>
	'	static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>, <dec(nodeTupleArgs(ts))>) {
	'		assert <use(bitmapField)> == 0;	
	'		return <toString(call(ts.nodeOf_BitmapIndexedNode, 
				argsOverride = (ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.bitmapField.\type, "0")),						
								ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { <use(nodeTupleArgs(ts))> }"),
								ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "1")),
								ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "0")))))>;
	'	}
	<}>

	<generate_specializationFactoryMethods(ts, ts.setup)>

	<for (op <- createNodeFactorySpecializationList(ts, compactNode())) {>
		<impl(ts, trieNode(compactNode()), op)>
	<}>

	<impl(ts, trieNode(compactNode()), index2())>
	<impl(ts, trieNode(compactNode()), index3())>

	<impl(ts, trieNode(compactNode()), dataIndex())>
	<impl(ts, trieNode(compactNode()), nodeIndex())>
	
	<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeAt(<dec(ts.bitposField)>) {
		return getNode(nodeIndex(bitpos)); 
	}
	
	<impl(ts, trieNode(compactNode()), containsKey())>
	<impl(ts, trieNode(compactNode()), containsKey(customComparator = true))>

	<impl(ts, trieNode(compactNode()), get())>
	<impl(ts, trieNode(compactNode()), get(customComparator = true))>
	
	<impl(ts, trieNode(compactNode()), insertTuple())>
	<impl(ts, trieNode(compactNode()), insertTuple(customComparator = true))>

	<impl(ts, trieNode(compactNode()), removeTuple())>
	<impl(ts, trieNode(compactNode()), removeTuple(customComparator = true))>

	'	/**
	'	 * @return 0 \<= mask \<= 2^BIT_PARTITION_SIZE - 1
	'	 */
	'	static byte recoverMask(<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))> map, byte i_th) {
	'		assert 1 \<= i_th && i_th \<= <ts.nMax>;
	'		
	'		byte cnt1 = 0;
	'		byte mask = 0;
	'		
	'		while (mask \< <ts.nMax>) {
	'			if ((map & 0x01) == 0x01) {
	'				cnt1 += 1;
	'		
	'				if (cnt1 == i_th) {
	'					return mask;
	'				}
	'			}
	'		
	'			map = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (map \>\> 1);
	'			mask += 1;
	'		}
	'			
	'		assert cnt1 != i_th;
	'		throw new RuntimeException(\"Called with invalid arguments.\");
	'	}

	'	@Override
	'	public String toString() {
	'		final StringBuilder bldr = new StringBuilder();
	'		bldr.append(\'[\');
	'
	'		for (byte i = 0; i \< payloadArity(); i++) {
	'			final byte pos = recoverMask(<use(valmapMethod)>, (byte) (i + 1));
	'			bldr.append(String.format(\"@%d\<<intercalate(",", times("#%d", size(ts.payloadTuple)))>\>\", pos, <use(invoke_getAndHashCode_for_payloadTuple(ts.ds, ts.tupleTypes, field("i")))>));
	'
	'			if (!((i + 1) == payloadArity())) {
	'				bldr.append(\", \");
	'			}
	'		}
	'
	'		if (payloadArity() \> 0 && nodeArity() \> 0) {
	'			bldr.append(\", \");
	'		}
	'
	'		for (byte i = 0; i \< nodeArity(); i++) {
	'			final byte pos = recoverMask(<use(bitmapMethod)>, (byte) (i + 1));
	'			bldr.append(String.format(\"@%d: %s\", pos, getNode(i)));
	'
	'			if (!((i + 1) == nodeArity())) {
	'				bldr.append(\", \");
	'			}
	'		}
	'
	'		bldr.append(\']\');
	'		return bldr.toString();
	'	}
	
		<impl(ts, trieNode(compactNode()), isTrieStructureValid())>
	
	'}
	
	protected static abstract class <className(ts, compactNode(specializeByBitmap(true, true)))><GenericsStr(ts.tupleTypes)> extends <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> {

		private <dec(ts.bitmapField)>;
		private <dec(ts.valmapField)>;

		<className(ts, compactNode(specializeByBitmap(true, true)))>(final AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>) {
			this.<bitmapField.name> = <bitmapField.name>;
			this.<valmapField.name> = <valmapField.name>;
		}

		@Override
		public <typeToString(ts.bitmapField.\type)> <bitmapField.name>() {
			return <bitmapField.name>;
		}

		@Override
		public <typeToString(ts.valmapField.\type)> <valmapField.name>() {
			return <valmapField.name>;
		}

	}

	<if (isOptionEnabled(ts.setup,useSpecialization())) {>
	protected static abstract class <className(ts, compactNode(specializeByBitmap(true, false)))><GenericsStr(ts.tupleTypes)> extends <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> {

		private <dec(ts.bitmapField)>;

		<className(ts, compactNode(specializeByBitmap(true, false)))>(final AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>) {
			this.<bitmapField.name> = <bitmapField.name>;
		}

		@Override
		public <typeToString(ts.bitmapField.\type)> <bitmapField.name>() {
			return <bitmapField.name>;
		}

		@Override
		public <typeToString(ts.valmapField.\type)> <valmapField.name>() {
			return 0;
		}

	}

	protected static abstract class <className(ts, compactNode(specializeByBitmap(false, true)))><GenericsStr(ts.tupleTypes)> extends <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> {

		private <dec(ts.valmapField)>;

		<className(ts, compactNode(specializeByBitmap(false, true)))>(final AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>) {
			this.<valmapField.name> = <valmapField.name>;
		}

		@Override
		public <typeToString(ts.bitmapField.\type)> <bitmapField.name>() {
			return 0;
		}

		@Override
		public <typeToString(ts.valmapField.\type)> <valmapField.name>() {
			return <valmapField.name>;
		}

	}
	
	protected static abstract class <className(ts, compactNode(specializeByBitmap(false, false)))><GenericsStr(ts.tupleTypes)> extends <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> {

		<className(ts, compactNode(specializeByBitmap(false, false)))>(final AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>) {
		}

		@Override
		public <typeToString(ts.bitmapField.\type)> <bitmapField.name>() {
			return 0;
		}

		@Override
		public <typeToString(ts.valmapField.\type)> <valmapField.name>() {
			return 0;
		}

	}
	<}>
	"
	;
	
}

bool exists_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) = true; 
str generate_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"if (mask0 \< mask1) {
	'	return nodeOf(null, (byte) mask0, <use(__payloadTuple(ts.ds, ts.tupleTypes, 0))>, (byte) mask1, <use(__payloadTuple(ts.ds, ts.tupleTypes, 1))>);
	'} else {
	'	return nodeOf(null, (byte) mask1, <use(__payloadTuple(ts.ds, ts.tupleTypes, 1))>, (byte) mask0, <use(__payloadTuple(ts.ds, ts.tupleTypes, 0))>);
	'}";

bool exists_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) = true;
str generate_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))> | <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>);
	' 
	'if (mask0 \< mask1) {
	'	return nodeOf(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0, <use(valmapField)>, <use(appendToName(nodeTupleArgs(ts), "0") + appendToName(nodeTupleArgs(ts), "1"))>);
	'} else {
	'	return nodeOf(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0, <use(valmapField)>, <use(appendToName(nodeTupleArgs(ts), "1") + appendToName(nodeTupleArgs(ts), "0"))>);
	'}";

/*
 *	Both <call> invocatiosn in the body have similar data; only content array differs.
 */	
bool exists_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) = true;
str generate_bodyOf_mergeTwoValues(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))> | <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>);
	'	
	'if (mask0 \< mask1) {
		return <toString(call(ts.nodeOf_BitmapIndexedNode, 
				argsOverride = (ts.mutator: NULL(),								
								ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.bitmapField.\type, "0")), 
								ts.valmapField: useExpr(ts.valmapField),
								ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { <use(appendToName(nodeTupleArgs(ts), "0") + appendToName(nodeTupleArgs(ts), "1"))> }"),
								ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "2")),
								ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "0")))))>;
	'} else {
		return <toString(call(ts.nodeOf_BitmapIndexedNode, 
				argsOverride = (ts.mutator: NULL(),								
								ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.bitmapField.\type, "0")), 
								ts.valmapField: useExpr(ts.valmapField),
								ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { <use(appendToName(nodeTupleArgs(ts), "1") + appendToName(nodeTupleArgs(ts), "0"))> }"),
								ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "2")),
								ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "0")))))>;	
	'}";	

default bool exists_bodyOf_mergeTwoValues(TrieSpecifics _, Option _, Position _)  = true;
default str generate_bodyOf_mergeTwoValues(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }

bool exists_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) = true; 
str generate_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"return nodeOf(null, (byte) mask0, node);";

bool exists_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) = true; 
str generate_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"<dec(ts.bitmapField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))>;
	'return nodeOf(null, <use(bitmapField)>, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0, node);";		
	
bool exists_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) = true; 
str generate_bodyOf_mergeOnNextLevel(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"<dec(ts.bitmapField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))>;
	'return <toString(call(ts.nodeOf_BitmapIndexedNode, 
		argsOverride = (ts.mutator: NULL(),								
						ts.bitmapField: useExpr(ts.bitmapField),
						ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.valmapField.\type, "0")),						
						ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { <use(\node(ts.ds, ts.tupleTypes))> }"),
						ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "0")),
						ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "1")))))>;";	

default bool exists_bodyOf_mergeOnNextLevel(TrieSpecifics _, Option _, Position _)  = true;
default str generate_bodyOf_mergeOnNextLevel(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }

bool exists_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) = true; 
str generate_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionField()) =
	"// store values before node
	'return nodeOf(null, (byte) mask1, <use(__payloadTuple(ts.ds, ts.tupleTypes, 1))>, (byte) mask0, node0);";

bool exists_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) = true; 
str generate_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),true>}, Position pos:positionBitmap()) =
	"<dec(ts.bitmapField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))>;
	'<dec(ts.valmapField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>;
	'
	'// store values before node
	'return nodeOf(null, <use(bitmapField)>, <use(valmapField)>, <use(__payloadTuple(ts.ds, ts.tupleTypes, 1))>, node0);";		
	
bool exists_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) = true; 
str generate_bodyOf_mergeNodeAndValue(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, <useSpecialization(),false>}, Position _) =
	"<dec(ts.bitmapField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))>;
	'<dec(ts.valmapField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>;
	'
	'// store values before node
	'return <toString(call(ts.nodeOf_BitmapIndexedNode, 
		argsOverride = (ts.mutator: NULL(),								
						ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { <use(__payloadTuple(ts.ds, ts.tupleTypes, 1))>, <use(\node(ts.ds, ts.tupleTypes, 0))> }"),
								ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "1")),
								ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "1")))))>;";			

default bool exists_bodyOf_mergeNodeAndValue(TrieSpecifics _, Option _, Position _)  = true;
default str generate_bodyOf_mergeNodeAndValue(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }
		
str updatedOn_KeysEqual(TrieSpecifics ts, str(Argument, Argument) eq) = 
	"return this;" 
when \set() := ts.ds;	
	
str updatedOn_KeysEqual(TrieSpecifics ts, str(Argument, Argument) eq) = 
	"<dec(val(ts.valType, "currentVal"))> = getValue(dataIndex);
	'
	'if (<eq(val(ts.valType, "currentVal"), val(ts.valType))>) {
	'	return this;
	'} else {
	'	// update mapping
	'	details.updated(currentVal);
	'	return copyAndSetValue(mutator, bitpos, val);
	'}" 
when \map(multi = false) := ts.ds && isOptionEnabled(ts.setup, compareValueAtMapPut());

str updatedOn_KeysEqual(TrieSpecifics ts, str(Argument, Argument) eq) = 
	"<dec(val(ts.valType, "currentVal"))> = getValue(dataIndex);
	'
	'// update mapping
	'details.updated(currentVal);
	'return copyAndSetValue(mutator, bitpos, val);" 
when \map(multi = false) := ts.ds && !isOptionEnabled(ts.setup, compareValueAtMapPut());

// TODO: lost knowledge about 'customComparator'
// TODO: lost knowledge about 'artifact'
str updatedOn_KeysEqual(TrieSpecifics ts, str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"<dec(nodeTupleArg(ts, 1))> = getValue(dataIndex);
	'
	'final int valHash = <hashCode(val(ts.valType))>;
	'// if(<toString(call(nodeTupleArg(ts, 1), getDef(tsSet, trieNode(abstractNode()), containsKey()), 
					argsOverride = (key(tsSet.keyType): useExpr(val(ts.valType)), 
					tsSet.keyHash: call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ival("valHash")))), 
					tsSet.shift: constant(tsSet.shift.\type, "0"))))>) {
	'if(<toString(call(collTupleArg(ts, 1), getDef(tsSet, core(immutable()), containsKey()),
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>) {
	'	return this;
	'} else {
	'	// add new mapping
	'	// <dec(appendToName(nodeTupleArg(ts, 1), "New"))> = <toString(call(nodeTupleArg(ts, 1), getDef(tsSet, trieNode(abstractNode()), insertTuple()), 
					argsOverride = (ts.mutator: NULL(), 
						ts.keyHash: call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ival("valHash")))), 
						ts.shift: constant(ts.shift.\type, "0"),
						ts.details: exprFromString("<tsSet.ResultStr>.unchanged()")), // TODO: remove tsSet dependency here
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))
						))>;
	'	<dec(appendToName(collTupleArg(ts, 1), "New"))> = <toString(call(collTupleArg(ts, 1), getDef(tsSet, core(immutable()), insertTuple()), 
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>;
	'
	'	details.modified();
	'	return copyAndSetValue(mutator, bitpos, <use(appendToName(nodeTupleArg(ts, 1), "New"))>);
	'}" 
when \map(multi = true) := ts.ds;	







default str updatedOn_KeysDifferent(TrieSpecifics ts, str(Argument, Argument) eq) = 
	"<if (\map() := ts.ds) {><dec(val(ts.valType, "currentVal"))> = getValue(dataIndex);<}> final <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> subNodeNew = mergeTwoKeyValPairs(currentKey, <if (\map() := ts.ds) {> currentVal,<}><toString(call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): hashCodeExpr(ts, key(ts.keyType, "currentKey")))))>, key, <if (\map() := ts.ds) {> val,<}> keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>);
	'
	'details.modified();
	'return copyAndMigrateFromInlineToNode(mutator, bitpos, subNodeNew);";	
		
str updatedOn_KeysDifferent(TrieSpecifics ts, str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"final int valHash = <hashCode(val(ts.valType))>;
	'// <dec(nodeTupleArg(ts, 1))> = <toString(call(exprFromString("CompactSetNode.EMPTY_NODE"), getDef(tsSet, trieNode(abstractNode()), insertTuple()), 
					argsOverride = (ts.mutator: NULL(), 
						ts.keyHash: call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ival("valHash")))), 
						ts.shift: constant(ts.shift.\type, "0"),
						ts.details: exprFromString("<tsSet.ResultStr>.unchanged()")), // TODO: remove dependency on tsSet here
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))						
					))>;
	' <dec(collTupleArg(ts, 1))> = <tsSet.coreSpecializedClassName>.setOf(<use(val(ts.valType))>);
	'
	'<dec(replaceName(nodeTupleArg(ts, 1), "currentValNode"))> = getValue(dataIndex);
	'final <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> subNodeNew = mergeTwoKeyValPairs(currentKey, currentValNode, <toString(call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): hashCodeExpr(ts, key(ts.keyType, "currentKey")))))>, key, <use(collTupleArg(ts, 1))>, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>);
	'
	'details.modified();
	'return copyAndMigrateFromInlineToNode(mutator, bitpos, subNodeNew);" 
when \map(multi = true) := ts.ds;






default str updatedOn_NoTuple(TrieSpecifics ts, str(Argument, Argument) eq) = 
	"<use(ts.details)>.modified();
	'return copyAndInsertValue(mutator, bitpos, key<if (\map() := ts.ds) {>, val<}>);";
		
str updatedOn_NoTuple(TrieSpecifics ts, str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"final int valHash = <hashCode(val(ts.valType))>;
	'// <dec(nodeTupleArg(ts, 1))> = <toString(call(exprFromString("CompactSetNode.EMPTY_NODE"), getDef(tsSet, trieNode(abstractNode()), insertTuple()), 
					argsOverride = (ts.mutator: NULL(), 
						ts.keyHash: call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ival("valHash")))), 
						ts.shift: constant(ts.shift.\type, "0"),
						ts.details: exprFromString("<ts.ResultStr>.unchanged()")),
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))	
					))>;
	' <dec(collTupleArg(ts, 1))> = <tsSet.coreSpecializedClassName>.setOf(<use(val(ts.valType))>);
	'
	'details.modified();
	'return copyAndInsertValue(mutator, bitpos, <use(nodeTupleArgs(ts))>);"
when \map(multi = true) := ts.ds;



default bool exists_bodyOf_SpecializedBitmapPositionNode_updated(TrieSpecifics ts, str(Argument, Argument) eq)  = true;
default str generate_bodyOf_SpecializedBitmapPositionNode_updated(TrieSpecifics ts, str(Argument, Argument) eq) {
	
	Argument subNode 	= \node(ts.ds, ts.tupleTypes, "subNode");
	Argument subNodeNew = \node(ts.ds, ts.tupleTypes, "subNodeNew");

	return  
	"<dec(ts.mask)> = <toString(call(getDef(ts, trieNode(compactNode()), mask())))>;
	'<dec(ts.bitposField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos())))>;
	'
	'if ((<use(valmapMethod)> & bitpos) != 0) { // inplace value
	'	<dec(field(primitive("int"), "dataIndex"))> = dataIndex(bitpos);
	'	<dec(key(ts.keyType, "currentKey"))> = getKey(dataIndex);
	'
	'	if (<eq(key(ts.keyType, "currentKey"), key(ts.keyType))>) {
	'		<updatedOn_KeysEqual(ts, eq)>				
	'	} else {
	'		<updatedOn_KeysDifferent(ts, eq)>					
	'	}
	'} else if ((<use(bitmapMethod)> & bitpos) != 0) { // node (not value)
	'	<dec(subNode)> = nodeAt(bitpos);
	'	<dec(subNodeNew)> = <use(subNode)>.updated(mutator, <use(ts.payloadTuple)>, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>, <use(ts.details)><if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);
	'
	'	if (<use(ts.details)>.isModified()) {
	'		return copyAndSetNode(mutator, bitpos, <use(subNodeNew)>);
	'	} else {
	'		return this;
	'	}
	'} else {
	'	// no value
	'	<updatedOn_NoTuple(ts, eq)>
	'}"
	;
}
	

default str removedOn_TupleFound(TrieSpecifics ts, str(Argument, Argument) eq) =
	"<if (\map() := ts.ds) {><dec(val(ts.valType, "currentVal"))> = getValue(dataIndex); details.updated(currentVal);<} else {>details.modified();<}>
	
	<removed_value_block1(ts, ts.setup)> else <if (isOptionEnabled(ts.setup,useSpecialization())) {><removed_value_block2(ts, ts.setup)> else<}> {					
		return copyAndRemoveValue(mutator, bitpos);
	}";
	
str removedOn_TupleFound(TrieSpecifics ts, str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"<dec(nodeTupleArg(ts, 1))> = getValue(dataIndex); 
	'
	'final int valHash = <hashCode(val(ts.valType))>;
	'// if(<toString(call(nodeTupleArg(ts, 1), getDef(tsSet, trieNode(abstractNode()), containsKey()), 
					argsOverride = (key(tsSet.keyType): useExpr(val(ts.valType)), tsSet.keyHash: call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ival("valHash")))), tsSet.shift: constant(tsSet.shift.\type, "0"))))>) {
	' if(<toString(call(collTupleArg(ts, 1), getDef(tsSet, core(immutable()), containsKey()) 
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>) {
	'	details.updated(<use(val(ts.valType))>);
	'	
	'	// remove mapping
	'	// <dec(appendToName(nodeTupleArg(ts, 1), "New"))> = <toString(call(nodeTupleArg(ts, 1), getDef(tsSet, trieNode(abstractNode()), removeTuple()), 
					argsOverride = (ts.mutator: NULL(), 
						ts.keyHash: call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ival("valHash")))), 
						ts.shift: constant(ts.shift.\type, "0"),
						ts.details: exprFromString("<ts.ResultStr>.unchanged()")),
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))	
					))>;
	'	<dec(appendToName(nodeTupleArg(ts, 1), "New"))> = <toString(call(nodeTupleArg(ts, 1), getDef(tsSet, core(immutable()), removeTuple()), 
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>;
	'	
	'	if (<use(appendToName(nodeTupleArg(ts, 1), "New"))>.size() == 0) { // earlier: arity() == 0
	'		<removed_value_block1(ts, ts.setup)> else <if (isOptionEnabled(ts.setup,useSpecialization())) {><removed_value_block2(ts, ts.setup)> else<}> {					
	'			return copyAndRemoveValue(mutator, bitpos);
	'		}
	'	} else {
	'		return copyAndSetValue(mutator, bitpos, <use(appendToName(nodeTupleArg(ts, 1), "New"))>);		
	'	}	
	'} else {	
	'	return this;
	'}" 
when \map(multi = true) := ts.ds;
	
default bool exists_bodyOf_SpecializedBitmapPositionNode_removed(TrieSpecifics ts, PredefOp op, 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true;
default str generate_bodyOf_SpecializedBitmapPositionNode_removed(TrieSpecifics ts, PredefOp op,
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) {
		
	Argument subNode 	= \node(ts.ds, ts.tupleTypes, "subNode");
	Argument subNodeNew = \node(ts.ds, ts.tupleTypes, "subNodeNew");

	return
	"<dec(ts.mask)> = <toString(call(getDef(ts, trieNode(compactNode()), mask())))>;
	'<dec(ts.bitposField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos())))>;

	if ((<use(valmapMethod)> & bitpos) != 0) { // inplace value
		<dec(field(primitive("int"), "dataIndex"))> = dataIndex(bitpos);
		
		if (<eq(key(ts.keyType, "getKey(dataIndex)"), key(ts.keyType))>) {
			<removedOn_TupleFound(ts, eq)>
		} else {		
			return this;
		}
	} else if ((<use(bitmapMethod)> & bitpos) != 0) { // node (not value)
		<dec(subNode)> = nodeAt(bitpos);
		<dec(subNodeNew)> = <toString(call(subNode, getDef(ts, trieNode(abstractNode()), op), argsOverride = (ts.shift: exprFromString("shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>"))))>;

		if (!<use(ts.details)>.isModified()) {
			return this;
		}
		
		switch (subNodeNew.sizePredicate()) {
		case 0: {
			throw new IllegalStateException(\"Sub-node must have at least one element.\"); 
		}
		case 1: {
			<if (isOptionEnabled(ts.setup,useSpecialization())) {>// inline value (move to front)
				details.modified();
				return copyAndMigrateFromNodeToInline(mutator, bitpos, subNodeNew);<} else {>if (this.payloadArity() == 0 && this.nodeArity() == 1) {
				// escalate (singleton or empty) result
				return <use(subNodeNew)>;
			} else {
				// inline value (move to front)
				return copyAndMigrateFromNodeToInline(mutator, bitpos, subNodeNew);
			}<}>
		}
		default: {
			// modify current node (set replacement node)
			return copyAndSetNode(mutator, bitpos, subNodeNew);
		}
		}		
	}

	return this;";
}
	
default str removed_value_block2(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) =
	"if (this.arity() == <nBound + 1>) {
	'	return removeInplaceValueAndConvertToSpecializedNode(mutator, bitpos);
	'}";
	
default str removed_value_block1(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) =
	"if (this.payloadArity() == 2 && this.nodeArity() == 0) {
	'	/*
	'	 * Create new node with remaining pair. The new node
	'	 * will a) either become the new root returned, or b)
	'	 * unwrapped and inlined during returning.
	'	 */
	'	final <typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))> newDataMap = (shift == 0) ? (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<use(valmapMethod)> ^ bitpos)
	'					: <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.shift: constant(primitive("int"), "0"))))))>;
	'
	'	if (dataIndex == 0) {
	'		return <CompactNode(ts.ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0,
	'						newDataMap, getKey(1)<if (\map() := ds) {>, getValue(1)<}>);
	'	} else {
	'		return <CompactNode(ts.ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0,
	'						newDataMap, getKey(0)<if (\map() := ds) {>, getValue(0)<}>);
	'	}
	'}";
	
	
// bit-shifting with signed integers in Java
int oneShiftedLeftBy(int count) = toInt(pow(2, count)) when count >= 0 && count <= 30;
int oneShiftedLeftBy(31) = -2147483648;
default int oneShiftedLeftBy(int count) { throw "Not supported!"; }




list[PredefOp] createNodeFactorySpecializationList(TrieSpecifics ts, TrieNodeType nodeType:compactNode()) 
	= [ nodeFactory_Specialization(i, j) | j <- [0..ts.nMax+1], i <- [0..ts.nMax+1], ((i + j) <= ts.nMax && (i + j) <= ts.nBound + 1)]; 

data PredefOp = nodeFactory_Specialization(int n, int m);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Specialization(int n, int m))
	= function(ts.compactNodeClassReturn, "nodeOf<n>x<m>", generics = ts.genericTupleTypes, args = [ ts.mutator ] + metadataArguments(ts) + contentArguments(n, m, ts), argsFilter = ts.argsFilter);
// isActive = isOptionEnabled(ts.setup, useSpecialization())

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Specialization(int n, int m)) = true;

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Specialization(int n, int m)) =
	generate_bodyOf_factoryMethod_bitmap(n, m, ts, CompactNode_factoryMethod_bitmap(n, m, ts))
; // when isOptionEnabled(ts.setup, useSpecialization()) && !isOptionEnabled(ts.setup,useUntypedVariables());


str generate_specializationFactoryMethods(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = 
	"<for(j <- [0..ts.nMax+1], i <- [0..ts.nMax+1], ((i + j) <= ts.nMax && (i + j) <= ts.nBound + 1)) {>
	'	<implOrOverride(CompactNode_factoryMethod_bitmap(i, j, ts), generate_bodyOf_factoryMethod_bitmap(i, j, ts, CompactNode_factoryMethod_bitmap(i, j, ts)))>
	'<}>"
when isOptionEnabled(ts.setup,useSpecialization()) && !isOptionEnabled(ts.setup,useUntypedVariables());
	
/**
 * More complicated slot count expression.
 * sort(toList({ n + 2 * m | n <- [0..32+1], m <- [0..32+1], ((n + m) <= 32)}))
 */
str generate_specializationFactoryMethods(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = 
	"<for(mn <- [0.. tupleLength(ts.ds) * ts.nMax + 1], mn <= tupleLength(ts.ds) * ts.nBound + tupleLength(ts.ds)) {>
	'	<generate_valNodeOf_factoryMethod_bitmap_untyped(mn, ts, ts.setup)>
	'<}>"
when isOptionEnabled(ts.setup,useSpecialization()) && isOptionEnabled(ts.setup,useUntypedVariables());

default str generate_specializationFactoryMethods(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = "";




Method CompactNode_factoryMethod_bitmap(int n, int m, TrieSpecifics ts) {
	constructorArgs = ts.mutator + metadataArguments(ts) + contentArguments(n, m, ts);

	return function(ts.compactNodeClassReturn, "nodeOf", generics = ts.tupleTypes, args = constructorArgs);	
}

str generate_bodyOf_factoryMethod_bitmap(int n:0, int m:0, TrieSpecifics ts, Method decleration) { 
	return "return <emptyTrieNodeConstantName>;";
}

//bool exists_valNodeOf_factoryMethod_bitmap(n:1, m:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup: = true;
//str generate_valNodeOf_factoryMethod_bitmap(n:1, m:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, compactionViaFieldToMethod()}) {
//	// TODO: remove code duplication
//	constructorArgs = ts.mutator + metadataArguments(ts) + contentArguments(n, m, ts);
//
//	specializedClassNameStr = "<toString(ts.ds)><m>To<n>Node<ts.classNamePostfix>";
//
//	return
//	"static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
//	'	switch(<use(bitmapMethod)>) {
//	'	<for (i <- [1..ts.nMax+1]) {>case <oneShiftedLeftBy(i-1)>:
//	'		return new <toString(ts.ds)><m>To<n>NodeAtMask<i-1><InferredGenerics(ts.ds, ts.tupleTypes)>(<intercalate(", ", mapper(constructorArgs, use))>);
//	'	<}>default:
//	'		throw new IllegalStateException(\"Index out of range.\");
//	'	}
//	'}"
//	;
//}

str generate_bodyOf_factoryMethod_bitmap(int n, int m, TrieSpecifics ts, Method decleration) 
	= "return new <specializedClassNameStr><InferredGenerics(ts.ds, ts.tupleTypes)>(<use(decleration.args)>);"
when (n + m) <= ts.nBound
		&& specializedClassNameStr := "<toString(ts.ds)><m>To<n>Node<ts.classNamePostfix>";

/* TODO: fix argument lists! */
str generate_bodyOf_factoryMethod_bitmap(int n, int m, TrieSpecifics ts, Method decleration) {
	if (!((n + m) == ts.nBound + 1 && (n + m) < ts.nMax)) {
		fail;
	}

	list[Argument] argsForArray = contentArguments(n, m, ts);

	return "return nodeOf(mutator, <bitmapField.name>, <valmapField.name>, new Object[] { <use(argsForArray)> }<if (!isOptionEnabled(ts.setup,useSandwichArrays())) {>, (byte) <m><}>);";
}

default str generate_bodyOf_factoryMethod_bitmap(int n, int m, TrieSpecifics ts, Method decleration) { 
	throw "Arguments out of bounds (n = <n>, m = <m>)."; 
}


bool exists_valNodeOf_factoryMethod_bitmap_untyped(mn:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, int n = 0, int m = 0) = true; 
str generate_valNodeOf_factoryMethod_bitmap_untyped(mn:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, int n = 0, int m = 0) { 
	// TODO: remove code duplication
	constructorArgs = ts.mutator + metadataArguments(ts) + contentArguments(n, m, ts);

	return
	"static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
	'	return <emptyTrieNodeConstantName>;
	'}"
	;
}

//bool exists_valNodeOf_factoryMethod_bitmap_untyped(n:1, m:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup: = true;
//str generate_valNodeOf_factoryMethod_bitmap_untyped(n:1, m:0, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup:{_*, compactionViaFieldToMethod()}) {
//	// TODO: remove code duplication
//	constructorArgs = ts.mutator + metadataArguments(ts) + contentArguments(n, m, ts);
//
//	specializedClassNameStr = "<toString(ts.ds)><m>To<n>Node<ts.classNamePostfix>";
//
//	return
//	"static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
//	'	switch(<use(bitmapMethod)>) {
//	'	<for (i <- [1..ts.nMax+1]) {>case <oneShiftedLeftBy(i-1)>:
//	'		return new <toString(ts.ds)><m>To<n>NodeAtMask<i-1><InferredGenerics(ts.ds, ts.tupleTypes)>(<intercalate(", ", mapper(constructorArgs, use))>);
//	'	<}>default:
//	'		throw new IllegalStateException(\"Index out of range.\");
//	'	}
//	'}"
//	;
//}

bool exists_valNodeOf_factoryMethod_bitmap_untyped(int mn, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, int n = mn, int m = 0) = true;
default str generate_valNodeOf_factoryMethod_bitmap_untyped(int mn, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, int n = mn, int m = 0) {
	// TODO: remove code duplication
	constructorArgs = ts.mutator + metadataArguments(ts) + contentArguments(n, m, ts);

	specializedClassNameStr = "<toString(ts.ds)><m>To<n>Node<ts.classNamePostfix>";

	if ((mn) <= tupleLength(ts.ds) * ts.nBound) {		
		return
		"static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return new <specializedClassNameStr><InferredGenerics(ts.ds, ts.tupleTypes)>(<intercalate(", ", mapper(constructorArgs, use))>);
		'}
		"; 
	} else if ((mn) > tupleLength(ts.ds) * ts.nBound && (mn) <= tupleLength(ts.ds) * ts.nBound + tupleLength(ts.ds) && (mn) < tupleLength(ts.ds) * ts.nMax) {
		list[Argument] argsForArray = contentArguments(n, m, ts);

		return
		"static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return nodeOf(mutator, <bitmapField.name>, <valmapField.name>, new Object[] { <use(argsForArray)> }<if (!isOptionEnabled(ts.setup,useSandwichArrays())) {>, (byte) <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapField)>)<}>);
		'}
		";
	} else {
		throw "Arguments out of bounds (n = <n>, m = <m>).";
	}
}






list[Argument] invoke_getAndHashCode_for_payloadTuple(DataStructure ds:\map(), list[Type] tupleTypes:[keyType, valType, *_], Argument idx) = [ key(keyType, "Objects.hashCode(getKey(<use(idx)>))"), val(valType, "Objects.hashCode(getValue(<use(idx)>))") ];
list[Argument] invoke_getAndHashCode_for_payloadTuple(DataStructure ds:\set(), list[Type] tupleTypes:[keyType, *_], Argument idx) = [ key(keyType, "Objects.hashCode(getKey(<use(idx)>))") ];




	
	
default bool exists_bodyOf_mergeTwoKeyValPairs(TrieSpecifics ts)  = true;
default str generate_bodyOf_mergeTwoKeyValPairs(TrieSpecifics ts) = 
	"assert !(<equalityDefaultForArguments(key(ts.keyType, "key0"), key(ts.keyType, "key1"))>);
	
	if (<use(ts.shift)> \>= <toString(call(getDef(ts, trieNode(compactNode()), hashCodeLength())))>) {
		// throw new IllegalStateException(\"Hash collision not yet fixed.\");
		return new <ts.hashCollisionClassName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash0, (<typeToString(nodeTupleType(ts, 0))>[]) new <if (isPrimitive(nodeTupleArg(ts, 0))) {><typeToString(nodeTupleType(ts, 0))><} else {>Object<}>[] { <use(appendToName([ "0", "1" ], nodeTupleArg(ts, 0)))> }
						<if (\map() := ts.ds) {>, (<typeToString(nodeTupleType(ts, 1))>[]) new <if (isPrimitive(nodeTupleArg(ts, 1))) {><typeToString(nodeTupleType(ts, 1))><} else {>Object<}>[] { <use(appendToName([ "0", "1" ], nodeTupleArg(ts, 1)))> }<}>);
	}

	<dec(ts.mask0)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash0))))>;
	<dec(ts.mask1)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash1))))>;

	if (mask0 != mask1) {
		// both nodes fit on same level
		<generate_bodyOf_mergeTwoValues(ts, ts.setup, positionBitmap())>
	} else {
		final <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> node = <toString(call(getDef(ts, trieNode(compactNode()), mergeTwoKeyValPairs()), argsOverride = (ts.shift: plus(useExpr(ts.shift), call(getDef(ts, trieNode(compactNode()), bitPartitionSize()))))))>;
		// values fit on next level

		<generate_bodyOf_mergeOnNextLevel(ts, ts.setup, positionBitmap())>
	}";	
	
bool exists_bodyOf_mergeTwoKeyValPairs(TrieSpecifics ts)  = true;
str generate_bodyOf_mergeTwoKeyValPairs(TrieSpecifics ts) = 
	"assert !(<equalityDefaultForArguments(key(ts.keyType, "key0"), key(ts.keyType, "key1"))>);

	if (keyHash0 == keyHash1) {
		return new <ts.hashCollisionClassName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash0, (<typeToString(ts.keyType)>[]) new <if (isPrimitive(ts.keyType)) {><typeToString(ts.keyType)><} else {>Object<}>[] { key0, key1 }
						<if (\map() := ts.ds) {>, (<typeToString(ts.valType)>[]) new <if (isPrimitive(ts.valType)) {><typeToString(ts.valType)><} else {>Object<}>[] { val0, val1 }<}>);
	}

	int originalShift = shift;

	<dec(ts.mask0)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash0))))>;
	<dec(ts.mask1)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash1))))>;

	while (mask0 == mask1) {
		shift += <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>;

		mask0 = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash0))))>;
		mask1 = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash1))))>;
	}

	final int dataMap = bitpos(mask0) | bitpos(mask1);
	final Object[] content;
	
	if (mask0 \< mask1) {
		content = new Object[] { <use([ *__payloadTuple(ts.ds, ts.tupleTypes, 0), *__payloadTuple(ts.ds, ts.tupleTypes, 1) ])> };
	} else {
		content = new Object[] { <use([ *__payloadTuple(ts.ds, ts.tupleTypes, 1), *__payloadTuple(ts.ds, ts.tupleTypes, 0) ])> };
	}
			
	// TODO: old semantics; fixme, I am still wrong
	if (shift != originalShift) {
		// apply path compression (TODO: correct allocation)
		return new PathCompressedBitmapIndexedMapNode_ValuesOnly\<\>(null, dataMap, content, shift, prefix(keyHash0, shift));
	} else {
		// path compression not necessary (TODO: correct allocation)
		return new BitmapIndexedMapNode_ValuesOnly\<\>(null, dataMap, content);
	}"
when isOptionEnabled(ts.setup, usePathCompression());
		
default bool exists_bodyOf_mergeNodeAndKeyValPair(TrieSpecifics ts)  = true;
default str generate_bodyOf_mergeNodeAndKeyValPair(TrieSpecifics ts) = 
	"<dec(ts.mask0)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash0))))>;
	<dec(ts.mask1)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash1))))>;

	if (mask0 != mask1) {
		// both nodes fit on same level
		<generate_bodyOf_mergeNodeAndValue(ts, ts.setup, positionBitmap())>
	} else {
		// values fit on next level
		final <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> node = <toString(call(getDef(ts, trieNode(compactNode()), mergeNodeAndKeyValPair()), argsOverride = (ts.shift: plus(useExpr(ts.shift), call(getDef(ts, trieNode(compactNode()), bitPartitionSize()))))))>;
		
		<generate_bodyOf_mergeOnNextLevel(ts, ts.setup, positionBitmap())>
	}";	

bool exists_bodyOf_mergeNodeAndKeyValPair(TrieSpecifics ts) = true;
str generate_bodyOf_mergeNodeAndKeyValPair(TrieSpecifics ts) = 
	"if (keyHash0 == keyHash1) {
		return new <ts.hashCollisionClassName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash0, (<typeToString(ts.keyType)>[]) new <if (isPrimitive(ts.keyType)) {><typeToString(ts.keyType)><} else {>Object<}>[] { key0, key1 }
						<if (\map() := ts.ds) {>, (<typeToString(ts.valType)>[]) new <if (isPrimitive(ts.valType)) {><typeToString(ts.valType)><} else {>Object<}>[] { val0, val1 }<}>);
	}

	int originalShift = shift;

	<dec(ts.mask0)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash0))))>;
	<dec(ts.mask1)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash1))))>;

	while (mask0 == mask1) {
		shift += <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>;

		mask0 = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash0))))>;
		mask1 = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash1))))>;
	}
	
	// both nodes fit on same level
	final int nodeMap = bitpos(mask0);
	final int dataMap = bitpos(mask1);

	// store values before node
	final Object[] content = new Object[] { key1, val1, node0 };
	
	// TODO: old semantics; fixme, I am still wrong
	if (shift != originalShift) {
		// apply path compression (TODO: correct allocation)
		return new PathCompressedBitmapIndexedMapNode_Mixed\<\>(null, nodeMap, dataMap,
						content, shift, prefix(keyHash0, shift));

	} else {
		// path compression not necessary (TODO: correct allocation)
		return new BitmapIndexedMapNode_Mixed\<\>(null, nodeMap, dataMap, content);
	}"
when isOptionEnabled(ts.setup, usePathCompression());
