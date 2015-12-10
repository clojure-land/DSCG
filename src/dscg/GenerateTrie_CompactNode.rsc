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
import String;

import dscg::Common;
import dscg::CoreModel;

str generateCompactNodeClassString(TrieSpecifics ts, bool isLegacy:false) {  
	str result = "";

	for(nt <- carrier(ts.model.refines), nt is compactNode) {
		if (nt has bitmapSpecialization) {
			JavaDataType jdt = compactNode(ts, nt, modifierList = [ "private", "abstract", "static" ]);
			result += generateJdtString(ts, jdt, nt);
			result += "\n\n";
		} else {	
			JavaDataType jdt = compactNode(ts, modifierList = [ "private", "abstract", "static" ]);
			result += generateJdtString(ts, jdt, nt);
			result += "\n\n";
		}
	}
		
	return result;
}


data PredefOp = isRare1();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRare1())
	= function(\return(primitive("boolean")), "isRare", args = [ ts.stdObjectArg ], isActive = isOptionEnabled(ts, useHeterogeneousEncoding()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRare1()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRare1())
	= "throw new UnsupportedOperationException(); // TODO: to implement";


data PredefOp = isRare2();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRare2())
	= function(\return(primitive("boolean")), "isRare", args = [ ts.stdObjectArg0, ts.stdObjectArg1 ], isActive = isOptionEnabled(ts, useHeterogeneousEncoding()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRare2()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRare2())
	= "throw new UnsupportedOperationException(); // TODO: to implement";
	
	
data PredefOp = isRareBitpos();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRareBitpos())
	= function(\return(primitive("boolean")), "isRare", args = [ ts.bitposField ], isActive = isOptionEnabled(ts, useHeterogeneousEncoding()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRareBitpos()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isRareBitpos())
	= "throw new UnsupportedOperationException(); // TODO: to implement";	


// TODO: factor logic used in this constructor implementation
data PredefOp = constructor();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode(BitmapSpecialization bs)), PredefOp::constructor())
	= constructor(\return(\type), jdt.typeName, args = [ ts.mutator ] + fieldList, visibility = "private", argsFilter = argsFilter(ts)) // metadataArguments(ts)
when jdt := compactNode(ts, nodeType) && 
		\type := jdtToType(jdt) &&
		fieldList := [ predefOpToArgument(ts, artifact, op) | op <- declares(ts, nodeType)<1>, op != constructor(), opDef := getDef(ts, artifact, op), opDef is property, opDef.isActive && opDef.isStateful && opDef.initializeAtConstruction ];
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode(BitmapSpecialization bs)), PredefOp::constructor()) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode(BitmapSpecialization bs)), PredefOp::constructor())
	= compoundStatement([
		uncheckedStringStatement(initFieldsWithIdendity(fieldList)) // TODO: automatically infer which def.args need to be initialized
	])
when def := getDef(ts, artifact, constructor()) &&
		fieldList := [ predefOpToArgument(ts, artifact, op) | op <- declares(ts, nodeType)<1>, op != constructor(), opDef := getDef(ts, artifact, op), opDef is property, opDef.isActive && opDef.isStateful && opDef.initializeAtConstruction ];


data PredefOp = nodeMap();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeMap())
	= method(ts.bitmapField, ts.bitmapField.name);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeMap()) = isOptionEnabled(ts, useHeterogeneousEncoding());

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeMap())
	= result(maskAndNarrowBitmapCast(ts, exprFromString("rawMap1() ^ rareMap()")), isActive = isOptionEnabled(ts, useHeterogeneousEncoding()));

Expression maskAndNarrowBitmapCast(TrieSpecifics ts, Expression sourceExpression) = maskAndNarrowPrimitiveCast(
	sourceExpression,
	integerOrLongPrimitiveType(ts.bitPartitionSize),
	chunkSizeToPrimitive(ts.bitPartitionSize));
	
Expression maskAndWidenBitmapCast(TrieSpecifics ts, Expression sourceExpression) = maskAndWidenPrimitiveCast(
	sourceExpression,
	chunkSizeToPrimitive(ts.bitPartitionSize),
	integerOrLongPrimitiveType(ts.bitPartitionSize));	

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::nodeMap())
	= property(ts.bitmapField, ts.bitmapField.name, isStateful = true, isConstant = false, hasGetter = true, initializeAtConstruction = true, isActive = !isOptionEnabled(ts, useHeterogeneousEncoding()))
when bs.supportsNodes;

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::nodeMap())
	= property(ts.bitmapField, ts.bitmapField.name, isStateful = false, isConstant = false, hasGetter = true, isActive = !isOptionEnabled(ts, useHeterogeneousEncoding()))
when !bs.supportsNodes;

// Default Value for Property
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::nodeMap()) = true;
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::nodeMap())
	= result(iconst(0));


data PredefOp = dataMap();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::dataMap())
	= method(ts.valmapField, ts.valmapField.name);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::dataMap()) = isOptionEnabled(ts, useHeterogeneousEncoding());

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::dataMap())
	= result(maskAndNarrowBitmapCast(ts, exprFromString("rawMap2() ^ rareMap()")), isActive = isOptionEnabled(ts, useHeterogeneousEncoding()));

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::dataMap())
	= property(ts.valmapField, ts.valmapField.name, isStateful = true, isConstant = false, hasGetter = true, initializeAtConstruction = true, isActive = !isOptionEnabled(ts, useHeterogeneousEncoding()))
when bs.supportsValues;

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::dataMap())
	= property(ts.valmapField, ts.valmapField.name, isStateful = false, isConstant = false, hasGetter = true, isActive = !isOptionEnabled(ts, useHeterogeneousEncoding()))
when !bs.supportsValues;

// Default Value for Property
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::dataMap()) = true;
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::dataMap())
	= result(iconst(0));


//data PredefOp = rawMap1();
//
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::rawMap1())
//	= method(ts.valmapField, "rawMap1", isActive = isOptionEnabled(ts, useHeterogeneousEncoding())); // TODO: fix reference in return
//
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rawMap1())
//	= property(ts.valmapField, "rawMap1");
//
//
//data PredefOp = rawMap2();
//
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::rawMap2())
//	= method(ts.valmapField, "rawMap2", isActive = isOptionEnabled(ts, useHeterogeneousEncoding())); // TODO: fix reference in return


data PredefOp = rawMap1();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::rawMap1())
	= method(ts.valmapField, "rawMap1", isActive = true);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::rawMap1())
	= property(ts.valmapField, "rawMap1", isStateful = true, isConstant = false, hasGetter = true, initializeAtConstruction = true, isActive = isOptionEnabled(ts, useHeterogeneousEncoding()))
when bs.supportsValues || bs.supportsNodes;

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::rawMap1())
	= property(ts.valmapField, "rawMap1", isStateful = false, isConstant = false, hasGetter = true, initializeAtConstruction = true, isActive = isOptionEnabled(ts, useHeterogeneousEncoding()))
when !(bs.supportsValues || bs.supportsNodes);

// Default Value for Property
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::rawMap1()) = true;

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::rawMap1())
	= result(iconst(0));


data PredefOp = rawMap2();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::rawMap2())
	= property(ts.valmapField, "rawMap2", isStateful = true, isConstant = false, hasGetter = true);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::rawMap2())
	= property(ts.valmapField, "rawMap2", isStateful = true, isConstant = false, hasGetter = true, initializeAtConstruction = true, isActive = isOptionEnabled(ts, useHeterogeneousEncoding()))
when bs.supportsValues;

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::rawMap2())
	= property(ts.valmapField, "rawMap2", isStateful = false, isConstant = false, hasGetter = true, initializeAtConstruction = true, isActive = isOptionEnabled(ts, useHeterogeneousEncoding()))
when !bs.supportsValues;

// Default Value for Property
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::rawMap2()) = true;

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode(BitmapSpecialization bs)), PredefOp::rawMap2())
	= result(iconst(0));


data PredefOp = rareMap();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::rareMap())
	= method(ts.valmapField, "rareMap", isActive = isOptionEnabled(ts, useHeterogeneousEncoding())); // TODO: fix reference in return

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::rareMap()) = true;

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::rareMap())
	= result(maskAndNarrowBitmapCast(ts, exprFromString("rawMap1() & rawMap2()")), isActive = isOptionEnabled(ts, useHeterogeneousEncoding()));

data PredefOp = nodeIndex();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), nodeIndex())
	=  method(\return(primitive("int")), "nodeIndex", args = [ts.bitposField]);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), nodeIndex()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), nodeIndex())
	= "return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(___bitmapMethod(ts.bitPartitionSize))> & (bitpos - 1));";


data PredefOp = dataIndex();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), dataIndex())
	=  method(\return(primitive("int")), "dataIndex", args = [ts.bitposField]);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), dataIndex()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), dataIndex())
	= "return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(___valmapMethod(ts.bitPartitionSize))> & (bitpos - 1));";


data PredefOp = rareIndex();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), rareIndex())
	=  method(\return(primitive("int")), "rareIndex", args = [ts.bitposField], isActive = isOptionEnabled(ts, useHeterogeneousEncoding()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), rareIndex()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), rareIndex())
	= "return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(___raremapMethod(ts.bitPartitionSize))> & (bitpos - 1));";


data PredefOp = hashCodeLength();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::hashCodeLength())
	= property(\return(primitive("int")), "hashCodeLength", isStateful = false, isConstant = true);
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::hashCodeLength()) = true;
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::hashCodeLength())
	= result(iconst(32));


data PredefOp = bitPartitionSize();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::bitPartitionSize())
	= property(\return(primitive("int")), "bitPartitionSize", isStateful = false, isConstant = true);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::bitPartitionSize()) = true;
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::bitPartitionSize())
	= result(iconst(ts.bitPartitionSize));


data PredefOp = bitPartitionMask();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::bitPartitionMask())
	= property(\return(primitive("int")), "bitPartitionMask", isStateful = false, isConstant = true);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::bitPartitionMask()) = true;
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::bitPartitionMask())
	= result(binaryLiteralOfOnes(ts.bitPartitionSize));


data PredefOp = nodeInvariant();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeInvariant())
	= method(\return(primitive("boolean")), "nodeInvariant");

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeInvariant()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeInvariant()) = 
	"boolean inv1 = (size() - payloadArity() \>= 2 * (arity() - payloadArity()));
	'boolean inv2 = (this.arity() == 0) ? sizePredicate() == sizeEmpty() : true;
	'boolean inv3 = (this.arity() == 1 && payloadArity() == 1) ? sizePredicate() == sizeOne() : true;
	'boolean inv4 = (this.arity() \>= 2) ? sizePredicate() == sizeMoreThanOne() : true;
	'boolean inv5 = (this.nodeArity() \>= 0) && (this.payloadArity() \>= 0) && ((this.payloadArity() + this.nodeArity()) == this.arity());
	'
	'return inv1 && inv2 && inv3 && inv4 && inv5;";


data PredefOp = isTrieStructureValid();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeInvariant())
	= method(\return(primitive("boolean")), "nodeInvariant");


data PredefOp = nodeAt();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), nodeAt())
	= method(\return(jdtToType(abstractNode(ts))), "nodeAt", args = [ts.bitposField]);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeAt()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeAt()) =
	"return getNode(nodeIndex(bitpos));";


data PredefOp = recoverMask();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::recoverMask())
	= function(\return(primitive("byte")), "recoverMask", args = [ var(primitive("int"), "map"), val(primitive("byte"), "i_th") ]);
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::recoverMask()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::recoverMask()) =
	"assert 1 \<= i_th && i_th \<= <ts.nMax>;
	'
	'byte cnt1 = 0;
	'byte mask = 0;
	'
	'while (mask \< <ts.nMax>) {
	'	if ((map & 0x01) == 0x01) {
	'		cnt1 += 1;
	'
	'		if (cnt1 == i_th) {
	'			return mask;
	'		}
	'	}
	'
	'	map = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (map \>\> 1);
	'	mask += 1;
	'}
	'	
	'assert cnt1 != i_th;
	'throw new RuntimeException(\"Called with invalid arguments.\");";	
		

data PredefOp = toString();

/* 
 * visibility is enforced through Object.toString 
 */
@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::toString())
	= method(\return(specific("java.lang.String")), "toString", visibility = "public");

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::toString()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::toString()) =
	"final StringBuilder bldr = new StringBuilder();
	'bldr.append(\'[\');
	'for (byte i = 0; i \< payloadArity(); i++) {
	'	final byte pos = recoverMask(<use(valmapMethod)>, (byte) (i + 1));
	'	bldr.append(String.format(\"@%d\<<intercalate(",", times("#%d", size(ts.payloadTuple)))>\>\", pos, <use(invoke_getAndHashCode_for_payloadTuple(ts.ds, ts.tupleTypes, field("i")))>));
	'	if (!((i + 1) == payloadArity())) {
	'		bldr.append(\", \");
	'	}
	'}
	'if (payloadArity() \> 0 && nodeArity() \> 0) {
	'	bldr.append(\", \");
	'}
	'for (byte i = 0; i \< nodeArity(); i++) {
	'	final byte pos = recoverMask(<use(bitmapMethod)>, (byte) (i + 1));
	'	bldr.append(String.format(\"@%d: %s\", pos, getNode(i)));
	'	if (!((i + 1) == nodeArity())) {
	'		bldr.append(\", \");
	'	}
	'}
	'bldr.append(\']\');
	'return bldr.toString();";


/* DEPRECATED: DELETE
	<if (!isOptionEnabled(ts,useSpecialization()) || ts.nBound < ts.nMax) {>
	'	<toString(UNCHECKED_ANNOTATION())>
	'	static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(AtomicReference\<Thread\> mutator) {
	'		return <emptyTrieNodeConstantName>;
	'	}
	<} else {>
	'	// TODO: consolidate and remove
	'	static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(AtomicReference\<Thread\> mutator) {
	'		<toString(result(call(getDef(ts, trieNode(compactNode()), emptyTrieNodeConstant()))))>;
	'	}
	<}>

	<if (!isOptionEnabled(ts,useSpecialization())) {>
	'	static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>, <dec(nodeTupleArgs(ts))>) {
	'		assert <use(bitmapField)> == 0;	
	'		return <toString(call(ts.nodeOf_BitmapIndexedNode, 
				argsOverride = (ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.bitmapField.\type, "0")),						
								ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { <use(nodeTupleArgs(ts))> }"),
								ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "1")),
								ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "0")))))>;
	'	}
	<}>
*/

data PredefOp = nodeFactory_Array();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Array())
	= function(\return(jdtToType(compactNode(ts))), "nodeOf", generics = ts.genericTupleTypes, args = [ ts.mutator, ts.bitmapField, ts.valmapField, ts.BitmapIndexedNode_contentArray, ts.BitmapIndexedNode_payloadArity, ts.BitmapIndexedNode_nodeArity ], argsFilter = argsFilter(ts), isActive = !isOptionEnabled(ts,useSpecialization()) || ts.nBound < ts.nMax);


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Array()) = true;
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Array()) 
	= result(call(ts.BitmapIndexedNode_constructor, inferredGenericsStr = InferredGenerics(ts.ds, ts.tupleTypes)));


data PredefOp = nodeFactory_Empty();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Empty())
	= function(\return(jdtToType(abstractNode(ts))), "nodeOf", generics = ts.genericTupleTypes, args = [ ts.mutator ], argsFilter = argsFilter(ts), isActive = !isOptionEnabled(ts,useSpecialization()) || ts.nBound < ts.nMax);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Empty()) = true;
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Empty()) 
	= result(call(getDef(ts, core(unknownUpdateSemantic()), emptyTrieNodeConstant())));


data PredefOp = nodeFactory_Singleton();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Singleton())
	= function(\return(jdtToType(abstractNode(ts))), "nodeOf", generics = ts.genericTupleTypes, args = [ ts.mutator, ts.bitmapField, ts.valmapField, *ts.payloadTuple ], argsFilter = argsFilter(ts), isActive = !isOptionEnabled(ts,useSpecialization()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Singleton()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Singleton()) =
	"assert <use(bitmapField)> == 0;	
	'return <toString(call(ts.nodeOf_BitmapIndexedNode, 
		argsOverride = (ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.bitmapField.\type, "0")),						
						ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { <use(nodeTupleArgs(ts))> }"),
						ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "1")),
						ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "0")))))>;";


str generate_copyAnd_generalPrelude(TrieSpecifics ts, Artifact artifact, PredefOp op) =
	"
	final Class srcClass = this.getClass();
	final Class dstClass = <generate_copyAnd_dstClass(ts, artifact, op)>;
	
	if (dstClass == null) {
		throw new RuntimeException(String.format(\"[%s] No new specialization [payloadArity=%d, nodeArity=%d].\", srcClass.getName(), payloadArity(), nodeArity()));
	}
	
	<dec(jdtToVal(compactNode(ts), "src"))> = this;
	<dec(jdtToVal(compactNode(ts), "dst"))> = <toString(cast(jdtToType(compactNode(ts)), exprFromString("unsafe.allocateInstance(dstClass)")))>;				
				
	final long[] srcArrayOffsets = (long[]) unsafe.getObject(srcClass, globalArrayOffsetsOffset);	
	final long[] dstArrayOffsets = (long[]) unsafe.getObject(dstClass, globalArrayOffsetsOffset);
		
	long srcOffset = arrayBase;
	long dstOffset = arrayBase;
	";
	
str generate_equalsFunctionUnsafe_generalPrelude(TrieSpecifics ts, Artifact artifact, PredefOp op) =
	"<dec(jdtToVal(compactNode(ts), "src"))> = (<typeToString(jdtToType(compactNode(ts)))>) o1;
	'<dec(jdtToVal(compactNode(ts), "dst"))> = (<typeToString(jdtToType(compactNode(ts)))>) o2;				
	'
	'final Class clazz = o1.getClass();
	'final long[] arrayOffsets = (long[]) unsafe.getObject(clazz, globalArrayOffsetsOffset);";	

str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op)
	= "<if (/^<constructorName:.*>\(\)$/ := "<op>") {><"<constructorName>_nextClass()"><}>"
when isOptionEnabled(ts, useSunMiscUnsafe()) && !isOptionEnabled(ts, unsafeCodeAsData()); 

str generate_copyAnd_dstClass_string(str mNext, str nNext) = "specializationsByContentAndNodes[<mNext>][<nNext>]";





str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndInsertValue(_), str m = "payloadArity()", str n = "nodeArity()")
	= generate_copyAnd_dstClass_string("<m> + 1", n)
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && !isOptionEnabled(ts, useHeterogeneousEncoding());

str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndInsertValue(bool isRare:false), str m = "payloadArity()", str n = "untypedSlotArity()")
	= generate_copyAnd_dstClass_string("<m> + 1", n)
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && isOptionEnabled(ts, useHeterogeneousEncoding());

str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndInsertValue(bool isRare:true), str m = "payloadArity()", str n = "untypedSlotArity()")
	= generate_copyAnd_dstClass_string(m, "<n> + <use(tupleLengthConstant)>")
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && isOptionEnabled(ts, useHeterogeneousEncoding());






str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndRemoveValue(bool isRare), str m = "payloadArity()", str n = "nodeArity()")
	= generate_copyAnd_dstClass_string("<m> - 1", n)
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && !isOptionEnabled(ts, useHeterogeneousEncoding());

str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndRemoveValue(bool isRare:false), str m = "payloadArity()", str n = "untypedSlotArity()")
	= generate_copyAnd_dstClass_string("<m> - 1", n)
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && isOptionEnabled(ts, useHeterogeneousEncoding());

str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndRemoveValue(bool isRare:true), str m = "payloadArity()", str n = "untypedSlotArity()")
	= generate_copyAnd_dstClass_string(m, "<n> - <use(tupleLengthConstant)>")
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && isOptionEnabled(ts, useHeterogeneousEncoding());






str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndMigrateFromInlineToNode(_), str m = "payloadArity()", str n = "nodeArity()")
	= generate_copyAnd_dstClass_string("<m> - 1", "<n> + 1")
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && !isOptionEnabled(ts, useHeterogeneousEncoding());

str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndMigrateFromInlineToNode(bool isRare:false), str m = "payloadArity()", str n = "untypedSlotArity()")
	= generate_copyAnd_dstClass_string("<m> - 1", "<n> + 1")
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && isOptionEnabled(ts, useHeterogeneousEncoding());

str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndMigrateFromInlineToNode(bool isRare:true), str m = "payloadArity()", str n = "untypedSlotArity()")
	= generate_copyAnd_dstClass_string(m, "<n> - <use(tupleLengthConstant)> + 1")
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && isOptionEnabled(ts, useHeterogeneousEncoding());






str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndMigrateFromNodeToInline(bool isRare), str m = "payloadArity()", str n = "nodeArity()")
	= generate_copyAnd_dstClass_string("<m> + 1", "<n> - 1")
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && !isOptionEnabled(ts, useHeterogeneousEncoding());

str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndMigrateFromNodeToInline(bool isRare:false), str m = "payloadArity()", str n = "untypedSlotArity()")
	= generate_copyAnd_dstClass_string("<m> + 1", "<n> - <use(tupleLengthConstant)>")
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && isOptionEnabled(ts, useHeterogeneousEncoding());

str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndMigrateFromNodeToInline(bool isRare:true), str m = "payloadArity()", str n = "untypedSlotArity()")
	= generate_copyAnd_dstClass_string("<m>", "<n> + <use(tupleLengthConstant)> - 1")
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && isOptionEnabled(ts, useHeterogeneousEncoding());






str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndSetNode(_), str m = "payloadArity()", str n = "nodeArity()")
	= generate_copyAnd_dstClass_string(m, n)
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && !isOptionEnabled(ts, useHeterogeneousEncoding());

str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndSetNode(_), str m = "payloadArity()", str n = "untypedSlotArity()")
	= generate_copyAnd_dstClass_string(m, n)
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && isOptionEnabled(ts, useHeterogeneousEncoding());






str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndSetValue(bool isRare), str m = "payloadArity()", str n = "nodeArity()")
	= generate_copyAnd_dstClass_string(m, n)
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && !isOptionEnabled(ts, useHeterogeneousEncoding());

str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndSetValue(bool isRare), str m = "payloadArity()", str n = "untypedSlotArity()")
	= generate_copyAnd_dstClass_string(m, n)
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData()) && isOptionEnabled(ts, useHeterogeneousEncoding());






str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op, str m = "m", str n = "n")
	= generate_copyAnd_dstClass_string("???", "???")
when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData());

//str generate_copyAnd_dstClass(TrieSpecifics ts, Artifact artifact, PredefOp op:copyAndInsertValue(_), str m = "m", str n = "n")
//	= "<constructorName>_nextClass_array[<mNext>][<nNext>]"
//when isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, unsafeCodeAsData());


//str generate_copyAnd_copyAndUpdateBitmap(TrieSpecifics ts, Artifact artifact, PredefOp op, str bitmapName, Expression(Expression oldBitmapValueExpr) valueUpdater, Expression valueDefault) =
//	"
//	// copy and update bitmaps
//	if (<use(dstOffset)> != sun.misc.Unsafe.INVALID_FIELD_OFFSET) {
//		if (<use(srcOffset)> != sun.misc.Unsafe.INVALID_FIELD_OFFSET) {
//			<dec(bitmap)> = <eval(valueUpdater(oldBitmapValueExpr))>; 
//			unsafe.put<capitalize(chunkSizeToPrimitive(ts.bitPartitionSize).\type)>(dst, <use(dstOffset)>, <use(bitmap)>);
//		} else {
//			<dec(bitmap)> = <eval(valueDefault)>;
//			unsafe.put<capitalize(chunkSizeToPrimitive(ts.bitPartitionSize).\type)>(dst, <use(dstOffset)>, <use(bitmap)>);
//		}
//	}
//	"
//when bitmap := val(chunkSizeToPrimitive(ts.bitPartitionSize), "new<capitalize(bitmapName)>") &&
//		srcOffset := val(primitive("long"), "src<capitalize(bitmapName)>Offset") &&
//		dstOffset := val(primitive("long"), "dst<capitalize(bitmapName)>Offset") &&
//		oldBitmapValueExpr := exprFromString("unsafe.get<capitalize(chunkSizeToPrimitive(ts.bitPartitionSize).\type)>(src, <use(srcOffset)>)");


str generate_copyAnd_copyAndUpdateBitmap(TrieSpecifics ts, Artifact artifact, PredefOp op, str bitmapName, Expression(Expression oldBitmapValueExpr) valueUpdater, Expression valueDefault) =
	"
	// copy and update bitmaps
	<dec(bitmap)> = <eval(valueUpdater(oldBitmapValueExpr))>; 
	unsafe.put<capitalize(chunkSizeToPrimitive(ts.bitPartitionSize).\type)>(dst, <use(offset)>, <use(bitmap)>);
	"
when bitmap := val(chunkSizeToPrimitive(ts.bitPartitionSize), "new<capitalize(bitmapName)>") &&
		offset := val(primitive("long"), "global<capitalize(bitmapName)>Offset") &&
		oldBitmapValueExpr := exprFromString("unsafe.get<capitalize(chunkSizeToPrimitive(ts.bitPartitionSize).\type)>(src, <use(offset)>)");
		
str generate_equalsFunctionUnsafe_compareBitmap(TrieSpecifics ts, Artifact artifact, PredefOp op, str bitmapName) =
	"// compare <bitmapName>
	'if (!(<eq(
			val(chunkSizeToPrimitive(ts.bitPartitionSize), "unsafe.get<capitalize(chunkSizeToPrimitive(ts.bitPartitionSize).\type)>(src, <use(offset)>)"), 
			val(chunkSizeToPrimitive(ts.bitPartitionSize), "unsafe.get<capitalize(chunkSizeToPrimitive(ts.bitPartitionSize).\type)>(dst, <use(offset)>)")		
		)>)) {
	'	return false;
	'}"
when offset := val(primitive("long"), "global<capitalize(bitmapName)>Offset") &&
		eq := equalityComparatorForArguments;


data PredefOp = copyAndInsertValue(bool isRare);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp op:copyAndInsertValue(bool isRare))
	= method(\return(jdtToType(compactNode(ts))), "copyAndInsert<if (isRare) {>Rare<}>Value", args = [ts.mutator, ts.bitposField, *nodeTupleArgs(ts, isRare = isRare)]);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndInsertValue(bool isRare)) 
	= true when isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndInsertValue(bool isRare)) =
	"try {
		<dec(valIdx)> = <bitmapIndex(ctPayloadTuple(isRare = isRare))>(bitpos);
		
		<generate_copyAnd_generalPrelude(ts, artifact, op)>
						
		<if (isRare) {>
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 0), 
			Expression(Expression oldBitmapValueExpr) {
				return cast(chunkSizeToPrimitive(ts.bitPartitionSize),
						bitwiseOr(oldBitmapValueExpr, useExpr(ts.bitposField)));
			},
			cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)))>					
		<} else {>
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 0), 
			Expression(Expression oldBitmapValueExpr) {
				return oldBitmapValueExpr; // idendity
			},
			cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)))>		
		<}>						
						
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 1), 
			Expression(Expression oldBitmapValueExpr) {
				return cast(chunkSizeToPrimitive(ts.bitPartitionSize),
						bitwiseOr(oldBitmapValueExpr, useExpr(ts.bitposField)));
			},
			useExpr(ts.bitposField))>					
		
		<generatePartitionCopy(ts, copyAndInsert(isRare ? "rarePayload" : "payload", useExpr(valIdx), [ useExpr(x) | x <- payloadTupleArgs(ts, isRare = true) ]))>
		
		/*
		// copy payload range (isRare = <!isRare>)				
		<copyPayloadRange(ts, artifact, iconst(0), call(getDef(ts, artifact, payloadArity(isRare = !isRare))), indexIdentity, indexIdentity, isRare = !isRare)>								
						
		// copy payload range (isRare = <isRare>)				
		<copyPayloadRange(ts, artifact, iconst(0), useExpr(valIdx), indexIdentity, indexIdentity, isRare = isRare)>

		<copyPayloadRange(ts, artifact, useExpr(valIdx), plus(useExpr(valIdx), iconst(1)), indexIdentity, indexIdentity, isRare = isRare, argsOverride = (ctKey(isRare = isRare): useExpr(key(ts.keyType)), ctVal(isRare = isRare): useExpr(val(ts.valType))))>

		<copyPayloadRange(ts, artifact, useExpr(valIdx), call(getDef(ts, artifact, payloadArity(isRare = isRare))), indexIdentity, indexAdd1, isRare = isRare)>

		// copy node range
		<copyNodeRange(ts, artifact, iconst(0), call(getDef(ts, artifact, nodeArity())), indexIdentity, indexIdentity)>
		*/

		return dst;
	} catch (InstantiationException e) {
		throw new RuntimeException(e);
	}"
when valIdx := val(primitive("int"), "valIdx") && 
		i := var(primitive("int"), "i");

// TODO: unify copyPayloadRange and copyNodeRange
str copyPayloadRange(TrieSpecifics ts, Artifact artifact, Expression from, Expression to, Expression(Expression) srcIndexShift, Expression(Expression) dstIndexShift, bool isRare = false, map[ContentType, Expression] argsOverride = ()) = 
	"<if (fromPlusOne != to) {>for (int i = <toString(from)>; i \< <toString(to)>; i++) {<}>
		unsafe.<unsafePutMethodNameFromType(ct2type(ts)[ctKey(isRare = isRare)])>(dst, dstArrayOffsets[dst.<toString(callLogicalToPhysical(ts, artifact, ctKey(isRare = isRare), dstIndexShift(i)))>], <if (argsOverride[ctKey(isRare = isRare)]?) {><toString(argsOverride[ctKey(isRare = isRare)])><} else {>unsafe.<unsafeGetMethodNameFromType(ct2type(ts)[ctKey(isRare = isRare)])>(src, srcArrayOffsets[src.<toString(callLogicalToPhysical(ts, artifact, ctKey(isRare = isRare), srcIndexShift(i)))>])<}>);
		unsafe.<unsafePutMethodNameFromType(ct2type(ts)[ctVal(isRare = isRare)])>(dst, dstArrayOffsets[dst.<toString(callLogicalToPhysical(ts, artifact, ctVal(isRare = isRare), dstIndexShift(i)))>], <if (argsOverride[ctVal(isRare = isRare)]?) {><toString(argsOverride[ctVal(isRare = isRare)])><} else {>unsafe.<unsafeGetMethodNameFromType(ct2type(ts)[ctVal(isRare = isRare)])>(src, srcArrayOffsets[src.<toString(callLogicalToPhysical(ts, artifact, ctVal(isRare = isRare), srcIndexShift(i)))>])<}>);
	<if (fromPlusOne != to) {>}<}>"
when fromPlusOne := plus(from, iconst(1)) &&
		i := (fromPlusOne == to ? from : useExpr(var(primitive("int"), "i")));
		
// TODO: unify copyPayloadRange and copyNodeRange
str comparePayloadRange(TrieSpecifics ts, Artifact artifact, Expression from, Expression to, Expression(Expression) srcIndexShift, Expression(Expression) dstIndexShift, map[ContentType, Expression] argsOverride = ()) = 
	"<if (fromPlusOne != to) {>for (int i = <toString(from)>; i \< <toString(to)>; i++) {<}>		
		'if (!(<eq(
				val(ts.keyType, "unsafe.<unsafeGetMethodNameFromType(ts.keyType)>(src, arrayOffsets[src.<toString(callLogicalToPhysical(ts, artifact, ctKey(), srcIndexShift(i)))>])"), 
				val(ts.keyType, "unsafe.<unsafeGetMethodNameFromType(ts.keyType)>(dst, arrayOffsets[dst.<toString(callLogicalToPhysical(ts, artifact, ctKey(), srcIndexShift(i)))>])")		
			)>)) {
		'	return false;
		'}
		
		'if (!(<eq(
				val(ts.valType, "unsafe.<unsafeGetMethodNameFromType(ts.keyType)>(src, arrayOffsets[src.<toString(callLogicalToPhysical(ts, artifact, ctVal(), srcIndexShift(i)))>])"), 
				val(ts.valType, "unsafe.<unsafeGetMethodNameFromType(ts.keyType)>(dst, arrayOffsets[dst.<toString(callLogicalToPhysical(ts, artifact, ctVal(), srcIndexShift(i)))>])")
			)>)) {
		'	return false;
		'}
	<if (fromPlusOne != to) {>}<}>"
when fromPlusOne := plus(from, iconst(1)) &&
		i := (fromPlusOne == to ? from : useExpr(var(primitive("int"), "i"))) &&
		eq := equalityDefaultForArguments;	
		
// TODO: unify copyPayloadRange and copyNodeRange
str copyNodeRange(TrieSpecifics ts, Artifact artifact, Expression from, Expression to, Expression(Expression) srcIndexShift, Expression(Expression) dstIndexShift, map[ContentType, Expression] argsOverride = ()) = 
	"<if (fromPlusOne != to) {>for (int i = <toString(from)>; i \< <toString(to)>; i++) {<}>
		unsafe.putObject(dst, dstArrayOffsets[dst.<toString(callLogicalToPhysical(ts, artifact, ctNode(), dstIndexShift(i)))>], <if (argsOverride[ctNode()]?) {><toString(argsOverride[ctNode()])><} else {>unsafe.getObject(src, srcArrayOffsets[src.<toString(callLogicalToPhysical(ts, artifact, ctNode(), srcIndexShift(i)))>])<}>);
	<if (fromPlusOne != to) {>}<}>"
when fromPlusOne := plus(from, iconst(1)) &&
		i := (fromPlusOne == to ? from : useExpr(var(primitive("int"), "i")));
		
// TODO: unify copyPayloadRange and copyNodeRange
str compareNodeRange(TrieSpecifics ts, Artifact artifact, Expression from, Expression to, Expression(Expression) srcIndexShift, Expression(Expression) dstIndexShift, map[ContentType, Expression] argsOverride = ()) = 
	"<if (fromPlusOne != to) {>for (int i = <toString(from)>; i \< <toString(to)>; i++) {<}>
		'if (!(<eq(
				val(object(), "unsafe.<unsafeGetMethodNameFromType(object())>(src, arrayOffsets[src.<toString(callLogicalToPhysical(ts, artifact, ctNode(), srcIndexShift(i)))>])"), 
				val(object(), "unsafe.<unsafeGetMethodNameFromType(object())>(dst, arrayOffsets[dst.<toString(callLogicalToPhysical(ts, artifact, ctNode(), srcIndexShift(i)))>])")
			)>)) {
		'	return false;
		'}
	<if (fromPlusOne != to) {>}<}>"
when fromPlusOne := plus(from, iconst(1)) &&
		i := (fromPlusOne == to ? from : useExpr(var(primitive("int"), "i"))) &&
		eq := equalityDefaultForArguments;				


data PredefOp = copyAndInsertValue_nextClass();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndInsertValue_nextClass())
	= method(\return(specific("java.lang.Class")), "copyAndInsertValue_nextClass", isActive = isOptionEnabled(ts, useSunMiscUnsafe()) && !isOptionEnabled(ts, unsafeCodeAsData()));


data PredefOp = copyAndRemoveValue(bool isRare);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndRemoveValue(bool isRare))
	= method(\return(jdtToType(compactNode(ts))), "copyAndRemove<if (isRare) {>Rare<}>Value", args = [ts.mutator, ts.bitposField]);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndRemoveValue(bool isRare)) 
	= true when isOptionEnabled(ts, useSunMiscUnsafe());
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndRemoveValue(bool isRare)) =
	"try {		
		<dec(valIdx)> = <bitmapIndex(ctPayloadTuple(isRare = isRare))>(bitpos);
		
		<generate_copyAnd_generalPrelude(ts, artifact, op)>
						
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 0), 
			Expression(Expression oldBitmapValueExpr) {
				return oldBitmapValueExpr; // idendity
			},
			cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)))>					
						
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 1), 
			Expression(Expression oldBitmapValueExpr) {
				return cast(chunkSizeToPrimitive(ts.bitPartitionSize),
						bitwiseXor(oldBitmapValueExpr, useExpr(ts.bitposField)));
			},
			cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)))>					
				
		// copy payload range				
		<copyPayloadRange(ts, artifact, iconst(0), useExpr(valIdx), indexIdentity, indexIdentity)>
		<copyPayloadRange(ts, artifact, plus(useExpr(valIdx), iconst(1)), call(getDef(ts, artifact, payloadArity(isRare = false))), indexIdentity, indexSubtract1)>
		
		// copy node range				
		<copyNodeRange(ts, artifact, iconst(0), call(getDef(ts, artifact, nodeArity())), indexIdentity, indexIdentity)>
		
		return dst;	
	} catch (InstantiationException e) {
		throw new RuntimeException(e);
	}"
when valIdx := val(primitive("int"), "valIdx");

data PredefOp = copyAndRemoveValue_nextClass();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndRemoveValue_nextClass())
	= method(\return(specific("java.lang.Class")), "copyAndRemoveValue_nextClass", isActive = isOptionEnabled(ts, useSunMiscUnsafe()) && !isOptionEnabled(ts, unsafeCodeAsData()));


data PredefOp = copyAndSetValue(bool isRare);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:copyAndSetValue(bool isRare))
	= method(\return(jdtToType(compactNode(ts))), "copyAndSet<if (isRare) {>Rare<}>Value", lazyArgs = list[Argument]() { return [ts.mutator, ts.bitposField, payloadTupleArg(ts, 1, isRare = isRare) ]; }, isActive = \map() := ts.ds);


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndSetValue(bool isRare)) 
	= true when isOptionEnabled(ts, useSunMiscUnsafe());
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndSetValue(bool isRare)) {

	Argument valIdx = val(primitive("int"), "valIdx");

	copyPayloadRange_clojure = str(bool isRare) {
		if (isRare) {
			return 
				"
				// copy payload range (isRare = <!isRare>)				
				<copyPayloadRange(ts, artifact, iconst(0), call(getDef(ts, artifact, payloadArity())), indexIdentity, indexIdentity, isRare = !isRare)>								
				";
		} else {
			return 
				"
				// copy payload range (isRare = <!isRare>)				
				<copyPayloadRange(ts, artifact, iconst(0), call(getDef(ts, artifact, payloadArity())), indexIdentity, indexIdentity, isRare = !isRare)>								
								
				// copy payload range (isRare = <isRare>)				
				<copyPayloadRange(ts, artifact, iconst(0), useExpr(valIdx), indexIdentity, indexIdentity, isRare = isRare)>								
		
				<copyPayloadRange(ts, artifact, useExpr(valIdx), plus(useExpr(valIdx), iconst(1)), indexIdentity, indexIdentity, isRare = isRare, argsOverride = (ctVal(): useExpr(val(ts.valType))))>
						
				<copyPayloadRange(ts, artifact, plus(useExpr(valIdx), iconst(1)), call(getDef(ts, artifact, payloadArity())), indexIdentity, indexIdentity, isRare = isRare)>
				";
		}
	}; 

//		<copyPayloadRange_clojure(isRare)>
//		<copyPayloadRange_clojure(!isRare)>				
//				
//		// copy payload range				
//		<copyPayloadRange(ts, artifact, iconst(0), useExpr(valIdx), indexIdentity, indexIdentity)>								
//
//		<copyPayloadRange(ts, artifact, useExpr(valIdx), plus(useExpr(valIdx), iconst(1)), indexIdentity, indexIdentity, argsOverride = (ctVal(): useExpr(val(ts.valType))))>
//				
//		<copyPayloadRange(ts, artifact, plus(useExpr(valIdx), iconst(1)), call(getDef(ts, artifact, payloadArity())), indexIdentity, indexIdentity)>

	return "try {
		<dec(valIdx)> = dataIndex(bitpos);

		<generate_copyAnd_generalPrelude(ts, artifact, op)>
						
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 0), 
			Expression(Expression oldBitmapValueExpr) {
				return oldBitmapValueExpr; // idendity
			},
			cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)))>					
						
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 1), 
			Expression(Expression oldBitmapValueExpr) {
				return oldBitmapValueExpr; // idendity
			},
			cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)))>		
				
		// copy payload range (isRare = <!isRare>)				
		<copyPayloadRange(ts, artifact, iconst(0), call(getDef(ts, artifact, payloadArity(isRare = !isRare))), indexIdentity, indexIdentity, isRare = !isRare)>								
						
		// copy payload range (isRare = <isRare>)				
		<copyPayloadRange(ts, artifact, iconst(0), useExpr(valIdx), indexIdentity, indexIdentity, isRare = isRare)>								

		<copyPayloadRange(ts, artifact, useExpr(valIdx), plus(useExpr(valIdx), iconst(1)), indexIdentity, indexIdentity, isRare = isRare, argsOverride = (ctVal(): useExpr(val(ts.valType))))>
				
		<copyPayloadRange(ts, artifact, plus(useExpr(valIdx), iconst(1)), call(getDef(ts, artifact, payloadArity(isRare = isRare))), indexIdentity, indexIdentity, isRare = isRare)>					

		// copy node range
		<copyNodeRange(ts, artifact, iconst(0), call(getDef(ts, artifact, nodeArity())), indexIdentity, indexIdentity)>

		return dst;
	} catch (InstantiationException e) {
		throw new RuntimeException(e);
	}";
}


data PredefOp = copyAndSetValue_nextClass();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndSetValue_nextClass())
	= method(\return(specific("java.lang.Class")), "copyAndSetValue_nextClass", isActive = isOptionEnabled(ts, useSunMiscUnsafe()) && !isOptionEnabled(ts, unsafeCodeAsData()));


data PredefOp = copyAndSetNode(bool isRare);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndSetNode(_))
	= method(\return(jdtToType(compactNode(ts))), "copyAndSetNode", args = [ts.mutator, ts.bitposField, \inode(ts.ds, ts.tupleTypes)]);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndSetNode(_)) 
	= true when isOptionEnabled(ts, useSunMiscUnsafe());
	
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndSetNode(bool isRare)) =
	"try {
		<dec(idx)> = nodeIndex(bitpos);
		
		<generate_copyAnd_generalPrelude(ts, artifact, op)>
						
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 0), 
			Expression(Expression oldBitmapValueExpr) {
				return oldBitmapValueExpr; // idendity
			},
			cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)))>					
						
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 1), 
			Expression(Expression oldBitmapValueExpr) {
				return oldBitmapValueExpr; // idendity
			},
			cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)))>
		
		// TODO: create node differently from ctNode()
		<generatePartitionCopy(ts, copyAndSet("node", useExpr(idx), [ useExpr(\inode(ts.ds, ts.tupleTypes)) ]))>
		
		/*		
		// copy payload range (isRare = <!isRare>)				
		<copyPayloadRange(ts, artifact, iconst(0), call(getDef(ts, artifact, payloadArity(isRare = !isRare))), indexIdentity, indexIdentity, isRare = !isRare)>	
			
		// copy payload range (isRare = <isRare>)				
		<copyPayloadRange(ts, artifact, iconst(0), call(getDef(ts, artifact, payloadArity(isRare = isRare))), indexIdentity, indexIdentity, isRare = isRare)>

		// copy node range
		<copyNodeRange(ts, artifact, iconst(0), useExpr(idx), indexIdentity, indexIdentity)>
		
		<copyNodeRange(ts, artifact, useExpr(idx), plus(useExpr(idx), iconst(1)), indexIdentity, indexIdentity, argsOverride = (ctNode(): useExpr(\inode(ts.ds, ts.tupleTypes))))>	

		<copyNodeRange(ts, artifact, plus(useExpr(idx), iconst(1)), call(getDef(ts, artifact, nodeArity())), indexIdentity, indexIdentity)>
		*/
								
		return dst;
	} catch (InstantiationException e) {
		throw new RuntimeException(e);
	}"
when idx := val(primitive("int"), "idx");


data PredefOp = copyAndSetNode_nextClass();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndSetNode_nextClass())
	= method(\return(specific("java.lang.Class")), "copyAndSetNode_nextClass", isActive = isOptionEnabled(ts, useSunMiscUnsafe()) && !isOptionEnabled(ts, unsafeCodeAsData()));


data PredefOp = copyAndInsertNode(bool customComparator = false);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndInsertNode())
	= method(\return(jdtToType(compactNode(ts))), "copyAndInsertNode", args = [ts.mutator, ts.bitposField, \inode(ts.ds, ts.tupleTypes)], isActive = false);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndInsertNode()) 
	= true when isOptionEnabled(ts, useSunMiscUnsafe());
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndInsertNode()) =
	"
	";


data PredefOp = copyAndInsertNode_nextClass();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndInsertNode_nextClass())
	= method(\return(specific("java.lang.Class")), "copyAndInsertNode_nextClass", isActive = false);


data PredefOp = copyAndMigrateFromInlineToNode(bool isRare);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndMigrateFromInlineToNode(bool isRare))
	= method(\return(jdtToType(compactNode(ts))), "copyAndMigrateFrom<if (isRare) {>Rare<}>InlineToNode", args = [ts.mutator, ts.bitposField, \inode(ts.ds, ts.tupleTypes)]);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndMigrateFromInlineToNode(bool isRare)) 
	= true when isOptionEnabled(ts, useSunMiscUnsafe());
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndMigrateFromInlineToNode(bool isRare)) =
	"try {
		<dec(idxOld)> = <bitmapIndex(ctPayloadTuple(isRare = isRare))>(bitpos);
		<dec(idxNew)> = nodeIndex(bitpos);
		
		<generate_copyAnd_generalPrelude(ts, artifact, op)>
		
		// idempotent operation; in case of rare bit was already set before				
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 0), 
			Expression(Expression oldBitmapValueExpr) {
				return cast(chunkSizeToPrimitive(ts.bitPartitionSize),
						bitwiseOr(oldBitmapValueExpr, useExpr(ts.bitposField)));
			},
			useExpr(ts.bitposField))>							
						
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 1), 
			Expression(Expression oldBitmapValueExpr) {
				return cast(chunkSizeToPrimitive(ts.bitPartitionSize),
						bitwiseXor(oldBitmapValueExpr, useExpr(ts.bitposField)));
			},
			cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)))>

		// TODO: create node differently from ctNode()
		<generatePartitionCopy(ts, copyAndMigrateFromInlineToNode(isRare ? "rarePayload" : "payload", useExpr(idxOld), "node", useExpr(idxNew), [ useExpr(\inode(ts.ds, ts.tupleTypes)) ]))>

		/*
		// copy payload range (isRare = <!isRare>)				
		<copyPayloadRange(ts, artifact, iconst(0), call(getDef(ts, artifact, payloadArity(isRare = !isRare))), indexIdentity, indexIdentity, isRare = !isRare)>								

		// copy payload range (isRare = <isRare>)				
		<copyPayloadRange(ts, artifact, iconst(0), useExpr(idxOld), indexIdentity, indexIdentity, isRare = isRare)>
		
		<copyPayloadRange(ts, artifact, plus(useExpr(idxOld), iconst(1)), call(getDef(ts, artifact, payloadArity(isRare = isRare))), indexIdentity, indexSubtract1, isRare = isRare)>

		// copy node range
		<copyNodeRange(ts, artifact, iconst(0), useExpr(idxNew), indexIdentity, indexIdentity)>		
		
		<copyNodeRange(ts, artifact, 
			useExpr(idxNew), 
			plus(useExpr(idxNew), iconst(1)), 
			indexIdentity, 
			indexIdentity, 
			argsOverride = (
				ctNode(): useExpr(\inode(ts.ds, ts.tupleTypes))))>
		
		<copyNodeRange(ts, artifact, useExpr(idxNew), call(getDef(ts, artifact, nodeArity())), indexIdentity, indexAdd1)>
		*/

		return dst;
	} catch (InstantiationException e) {
		throw new RuntimeException(e);
	}"
when idxOld := val(primitive("int"), "idxOld") &&
		idxNew := val(primitive("int"), "idxNew");


data PredefOp = copyAndMigrateFromInlineToNode_nextClass();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndMigrateFromInlineToNode_nextClass())
	= method(\return(specific("java.lang.Class")), "copyAndMigrateFromInlineToNode_nextClass", isActive = isOptionEnabled(ts, useSunMiscUnsafe()) && !isOptionEnabled(ts, unsafeCodeAsData()));


data PredefOp = copyAndMigrateFromNodeToInline(bool isRare);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp op:copyAndMigrateFromNodeToInline(bool isRare))
	= method(\return(jdtToType(compactNode(ts))), "copyAndMigrateFromNodeTo<if (isRare) {>Rare<}>Inline", args = [ts.mutator, ts.bitposField, \inode(ts.ds, ts.tupleTypes)]);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndMigrateFromNodeToInline(bool isRare)) 
	= true when isOptionEnabled(ts, useSunMiscUnsafe());
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndMigrateFromNodeToInline(bool isRare)) =
	"try {
		<dec(idxOld)> = nodeIndex(bitpos);
		<dec(idxNew)> = <bitmapIndex(ctPayloadTuple(isRare = isRare))>(bitpos);	
		
		<generate_copyAnd_generalPrelude(ts, artifact, op)>
				
		// idempotent operation; in case of rare bit was already set before						
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 0), 
			Expression(Expression oldBitmapValueExpr) {
				return cast(chunkSizeToPrimitive(ts.bitPartitionSize),
						isRare ?
							bitwiseOr(oldBitmapValueExpr, useExpr(ts.bitposField)) :
							bitwiseXor(oldBitmapValueExpr, useExpr(ts.bitposField)));
			},
			cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)))>										
						
		<generate_copyAnd_copyAndUpdateBitmap(ts, artifact, op, lowLevelBitmapName(ts, 1), 
			Expression(Expression oldBitmapValueExpr) {
				return cast(chunkSizeToPrimitive(ts.bitPartitionSize),
						bitwiseOr(oldBitmapValueExpr, useExpr(ts.bitposField)));
			},
			useExpr(ts.bitposField))>									
		
		// copy payload range (isRare = <!isRare>)				
		<copyPayloadRange(ts, artifact, iconst(0), call(getDef(ts, artifact, payloadArity(isRare = !isRare))), indexIdentity, indexIdentity, isRare = !isRare)>										
		
		// copy payload range
		<copyPayloadRange(ts, artifact, iconst(0), useExpr(idxNew), indexIdentity, indexIdentity, isRare = isRare)>
			
		<copyPayloadRange(ts, artifact, 
			useExpr(idxNew), 
			plus(useExpr(idxNew), iconst(1)), 
			indexIdentity, 
			indexIdentity, 
			argsOverride = (
				ctPayloadArg(0, isRare = isRare): exprFromString("node.<contentAccessor(ctPayloadArg(0, isRare = isRare), "0")>"), 
				ctPayloadArg(1, isRare = isRare): exprFromString("node.<contentAccessor(ctPayloadArg(1, isRare = isRare), "0")>")),
			isRare = isRare)>
				
		<copyPayloadRange(ts, artifact, useExpr(idxNew), call(getDef(ts, artifact, payloadArity(isRare = isRare))), indexIdentity, indexAdd1, isRare = isRare)>		
				
		// copy node range
		<copyNodeRange(ts, artifact, iconst(0), useExpr(idxOld), indexIdentity, indexIdentity)>
		<copyNodeRange(ts, artifact, plus(useExpr(idxOld), iconst(1)), call(getDef(ts, artifact, nodeArity())), indexIdentity, indexSubtract1)>
				
		return dst;
	} catch (InstantiationException e) {
		throw new RuntimeException(e);
	}"
when idxOld := val(primitive("int"), "idxOld") &&
		idxNew := val(primitive("int"), "idxNew");


data PredefOp = copyAndMigrateFromNodeToInline_nextClass();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndMigrateFromNodeToInline_nextClass())
	= method(\return(specific("java.lang.Class")), "copyAndMigrateFromNodeToInline_nextClass", isActive = isOptionEnabled(ts, useSunMiscUnsafe()) && !isOptionEnabled(ts, unsafeCodeAsData()));

		
data PredefOp = copyAndRemoveNode(bool customComparator = false);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndRemoveNode())
	= method(\return(jdtToType(compactNode(ts))), "copyAndInsertNode", args = [ts.mutator, ts.bitposField], isActive = false);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndRemoveNode()) 
	= true when isOptionEnabled(ts, useSunMiscUnsafe());
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:copyAndRemoveNode()) =
	"
	";


data PredefOp = copyAndRemoveNode_nextClass();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndRemoveNode_nextClass())
	= method(\return(specific("java.lang.Class")), "copyAndRemoveNode_nextClass", isActive = false);


// TODO: specialize removed(..) to remove this method from this interface		
data PredefOp = removeInplaceValueAndConvertToSpecializedNode(bool customComparator = false);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), removeInplaceValueAndConvertToSpecializedNode())
	= method(\return(jdtToType(compactNode(ts))), "removeInplaceValueAndConvertToSpecializedNode", args = [ts.mutator, ts.bitposField], isActive = supportsConversionBetweenGenericAndSpecialized(ts));


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), removeInplaceValueAndConvertToSpecializedNode()) = true;
Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, removeInplaceValueAndConvertToSpecializedNode()) 
	= UNSUPPORTED_OPERATION_EXCEPTION;


lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, TrieNodeType nodeType:compactNode()) 
	= [ <nodeType,method> | method <- declaredMethodsByCompactNode(ts)]
	+ [ <nodeType,method> | method <- createNodeFactorySpecializationList(ts, nodeType)];

list[PredefOp] declaredMethodsByCompactNode(TrieSpecifics ts) = [

	arrayBase(),
	addressSize(),	

	specializationsByContentAndNodes(),

	globalFieldOffset(lowLevelBitmapName(ts, 0)),
	globalFieldOffset(lowLevelBitmapName(ts, 1)),
	//globalFieldOffset(lowLevelBitmapName(ts, 0)),
	//globalFieldOffset(lowLevelBitmapName(ts, 1)),
	globalFieldOffset("arrayOffsets"),	
	globalFieldOffset("nodeArity"),
	globalFieldOffset("payloadArity"),
	globalFieldOffset("slotArity"),
	globalFieldOffset("untypedSlotArity"),
	
	globalFieldOffset("arrayOffsetLast"),
	
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
	rareMap(),
	
	rawMap1(),
	rawMap2(),
	
	isRare1(),
	isRare2(),
	isRareBitpos(),
	
	// OLD: getKeyFunction(),
	getContentFunction(ctPayloadArg(0, isRare = false)),
	getContentFunction(ctPayloadArg(0, isRare = true)),
	getNodeFunction(),
	
	contentTypeEnum(),
	logicalToPhysicalIndex(),	
	
	nodeInvariant(),
	isTrieStructureValid(),
	
	arrayOffsetsFunction(),
	fieldOffsetFunction(),
	
	copyAndInsertNode(), // ???
	copyAndInsertNode_nextClass(), // ???
	
	copyAndRemoveNode(), // ???
	copyAndRemoveNode_nextClass(), // ???
	
	copyAndSetValue(false),
	copyAndSetValue(true),
	copyAndSetValue_nextClass(),
	
	copyAndInsertValue(false),
	copyAndInsertValue(true),
	copyAndInsertValue_nextClass(),
	
	copyAndRemoveValue(false),
	copyAndRemoveValue(true),
	copyAndRemoveValue_nextClass(),
	
	copyAndSetNode(false),
	//copyAndSetNode(true), // does the same as above
	copyAndSetNode_nextClass(),
	
	copyAndMigrateFromInlineToNode(false),
	copyAndMigrateFromInlineToNode(true),
	copyAndMigrateFromInlineToNode_nextClass(),
	
	copyAndMigrateFromNodeToInline(false),
	copyAndMigrateFromNodeToInline(true),
	copyAndMigrateFromNodeToInline_nextClass(),
	
	removeInplaceValueAndConvertToSpecializedNode(),	

	mergeTwoKeyValPairs(ctPayloadTuple(isRare = true), ctPayloadTuple(isRare = true)),
	mergeTwoKeyValPairs(ctPayloadTuple(isRare = true), ctPayloadTuple(isRare = false)),
	mergeTwoKeyValPairs(ctPayloadTuple(isRare = false), ctPayloadTuple(isRare = true)),
	mergeTwoKeyValPairs(ctPayloadTuple(isRare = false), ctPayloadTuple(isRare = false)),
	
	mergeNodeAndKeyValPair(),

	index2(),
	index3(),
	
	dataIndex(),
	nodeIndex(),
	rareIndex(),
	
	nodeAt(), // TODO: get rid of?
		
//	containsKey(),
//	containsKey(customComparator = true),
//
//	get(),
//	get(customComparator = true),

	//insertTuple(false, false),
	//insertTuple(false, true),

//	removeTuple(),
//	removeTuple(customComparator = true),	
		
	equalsFunctionUnsafe(),
		
	recoverMask(),
	toString()
	
];

lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, TrieNodeType nodeType:compactNode(BitmapSpecialization _)) 
	= [ <nodeType,constructor()>, <nodeType,nodeMap()>, <nodeType,dataMap()>, <nodeType,rawMap1()>, <nodeType,rawMap2()>];
	
lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, TrieNodeType nodeType:compactHeterogeneousNode(BitmapSpecialization _)) 
	= [ <nodeType,constructor()>, <nodeType,nodeMap()>, <nodeType,dataMap()>, <nodeType,rawMap1()>, <nodeType,rawMap2()>];	

str emptyTrieNodeConstantName = "EMPTY_NODE";
//str generateCompactNodeClassString(TrieSpecifics ts, bool isLegacy:true) {
//	
//	//TrieSpecifics ts = setArtifact(tsSuper, trieNode(compactNode()));
//	
//	abstractMembers = [ bitmapMethod, valmapMethod ];
//	concreteMembers = [];
//	
//	members = abstractMembers + concreteMembers;	
//	
//	constructorArgs = asFieldList(
//		  ts.mutator 
//		+ members);
//
//	str classNameStr = "<CompactNode(ts.ds)>"; 
//
//	int n = 0; // TODO: remove
//	int m = 0; // TODO: remove
//
//	return
//	"protected static abstract class <classNameStr><GenericsStr(ts.tupleTypes)> extends Abstract<toString(ts.ds)>Node<GenericsStr(ts.tupleTypes)> {
//		
//		<impl(ts, trieNode(compactNode()), hashCodeLength())>
//		<impl(ts, trieNode(compactNode()), bitPartitionSize())>
//		<impl(ts, trieNode(compactNode()), bitPartitionMask())>
//		
//		<impl(ts, trieNode(compactNode()), mask())>
//		<impl(ts, trieNode(compactNode()), bitpos())>		
//		
//		<dec(getDef(ts, trieNode(compactNode()), nodeMap()), asAbstract = true)>
//		<dec(getDef(ts, trieNode(compactNode()), dataMap()), asAbstract = true)>
//
//		<if (isOptionEnabled(ts, useHeterogeneousEncoding())) {>
//			<dec(getDef(ts, trieNode(compactNode()), rareMap()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), rawMap1()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), rawMap2()), asAbstract = true)>
//			
//			<impl(ts, trieNode(compactNode()), isRare1())>
//			<impl(ts, trieNode(compactNode()), isRare2())>
//			<impl(ts, trieNode(compactNode()), isRareBitpos())>	
//		<}>
//		
//		<dec(getDef(ts, trieNode(compactNode()), contentTypeEnum()))>
//		
//		<impl(ts, trieNode(compactNode()), logicalToPhysicalIndex())>
//		
//		<impl(ts, trieNode(compactNode()), nodeInvariant())>
//
//		<impl(ts, trieNode(compactNode()), copyAndInsertNode())>
//		<impl(ts, trieNode(compactNode()), copyAndInsertNode_nextClass())>
//		<impl(ts, trieNode(compactNode()), copyAndRemoveNode())>
//		<impl(ts, trieNode(compactNode()), copyAndRemoveNode_nextClass())>
//
//		<if (isOptionEnabled(ts, useSunMiscUnsafe())) {>			
//			<impl(ts, trieNode(compactNode()), arrayOffsetsFunction())>
//			<impl(ts, trieNode(compactNode()), fieldOffsetFunction())>			
//
//			<impl(ts, trieNode(compactNode()), getSlot())>
//			<impl(ts, trieNode(compactNode()), getContent(ctPayloadArg(0)))>
//			<impl(ts, trieNode(compactNode()), getContent(ctPayloadArg(1)))>
//			<impl(ts, trieNode(compactNode()), getContent(ctPayloadArg(0, isRare = true)))>
//			<impl(ts, trieNode(compactNode()), getContent(ctPayloadArg(1, isRare = true)))>
//			<impl(ts, trieNode(compactNode()), getKeyValueEntry())>
//			<impl(ts, trieNode(compactNode()), getContent(ctNode()))>
//						
//			<impl(ts, trieNode(compactNode()), copyAndInsertNode())>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndInsertNode_nextClass()), asAbstract = true)>
//			<impl(ts, trieNode(compactNode()), copyAndRemoveNode())>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndRemoveNode_nextClass()), asAbstract = true)>
//			<impl(ts, trieNode(compactNode()), copyAndSetValue(false))>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndSetValue_nextClass()), asAbstract = true)>
//			<impl(ts, trieNode(compactNode()), copyAndInsertValue())>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndInsertValue_nextClass()), asAbstract = true)>
//			<impl(ts, trieNode(compactNode()), copyAndRemoveValue())>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndRemoveValue_nextClass()), asAbstract = true)>
//			<impl(ts, trieNode(compactNode()), copyAndSetNode(false))>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndSetNode_nextClass()), asAbstract = true)>
//			<impl(ts, trieNode(compactNode()), copyAndMigrateFromInlineToNode())>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndMigrateFromInlineToNode_nextClass()), asAbstract = true)>
//			<impl(ts, trieNode(compactNode()), copyAndMigrateFromNodeToInline())>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndMigrateFromNodeToInline_nextClass()), asAbstract = true)>		
//		< } else {>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndInsertNode()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndInsertNode_nextClass()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndRemoveNode()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndRemoveNode_nextClass()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndSetValue(false)), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndSetValue_nextClass()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndInsertValue()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndInsertValue_nextClass()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndRemoveValue()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndRemoveValue_nextClass()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndSetNode(false)), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndSetNode_nextClass()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndMigrateFromInlineToNode()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndMigrateFromInlineToNode_nextClass()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndMigrateFromNodeToInline()), asAbstract = true)>
//			<dec(getDef(ts, trieNode(compactNode()), copyAndMigrateFromNodeToInline_nextClass()), asAbstract = true)>			
//		<}>
//		
//		</* TODO: specialize removed(..) to remove this method from this interface */"">
//		<impl(ts, trieNode(compactNode()), removeInplaceValueAndConvertToSpecializedNode())>
//
//		<impl(ts, trieNode(compactNode()), mergeTwoKeyValPairs())>
//		<impl(ts, trieNode(compactNode()), mergeNodeAndKeyValPair())>
//	
//	<implOrOverride(ts.nodeOf_BitmapIndexedNode,
//		"return <toString(call(ts.BitmapIndexedNode_constructor, inferredGenericsStr = InferredGenerics(ts.ds, ts.tupleTypes)))>;")>
//
//	<generate_specializationFactoryMethods(ts)>
//
//	<for (op <- createNodeFactorySpecializationList(ts, compactNode())) {>
//		<impl(ts, trieNode(compactNode()), op)>
//	<}>
//
//	<impl(ts, trieNode(compactNode()), index2())>
//	<impl(ts, trieNode(compactNode()), index3())>
//
//	<impl(ts, trieNode(compactNode()), dataIndex())>
//	<impl(ts, trieNode(compactNode()), nodeIndex())>
//	<impl(ts, trieNode(compactNode()), rareIndex())>
//	
//	<impl(ts, trieNode(compactNode()), nodeAt())>
//		
//	<impl(ts, trieNode(compactNode()), containsKey())>
//	<impl(ts, trieNode(compactNode()), containsKey(customComparator = true))>
//
//	<impl(ts, trieNode(compactNode()), get())>
//	<impl(ts, trieNode(compactNode()), get(customComparator = true))>
//	
//	<impl(ts, trieNode(compactNode()), insertTuple(false, false))>
//	<impl(ts, trieNode(compactNode()), insertTuple(false, true))>
//
//	/* EXPERIMENTAL <impl(ts, trieNode(compactNode()), insertTuple(true, false))> */
//	/* EXPERIMENTAL <impl(ts, trieNode(compactNode()), insertTuple(true, true))> */
//
//	<impl(ts, trieNode(compactNode()), removeTuple())>
//	<impl(ts, trieNode(compactNode()), removeTuple(customComparator = true))>
//
//	<impl(ts, trieNode(compactNode()), recoverMask())>
//	<impl(ts, trieNode(compactNode()), toString())>
//
//	<impl(ts, trieNode(compactNode()), isTrieStructureValid())>
//		
//	'}
//	
//	protected static abstract class <className(ts, compactNode(specializeByBitmap(true, true)))><GenericsStr(ts.tupleTypes)> extends <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> {
//
//		private <dec(ts.bitmapField)>;
//		private <dec(ts.valmapField)>;
//
//		<className(ts, compactNode(specializeByBitmap(true, true)))>(final AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>) {
//			this.<bitmapField.name> = <bitmapField.name>;
//			this.<valmapField.name> = <valmapField.name>;
//		}
//
//		@Override
//		public <typeToString(ts.bitmapField.\type)> <bitmapField.name>() {
//			return <bitmapField.name>;
//		}
//
//		@Override
//		public <typeToString(ts.valmapField.\type)> <valmapField.name>() {
//			return <valmapField.name>;
//		}
//
//	}
//
//	<if (isOptionEnabled(ts,useSpecialization())) {>
//	protected static abstract class <className(ts, compactNode(specializeByBitmap(true, false)))><GenericsStr(ts.tupleTypes)> extends <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> {
//
//		private <dec(ts.bitmapField)>;
//
//		<className(ts, compactNode(specializeByBitmap(true, false)))>(final AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>) {
//			this.<bitmapField.name> = <bitmapField.name>;
//		}
//
//		@Override
//		public <typeToString(ts.bitmapField.\type)> <bitmapField.name>() {
//			return <bitmapField.name>;
//		}
//
//		@Override
//		public <typeToString(ts.valmapField.\type)> <valmapField.name>() {
//			return 0;
//		}
//
//	}
//
//	protected static abstract class <className(ts, compactNode(specializeByBitmap(false, true)))><GenericsStr(ts.tupleTypes)> extends <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> {
//
//		private <dec(ts.valmapField)>;
//
//		<className(ts, compactNode(specializeByBitmap(false, true)))>(final AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>) {
//			this.<valmapField.name> = <valmapField.name>;
//		}
//
//		@Override
//		public <typeToString(ts.bitmapField.\type)> <bitmapField.name>() {
//			return 0;
//		}
//
//		@Override
//		public <typeToString(ts.valmapField.\type)> <valmapField.name>() {
//			return <valmapField.name>;
//		}
//
//	}
//	
//	protected static abstract class <className(ts, compactNode(specializeByBitmap(false, false)))><GenericsStr(ts.tupleTypes)> extends <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> {
//
//		<className(ts, compactNode(specializeByBitmap(false, false)))>(final AtomicReference\<Thread\> mutator, <dec(ts.bitmapField)>, <dec(ts.valmapField)>) {
//		}
//
//		@Override
//		public <typeToString(ts.bitmapField.\type)> <bitmapField.name>() {
//			return 0;
//		}
//
//		@Override
//		public <typeToString(ts.valmapField.\type)> <valmapField.name>() {
//			return 0;
//		}
//
//	}
//	<}>
//	"
//	;
//	
//}

@index=2 str generate_bodyOf_mergeTwoValues(TrieSpecifics ts, Artifact artifact, PredefOp op:mergeTwoKeyValPairs(ContentType ct0, ContentType ct1), Position pos:positionBitmap()) =
	"<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))> | <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>);
	'<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))> | <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>);
	' 
	'if (mask0 \< mask1) {
	'	return nodeOf<4>x<0>(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) <use(bitmapField)>, <use(valmapField)>, <use(appendToName(nodeTupleArgs(ts), "0") + appendToName(nodeTupleArgs(ts), "1"))>);
	'} else {
	'	return nodeOf<4>x<0>(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) <use(bitmapField)>, <use(valmapField)>, <use(appendToName(nodeTupleArgs(ts), "1") + appendToName(nodeTupleArgs(ts), "0"))>);
	'}"
when isOptionEnabled(ts, useSpecialization())
		&& ct0.isRare && ct1.isRare;
		
@index=2 str generate_bodyOf_mergeTwoValues(TrieSpecifics ts, Artifact artifact, PredefOp op:mergeTwoKeyValPairs(ContentType ct0, ContentType ct1), Position pos:positionBitmap()) =
	"<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))>);
	'<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))> | <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>);
	' 
	'// convention: rare after base
	'return nodeOf<2>x<1>(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) <use(bitmapField)>, <use(valmapField)>, <use(appendToName(nodeTupleArgs(ts), "1") + appendToName(nodeTupleArgs(ts), "0"))>);"
when isOptionEnabled(ts, useSpecialization())
		&& ct0.isRare && !ct1.isRare;

@index=2 str generate_bodyOf_mergeTwoValues(TrieSpecifics ts, Artifact artifact, PredefOp op:mergeTwoKeyValPairs(ContentType ct0, ContentType ct1), Position pos:positionBitmap()) =
	"<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>);
	'<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))> | <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>);
	' 
	'// convention: rare after base
	'return nodeOf<2>x<1>(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) <use(bitmapField)>, <use(valmapField)>, <use(appendToName(nodeTupleArgs(ts), "0") + appendToName(nodeTupleArgs(ts), "1"))>);"
when isOptionEnabled(ts, useSpecialization())
		&& !ct0.isRare && ct1.isRare;

@index=2 str generate_bodyOf_mergeTwoValues(TrieSpecifics ts, Artifact artifact, PredefOp op:mergeTwoKeyValPairs(ContentType ct0, ContentType ct1), Position pos:positionBitmap()) =
	"<dec(ts.bitmapField)> = 0;
	'<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))> | <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>);
	' 
	'if (mask0 \< mask1) {
	'	return nodeOf<0>x<2>(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) <use(bitmapField)>, <use(valmapField)>, <use(appendToName(nodeTupleArgs(ts), "0") + appendToName(nodeTupleArgs(ts), "1"))>);
	'} else {
	'	return nodeOf<0>x<2>(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) <use(bitmapField)>, <use(valmapField)>, <use(appendToName(nodeTupleArgs(ts), "1") + appendToName(nodeTupleArgs(ts), "0"))>);
	'}"
when isOptionEnabled(ts, useSpecialization())
		&& !ct0.isRare && !ct1.isRare;
		

@index=2 str generate_bodyOf_mergeTwoValues(TrieSpecifics ts, Artifact artifact, PredefOp op, Position pos:positionBitmap()) =
	"<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (<toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))> | <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>);
	' 
	'if (mask0 \< mask1) {
	'	return nodeOf<0>x<2>(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0, <use(valmapField)>, <use(appendToName(nodeTupleArgs(ts), "0") + appendToName(nodeTupleArgs(ts), "1"))>);
	'} else {
	'	return nodeOf<0>x<2>(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0, <use(valmapField)>, <use(appendToName(nodeTupleArgs(ts), "1") + appendToName(nodeTupleArgs(ts), "0"))>);
	'}"
when isOptionEnabled(ts, useSpecialization());

@index=2 str generate_bodyOf_mergeTwoValues(TrieSpecifics ts, Artifact artifact, PredefOp op, Position pos:positionField()) =
	"if (mask0 \< mask1) {
	'	return nodeOf<0>x<2>(null, (byte) mask0, <use(__payloadTuple(ts.ds, ts.tupleTypes, 0))>, (byte) mask1, <use(__payloadTuple(ts.ds, ts.tupleTypes, 1))>);
	'} else {
	'	return nodeOf<0>x<2>(null, (byte) mask1, <use(__payloadTuple(ts.ds, ts.tupleTypes, 1))>, (byte) mask0, <use(__payloadTuple(ts.ds, ts.tupleTypes, 0))>);
	'}"
when isOptionEnabled(ts, useSpecialization());

/*
 *	Both <call> invocatiosn in the body have similar data; only content array differs.
 */	
@index=2 str generate_bodyOf_mergeTwoValues(TrieSpecifics ts, Artifact artifact, PredefOp op, Position _) =
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
	'}"
when !isOptionEnabled(ts, useSpecialization());	

@index=2 str generate_bodyOf_mergeOnNextLevel(TrieSpecifics ts, Position pos:positionField()) =
	"return nodeOf<1>x<0>(null, (byte) mask0, node);"
when isOptionEnabled(ts, useSpecialization());

@index=2 str generate_bodyOf_mergeOnNextLevel(TrieSpecifics ts, Position pos:positionBitmap()) =
	"<dec(ts.bitmapField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))>;
	'return nodeOf<1>x<0>(null, <use(bitmapField)>, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0, node);"
when isOptionEnabled(ts, useSpecialization());		
	
@index=2 str generate_bodyOf_mergeOnNextLevel(TrieSpecifics ts, Position _) =
	"<dec(ts.bitmapField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))>;
	'return <toString(call(ts.nodeOf_BitmapIndexedNode, 
		argsOverride = (ts.mutator: NULL(),								
						ts.bitmapField: useExpr(ts.bitmapField),
						ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.valmapField.\type, "0")),						
						ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { <use(\inode(ts.ds, ts.tupleTypes))> }"),
						ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "0")),
						ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "1")))))>;"
when !isOptionEnabled(ts, useSpecialization());	

default str generate_bodyOf_mergeOnNextLevel(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }

@index=2 str generate_bodyOf_mergeNodeAndValue(TrieSpecifics ts, Position pos:positionField()) =
	"// store values before node
	'return nodeOf<1>x<1>(null, (byte) mask1, <use(__payloadTuple(ts.ds, ts.tupleTypes, 1))>, (byte) mask0, node0);"
when isOptionEnabled(ts, useSpecialization());

@index=2 str generate_bodyOf_mergeNodeAndValue(TrieSpecifics ts, Position pos:positionBitmap()) =
	"<dec(ts.bitmapField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))>;
	'<dec(ts.valmapField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>;
	'
	'// store values before node
	'return nodeOf<1>x<1>(null, <use(bitmapField)>, <use(valmapField)>, <use(__payloadTuple(ts.ds, ts.tupleTypes, 1))>, node0);"
when isOptionEnabled(ts, useSpecialization());		
	
@index=2 str generate_bodyOf_mergeNodeAndValue(TrieSpecifics ts, Position _) =
	"<dec(ts.bitmapField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask0))))>;
	'<dec(ts.valmapField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: useExpr(ts.mask1))))>;
	'
	'// store values before node
	'return <toString(call(ts.nodeOf_BitmapIndexedNode, 
		argsOverride = (ts.mutator: NULL(),								
						ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { <use(__payloadTuple(ts.ds, ts.tupleTypes, 1))>, <use(\inode(ts.ds, ts.tupleTypes, 0))> }"),
								ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "1")),
								ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "1")))))>;"
when !isOptionEnabled(ts, useSpecialization());			

default str generate_bodyOf_mergeNodeAndValue(TrieSpecifics _, Option _, Position _) { throw "something went wrong"; }
		
str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase) = 
	"return this;" 
when \set() := ts.ds;	
	
str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase) = 
	"<dec(content(ts, ctVal(isRare = isRareCase), "currentVal"))> = <contentAccessor(ctVal(isRare = isRareCase), "<bitmapPrefix(ctPayloadTuple(isRare = isRareCase))>Index")>;
	'
	'if (<selectEq(op)(val(ts.valType, "currentVal"), val(ts.valType))>) {
	'	return this;
	'} else {
	'	// update mapping
	'	details.updated(<boxPayloadTupleArg1(ts, "currentVal", isRareCase)>);
	'	return copyAndSet<if (isRareCase) {>Rare<}>Value(mutator, bitpos, val);
	'}" 
when \map(multi = false) := ts.ds && isOptionEnabled(ts, compareValueAtMapPut());

str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase) = 
	"<dec(content(ts, ctVal(isRare = isRareCase), "currentVal"))> = <contentAccessor(ctVal(isRare = isRareCase), "<bitmapPrefix(ctPayloadTuple(isRare = isRareCase))>Index")>;
	'
	'// update mapping
	'details.updated(<boxPayloadTupleArg1(ts, "currentVal", isRareCase)>);
	'return copyAndSet<if (isRareCase) {>Rare<}>Value(mutator, bitpos, val);" 
when \map(multi = false) := ts.ds && !isOptionEnabled(ts, compareValueAtMapPut());

str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"<dec(nodeTupleArg(ts, 1))> = getVal(dataIndex);
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
	'	// <dec(appendToName(nodeTupleArg(ts, 1), "New"))> = <toString(call(nodeTupleArg(ts, 1), getDef(tsSet, trieNode(abstractNode()), insertTuple(false, false)), // insertTuple??? 
					argsOverride = (ts.mutator: NULL(), 
						ts.keyHash: call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ival("valHash")))), 
						ts.shift: constant(ts.shift.\type, "0"),
						ts.details: exprFromString("<tsSet.ResultStr>.unchanged()")), // TODO: remove tsSet dependency here
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))
						))>;
	'	<dec(appendToName(collTupleArg(ts, 1), "New"))> = <toString(call(collTupleArg(ts, 1), getDef(tsSet, core(immutable()), insertTuple(false, false)), // insertTuple??? 
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>;
	'
	'	details.modified();
	'	return copyAndSetValue(mutator, bitpos, <use(appendToName(nodeTupleArg(ts, 1), "New"))>);
	'}" 
when \map(multi = true) := ts.ds;	







default str updatedOn_KeysDifferent(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase) = 
	"<if (\map() := ts.ds) {><dec(content(ts, ctVal(isRare = isRareCase), "currentVal"))> = <contentAccessor(ctVal(isRare = isRareCase), "<bitmapPrefix(ctPayloadTuple(isRare = isRareCase))>Index")>;<}> 
	
	final <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> subNodeNew = mergeTwoKeyValPairs(currentKey, <if (\map() := ts.ds) {> currentVal,<}><toString(call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): hashCodeExpr(ts, content(ts, ctKey(isRare = isRareCase), "currentKey")))))>, key, <if (\map() := ts.ds) {> val,<}> keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>);
	'
	'details.modified();
	'return copyAndMigrateFrom<if (isRareCase) {>Rare<}>InlineToNode(mutator, bitpos, subNodeNew);";	
		
str updatedOn_KeysDifferent(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"final int valHash = <hashCode(val(ts.valType))>;
	'// <dec(nodeTupleArg(ts, 1))> = <toString(call(exprFromString("CompactSetNode.EMPTY_NODE"), getDef(tsSet, trieNode(abstractNode()), insertTuple(false, false)), // insertTuple???  
					argsOverride = (ts.mutator: NULL(), 
						ts.keyHash: call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ival("valHash")))), 
						ts.shift: constant(ts.shift.\type, "0"),
						ts.details: exprFromString("<tsSet.ResultStr>.unchanged()")), // TODO: remove dependency on tsSet here
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))						
					))>;
	' <dec(collTupleArg(ts, 1))> = <tsSet.coreSpecializedClassName>.setOf(<use(val(ts.valType))>);
	'
	'<dec(replaceName(nodeTupleArg(ts, 1), "currentValNode"))> = getVal(dataIndex);
	'final <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> subNodeNew = mergeTwoKeyValPairs(currentKey, currentValNode, <toString(call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): hashCodeExpr(ts, content(ts, ctKey(isRare = op.isRare), "currentKey")))))>, key, <use(collTupleArg(ts, 1))>, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>);
	'
	'details.modified();
	'return copyAndMigrateFrom<if (isRareCase) {>Rare<}>InlineToNode(mutator, bitpos, subNodeNew);" 
when \map(multi = true) := ts.ds;





default str updatedOn_NoTuple(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase) = 
	"<use(ts.details)>.modified();
	'return copyAndInsert<if (isRareCase) {>Rare<}>Value(mutator, bitpos, key<if (\map() := ts.ds) {>, val<}>);";
		
str updatedOn_NoTuple(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"final int valHash = <hashCode(val(ts.valType))>;
	'// <dec(nodeTupleArg(ts, 1))> = <toString(call(exprFromString("CompactSetNode.EMPTY_NODE"), getDef(tsSet, trieNode(abstractNode()), insertTuple(false, false)), // insertTuple???  
					argsOverride = (ts.mutator: NULL(), 
						ts.keyHash: call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ival("valHash")))), 
						ts.shift: constant(ts.shift.\type, "0"),
						ts.details: exprFromString("<ts.ResultStr>.unchanged()")),
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))	
					))>;
	' <dec(collTupleArg(ts, 1))> = <tsSet.coreSpecializedClassName>.setOf(<use(val(ts.valType))>);
	'
	'details.modified();
	'return copyAndInsert<if (isRareCase) {>Rare<}>Value(mutator, bitpos, <use(nodeTupleArgs(ts))>);"
when \map(multi = true) := ts.ds;


// TODO: move
str (Argument, Argument) selectEq(PredefOp op) = op has customComparator && op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments;

// data PredefOp = insertTuple(bool isRare, bool customComparator);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:insertTuple(isRare:_, customComparator:_)) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:insertTuple(isRare:_, customComparator:_)) {
	
	Argument subNode 	= \inode(ts.ds, ts.tupleTypes, "subNode");
	Argument subNodeNew = \inode(ts.ds, ts.tupleTypes, "subNodeNew");

	return  
	"<dec(ts.mask)> = <toString(call(getDef(ts, trieNode(compactNode()), mask())))>;
	'<dec(ts.bitposField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos())))>;
	'
	'<insertTuple_checkForInplace(ts, artifact, op, false)>
	'
	'<insertTuple_checkForInplace(ts, artifact, op, true)>
	'
	'// check for node (not value)
	'<dec(val(chunkSizeToPrimitive(ts.bitPartitionSize), "nodeMap"))> = <use(bitmapMethod)>;
	'if (<isBitInBitmap("nodeMap", "bitpos")>) {
	'	<dec(field(primitive("int"), "nodeIndex"))> = index(nodeMap, mask, bitpos);
	'	<dec(subNode)> = <contentAccessor(ctNode(), "nodeIndex")>;
	'	<dec(subNodeNew)> = <use(subNode)>.updated(mutator, <use(ts.payloadTuple)>, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>, <use(ts.details)><if (!(selectEq(op) == equalityDefaultForArguments)) {>, <cmpName><}>);
	'
	'	if (<use(ts.details)>.isModified()) {
	'		return copyAndSetNode(mutator, bitpos, <use(subNodeNew)>);
	'	} else {
	'		return this;
	'	}
	'}
	'
	'// no value
	'<updatedOn_NoTuple(ts, artifact, op, isRare)>"
	;
}

str insertTuple_checkForInplace(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase) {
	return 
		"
		'// check for inplace <if (isRareCase) {>(rare) <}>value
		'<dec(val(chunkSizeToPrimitive(ts.bitPartitionSize), bitmapName(ctKey(isRare = isRareCase))))> = <bitmapAccessor(ctKey(isRare = isRareCase))>;
		'if (<isBitInBitmap(bitmapName(ctKey(isRare = isRareCase)), "bitpos")>) {
		'	<dec(field(primitive("int"), "<bitmapPrefix(ctPayloadTuple(isRare = isRareCase))>Index"))> = index(<bitmapName(ctKey(isRare = isRareCase))>, mask, bitpos);
		'	<dec(content(ts, ctKey(isRare = isRareCase), "currentKey"))> = <contentAccessor(ctKey(isRare = isRareCase), "<bitmapPrefix(ctPayloadTuple(isRare = isRareCase))>Index")>;
		'
		'	<if (op.isRare == isRareCase) {>
		'	if (<selectEq(op)(content(ts, ctKey(isRare = isRareCase), "currentKey"), content(ts, ctKey(isRare = isRareCase)))>) {
		'		<updatedOn_KeysEqual(ts, artifact, op, isRareCase)>				
		'	} else {
		'		<updatedOn_KeysDifferent(ts, artifact, op, isRareCase)>					
		'	}
		'	<} else {>
		'	<updatedOn_KeysDifferent(ts, artifact, op, isRareCase)>
		'	<}>
		'}
		";
}

str isBitInBitmap(str bitmap, str bitpos)
	= "<bitmap> != 0 && (<bitmap> == -1 || (<bitmap> & <bitpos>) != 0)";

bool isPayloadType(ContentType ct) = ct is ctPayloadArg || ct is ctPayloadTuple;

str bitmapAccessor(ContentType ct) = "<bitmapName(ct)>()";
str bitmapName(ContentType ct) = "dataMap" when isPayloadType(ct) && ct.isRare == false; 
str bitmapName(ContentType ct) = "rareMap" when isPayloadType(ct) && ct.isRare == true;
str bitmapPrefix(ContentType ct) = "data" when isPayloadType(ct) && ct.isRare == false; 
str bitmapPrefix(ContentType ct) = "rare" when isPayloadType(ct) && ct.isRare == true;	

str bitmapIndex(ContentType ct) = "<bitmapPrefix(ct)>Index";

str contentAccessor(ContentType ct, str index) = "<contentAccessorMethodName(ct)>(<index>)";

default str removedOn_KeysDifferent(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase) =
	"return this;";

default str removedOn_TupleFound(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase) =
	"<removedOn_TupleFound_modification(ts, artifact, op, isRareCase)>
	
	<removed_value_block1(ts, artifact, op, isRareCase)> else <if (supportsConversionBetweenGenericAndSpecialized(ts)) {><removed_value_block2(ts)> else<}> {					
		return copyAndRemove<if (isRareCase) {>Rare<}>Value(mutator, bitpos);
	}";
	
str removedOn_TupleFound_modification(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase) =
	"<dec(content(ts, ctVal(isRare = isRareCase), "currentVal"))> = <contentAccessor(ctVal(isRare = isRareCase), "<bitmapPrefix(ctPayloadTuple(isRare = isRareCase))>Index")>;
	'details.updated(<boxPayloadTupleArg1(ts, "currentVal", isRareCase)>);"
when \map() := ts.ds;
	
str removedOn_TupleFound_modification(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase) =
	"details.modified();"
when \set() := ts.ds;
	
str removedOn_TupleFound(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"<dec(nodeTupleArg(ts, 1))> = getVal(dataIndex); 
	'
	'final int valHash = <hashCode(val(ts.valType))>;
	'// if(<toString(call(nodeTupleArg(ts, 1), getDef(tsSet, trieNode(abstractNode()), containsKey()), 
					argsOverride = (key(tsSet.keyType): useExpr(val(ts.valType)), tsSet.keyHash: call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ival("valHash")))), tsSet.shift: constant(tsSet.shift.\type, "0"))))>) {
	' if(<toString(call(collTupleArg(ts, 1), getDef(tsSet, core(immutable()), containsKey()) 
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>) {
	'	details.updated(<boxPayloadTupleArg1(ts, "currentVal", isRareCase)>);
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
	'		<removed_value_block1(ts, artifact, op, isRareCase)> else <if (supportsConversionBetweenGenericAndSpecialized(ts)) {><removed_value_block2(ts)> else<}> {					
	'			return copyAndRemoveValue(mutator, bitpos);
	'		}
	'	} else {
	'		return copyAndSetValue(mutator, bitpos, <use(appendToName(nodeTupleArg(ts, 1), "New"))>);		
	'	}	
	'} else {	
	'	return this;
	'}" 
when \map(multi = true) := ts.ds;
		
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:removeTuple()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:removeTuple()) {
	Argument subNode 	= \inode(ts.ds, ts.tupleTypes, "subNode");
	Argument subNodeNew = \inode(ts.ds, ts.tupleTypes, "subNodeNew");

	return
	"<dec(ts.mask)> = <toString(call(getDef(ts, trieNode(compactNode()), mask())))>;
	'<dec(ts.bitposField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos())))>;
	'
	'<removeTuple_checkForInplace(ts, artifact, op, op.isRare)>
	
	// check for node (not value)
	if ((<use(bitmapMethod)> & bitpos) != 0) { // node (not value)
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
			<if (isOptionEnabled(ts,useSpecialization())) {>// inline value (move to front)
				if (payloadArity() == 1) {
					return copyAndMigrateFromNodeToInline(mutator, bitpos, subNodeNew);
				} else {
					return copyAndMigrateFromNodeToRareInline(mutator, bitpos, subNodeNew);
				}
			<} else {>
				if (this.payloadArity() == 0 && this.nodeArity() == 1) {
					// escalate (singleton or empty) result
					return <use(subNodeNew)>;
				} else {
					// inline value (move to front)
					return copyAndMigrateFromNodeToInline(mutator, bitpos, subNodeNew);
				}
			<}>
		}
		default: {
			// modify current node (set replacement node)
			return copyAndSetNode(mutator, bitpos, subNodeNew);
		}
		}		
	}

	// no value
	return this;";
}
	
str removeTuple_checkForInplace(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase) = 
	"// TODO: generalize bitmap declaration/if/index assignment in code generator pattern
	'// check for inplace <if (isRareCase) {>(rare) <}>value
	'<dec(val(chunkSizeToPrimitive(ts.bitPartitionSize), bitmapName(ctPayloadTuple(isRare = isRareCase))))> = <bitmapAccessor(ctKey(isRare = isRareCase))>;
	'if (<isBitInBitmap(bitmapName(ctPayloadTuple(isRare = isRareCase)), "bitpos")>) {
	'	<dec(field(primitive("int"), "<bitmapPrefix(ctPayloadTuple(isRare = isRareCase))>Index"))> = index(<bitmapName(ctPayloadTuple(isRare = isRareCase))>, mask, bitpos);
	'	<dec(content(ts, ctKey(isRare = isRareCase), "currentKey"))> = <contentAccessor(ctKey(isRare = isRareCase), "<bitmapPrefix(ctPayloadTuple(isRare = isRareCase))>Index")>;
	'	
	'	if (<selectEq(op)(content(ts, ctKey(isRare = isRareCase), "currentKey"), content(ts, ctKey(isRare = isRareCase)))>) {
	'		<removedOn_TupleFound(ts, artifact, op, isRareCase)>
	'	} else {
	'		<removedOn_KeysDifferent(ts, artifact, op, isRareCase)>				
	'	}
	'}";
	
default str removed_value_block2(TrieSpecifics ts) =
	"if (this.arity() == <ts.nBound + 1>) {
	'	return removeInplaceValueAndConvertToSpecializedNode(mutator, bitpos);
	'}";
	
/*
        // TODO: support dispatch for heterogeneous case
          if (this.payloadArity() == 0 && this.rarePayloadArity() == 2 && this.nodeArity() == 0) {
            final byte newRawMap =
                (shift == 0) ? (byte) (rawMap2() ^ bitpos) : bitpos(mask(keyHash, 0));

            return nodeOf2x0(mutator, newRawMap, newRawMap, getRareKey(1 - rareIndex),
                getRareVal(1 - rareIndex));
          } else if (this.payloadArity() == 1 && this.rarePayloadArity() == 1
              && this.nodeArity() == 0) {
            final byte newRawMap =
                (shift == 0) ? (byte) (rawMap2() ^ bitpos) : bitpos(mask(keyHash, 0));

            return nodeOf0x1(mutator, (byte) 0, newRawMap, getKey(0), getVal(0));
          } else {
            return copyAndRemoveRareValue(mutator, bitpos);
          }

*/	
	
default str removed_value_block1(TrieSpecifics ts, Artifact artifact, PredefOp op, bool isRareCase) =
	"
	'/*
	' * Case payload == 2:
	' * 	Create new node with remaining pair. The new node
	' * 	will a) either become the new root returned, or b)
	' * 	unwrapped and inlined during returning.
	' */
	'if (this.payloadArity() == <isRareCase ? 0 : 2> && this.rarePayloadArity() == <isRareCase ? 2 : 0> && this.nodeArity() == 0) {
	'	final <typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))> newRawMap = (shift == 0) ? (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (rawMap2() ^ bitpos)
	'					: <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.shift: constant(primitive("int"), "0"))))))>;
	'
	'	<toString(result(call(getDef(ts, trieNode(compactNode()), nodeFactory_Specialization(isRareCase ? 2 : 0, isRareCase ? 0 : 1)),
				argsOverride = (
					ts.bitmapField: exprFromString("newRawMap"), 
					ts.valmapField: exprFromString("newRawMap")),
				labeledArgsOverride = (contentArguments(): exprFromString("<remainderKeyExprStr><if (\map() := ts.ds) {>, <remainderValExprStr><}>")))))>;
	'} else if (this.payloadArity() == 1 && this.rarePayloadArity() == 1 && this.nodeArity() == 0) {
	'	final <typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))> newRawMap = (shift == 0) ? (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (rawMap2() ^ bitpos)
	'					: <toString(call(getDef(ts, trieNode(compactNode()), bitpos()), argsOverride = (ts.mask: call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.shift: constant(primitive("int"), "0"))))))>;
	'
	'	<toString(result(call(getDef(ts, trieNode(compactNode()), nodeFactory_Specialization(isRareCase ? 0 : 2, isRareCase ? 1 : 0)),
				argsOverride = (
					ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)), 
					ts.valmapField: exprFromString("newRawMap")),
				labeledArgsOverride = (contentArguments(): exprFromString("<theOtherRemainderKeyExprStr><if (\map() := ts.ds) {>, <theOtherRemainderValExprStr><}>")))))>;
	'}"
when remainderKeyExprStr := contentAccessor(ctPayloadArg(0, isRare = isRareCase), "1 - <bitmapIndex(ctPayloadTuple(isRare = isRareCase))>"),
		remainderValExprStr := contentAccessor(ctPayloadArg(1, isRare = isRareCase), "1 - <bitmapIndex(ctPayloadTuple(isRare = isRareCase))>"),
		theOtherRemainderKeyExprStr := contentAccessor(ctPayloadArg(0, isRare = !isRareCase), "0"),
		theOtherRemainderValExprStr := contentAccessor(ctPayloadArg(1, isRare = !isRareCase), "0");


	
	
// bit-shifting with signed integers in Java
int oneShiftedLeftBy(int count) = toInt(pow(2, count)) when count >= 0 && count <= 30;
int oneShiftedLeftBy(31) = -2147483648;
default int oneShiftedLeftBy(int count) { throw "Not supported!"; }





list[PredefOp] createNodeFactorySpecializationList(TrieSpecifics ts, TrieNodeType nodeType:compactNode()) 
	= [ nodeFactory_Specialization(i, j) | <i, j> <- ts.legacyNodeFactoryMethodSpecializationsUnderUnsafe ]
when isOptionEnabled(ts, useSunMiscUnsafe());

/*
 * Factory methods are generated until <= ts.nBound + tupleLength(ts.ds) 
 * because untyped specializations (especially for heterogeneous representations) 
 * could add tupleLength(ts.ds) slots per operation.  
 */
list[PredefOp] createNodeFactorySpecializationList(TrieSpecifics ts, TrieNodeType nodeType:compactNode()) 
	= [ nodeFactory_Specialization(i, j) | j <- [0..ts.nMax+1], i <- [0..ts.nMax+1], ((i + j) <= ts.nMax && (i + j) <= ts.nBound + tupleLength(ts.ds))]; 

//str generate_specializationFactoryMethods(TrieSpecifics ts) = 
//	"<for(<i, j> <- ts.legacyNodeFactoryMethodSpecializationsUnderUnsafe) {>
//	'	<implOrOverride(CompactNode_factoryMethod_bitmap(i, j, ts), generate_bodyOf_factoryMethod_bitmap(i, j, ts, CompactNode_factoryMethod_bitmap(i, j, ts)))>
//	'<}>"
//when isOptionEnabled(ts, useSunMiscUnsafe());





data PredefOp = nodeFactory_Specialization(int n, int m);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Specialization(int n, int m))
	= function(\return(jdtToType(abstractNode(ts))), "nodeOf<n>x<m>", generics = ts.genericTupleTypes, args = [ ts.mutator ] + metadataArguments(ts) + labeledArgumentList(contentArguments(), contentArguments(n, m, ts)), argsFilter = argsFilter(ts),
				isActive = true); // !isOptionEnabled(ts, useSunMiscUnsafe())
// isActive = isOptionEnabled(ts, useSpecialization())

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Specialization(int n, int m)) = true;

value generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::nodeFactory_Specialization(int n, int m)) =
	generate_bodyOf_factoryMethod_bitmap(n, m, ts, CompactNode_factoryMethod_bitmap(n, m, ts))
; // when isOptionEnabled(ts, useSpecialization()) && !isOptionEnabled(ts,useUntypedVariables());


str generate_specializationFactoryMethods(TrieSpecifics ts) = 
	"<for(j <- [0..ts.nMax+1], i <- [0..ts.nMax+1], ((i + j) <= ts.nMax && (i + j) <= ts.nBound + 1)) {>
	'	<implOrOverride(CompactNode_factoryMethod_bitmap(i, j, ts), generate_bodyOf_factoryMethod_bitmap(i, j, ts, CompactNode_factoryMethod_bitmap(i, j, ts)))>
	'<}>"
when !isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts,useSpecialization()) && !isOptionEnabled(ts,useUntypedVariables());
	
/**
 * More complicated slot count expression.
 * sort(toList({ n + 2 * m | n <- [0..32+1], m <- [0..32+1], ((n + m) <= 32)}))
 */
str generate_specializationFactoryMethods(TrieSpecifics ts) = 
	"<for(mn <- [0.. tupleLength(ts.ds) * ts.nMax + 1], mn <= tupleLength(ts.ds) * ts.nBound + tupleLength(ts.ds)) {>
	'	<generate_valNodeOf_factoryMethod_bitmap_untyped(mn, ts)>
	'<}>"
when !isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts,useSpecialization()) && isOptionEnabled(ts,useUntypedVariables());

str generate_specializationFactoryMethods(TrieSpecifics ts) = 
	"<for(<i, j> <- ts.legacyNodeFactoryMethodSpecializationsUnderUnsafe) {>
	'	<implOrOverride(CompactNode_factoryMethod_bitmap(i, j, ts), generate_bodyOf_factoryMethod_bitmap(i, j, ts, CompactNode_factoryMethod_bitmap(i, j, ts)))>
	'<}>"
when isOptionEnabled(ts, useSunMiscUnsafe());

default str generate_specializationFactoryMethods(TrieSpecifics ts) = "";





Method CompactNode_factoryMethod_bitmap(int n, int m, TrieSpecifics ts) {
	constructorArgs = ts.mutator + metadataArguments(ts) + contentArguments(n, m, ts);

	return function(\return(jdtToType(abstractNode(ts))), "nodeOf", generics = ts.tupleTypes, args = constructorArgs, argsFilter = argsFilter(ts));	
}

//Expression generate_bodyOf_factoryMethod_bitmap(int n:0, int m:0, TrieSpecifics ts, Method decleration) = 
//	result(call(getDef(ts, trieNode(compactNode()), emptyTrieNodeConstant())));


//bool exists_valNodeOf_factoryMethod_bitmap(n:1, m:0, TrieSpecifics ts: = true;
//str generate_valNodeOf_factoryMethod_bitmap(n:1, m:0, TrieSpecifics ts:{_*, compactionViaFieldToMethod()}) {
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


Statement generate_bodyOf_factoryMethod_bitmap(int n, int m, TrieSpecifics ts, Method decleration) { 
	if (!isOptionEnabled(ts, useSunMiscUnsafe())) {
		fail generate_bodyOf_factoryMethod_bitmap;
	}
	
	str className = className(ts, specializedBitmapIndexedNode(n, m));

	str resultStr =
		"try {	
		'	final Class\<<className>\> dstClass = <className>.class;
		'
		'	final <className> dst = (<className>) (unsafe.allocateInstance(dstClass));
		'	
		'	unsafe.put<capitalize(chunkSizeToPrimitive(ts.bitPartitionSize).\type)>(dst, global<capitalize(lowLevelBitmapName(ts, 0))>Offset, nodeMap);
		'	unsafe.put<capitalize(chunkSizeToPrimitive(ts.bitPartitionSize).\type)>(dst, global<capitalize(lowLevelBitmapName(ts, 1))>Offset, dataMap);
		'
		'	// works in presence of padding
		'	long offset = arrayBase;
		'	<for (arg <- contentArguments(n, m, ts)) {>unsafe.<unsafePutMethodNameFromType(arg.\type)>(dst, offset, <use(arg)>); offset += addressSize;<}>
		'
		'	return dst;
		'} catch (InstantiationException | SecurityException e) {
		'	throw new RuntimeException(e);
		'}";

	return uncheckedStringStatement(resultStr);	
}

@index=2 Expression generate_bodyOf_factoryMethod_bitmap(int n:0, int m:0, TrieSpecifics ts, Method decleration) 
	= result(call(getDef(ts, core(unknownUpdateSemantic()), emptyTrieNodeConstant())));

@index=2 Expression generate_bodyOf_factoryMethod_bitmap(int n, int m, TrieSpecifics ts, Method decleration) 
	= exprFromString("return new <specializedClassNameStr><InferredGenerics(ts.ds, ts.tupleTypes)>(<use(decleration.args)>);")
when (n + m) <= ts.nBound
		&& specializedClassNameStr := "<toString(ts.ds)><m>To<n>Node<ts.classNamePostfix>";

/* TODO: fix argument lists! */
@index=2 Expression generate_bodyOf_factoryMethod_bitmap(int n, int m, TrieSpecifics ts, Method decleration) {
	if (!((n + m) == ts.nBound + 1 && (n + m) < ts.nMax)) {
		fail;
	}

	list[Argument] argsForArray = contentArguments(n, m, ts);

	return result(call(getDef(ts, trieNode(compactNode()), nodeFactory_Array()), 
		argsOverride = (
			ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { <use(argsForArray)> }"),
			ts.BitmapIndexedNode_payloadArity: bconst(m),
			ts.BitmapIndexedNode_nodeArity: bconst(n)
	)));
}

default Expression generate_bodyOf_factoryMethod_bitmap(int n, int m, TrieSpecifics ts, Method decleration) { 
	throw "Arguments out of bounds (n = <n>, m = <m>)."; 
}


bool exists_valNodeOf_factoryMethod_bitmap_untyped(mn:0, TrieSpecifics ts, int n = 0, int m = 0) = true; 
str generate_valNodeOf_factoryMethod_bitmap_untyped(mn:0, TrieSpecifics ts, int n = 0, int m = 0) { 
	// TODO: remove code duplication
	constructorArgs = ts.mutator + metadataArguments(ts) + contentArguments(n, m, ts);

	return
	"static final <GenericsStr(ts.tupleTypes)> <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
	'	return <emptyTrieNodeConstantName>;
	'}"
	;
}

//bool exists_valNodeOf_factoryMethod_bitmap_untyped(n:1, m:0, TrieSpecifics ts: = true;
//str generate_valNodeOf_factoryMethod_bitmap_untyped(n:1, m:0, TrieSpecifics ts:{_*, compactionViaFieldToMethod()}) {
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

bool exists_valNodeOf_factoryMethod_bitmap_untyped(int mn, TrieSpecifics ts, int n = mn, int m = 0) = true;
default str generate_valNodeOf_factoryMethod_bitmap_untyped(int mn, TrieSpecifics ts, int n = mn, int m = 0) {
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
		'	return nodeOf(mutator, <bitmapField.name>, <valmapField.name>, new Object[] { <use(argsForArray)> }<if (!isOptionEnabled(ts,useSandwichArrays())) {>, (byte) <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapField)>)<}>);
		'}
		";
	} else {
		throw "Arguments out of bounds (n = <n>, m = <m>).";
	}
}






list[Argument] invoke_getAndHashCode_for_payloadTuple(DataStructure ds:\map(), list[Type] tupleTypes:[keyType, valType, *_], Argument idx) = [ key(keyType, "Objects.hashCode(getKey(<use(idx)>))"), val(valType, "Objects.hashCode(getVal(<use(idx)>))") ];
list[Argument] invoke_getAndHashCode_for_payloadTuple(DataStructure ds:\set(), list[Type] tupleTypes:[keyType, *_], Argument idx) = [ key(keyType, "Objects.hashCode(getKey(<use(idx)>))") ];




data PredefOp = mergeTwoKeyValPairs(ContentType ct0, ContentType ct1);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:mergeTwoKeyValPairs(ContentType ct0, ContentType ct1))
	=  function(\return(jdtToType(abstractNode(ts))), "mergeTwoKeyValPairs", args = [ *appendToName(contentList(ts, ct0), "0"), ts.keyHash0, *appendToName(contentList(ts, ct1), "1"), ts.keyHash1, ts.shift ], generics = ts.genericTupleTypes);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:mergeTwoKeyValPairs(ContentType ct0, ContentType ct1)) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:mergeTwoKeyValPairs(ContentType ct0, ContentType ct1)) 
	= generate_bodyOf_mergeTwoKeyValPairs(ts, artifact, op);
	
default str generate_bodyOf_mergeTwoKeyValPairs(TrieSpecifics ts, Artifact artifact, PredefOp op) = 
	"// assert !(<equalityDefaultForArguments(key(ts.keyType, "key0"), key(ts.keyType, "key1"))>);
	
	if (<use(ts.shift)> \>= <toString(call(getDef(ts, trieNode(compactNode()), hashCodeLength())))>) {
		return new <ts.hashCollisionClassName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash0, (<typeToString(collTupleType(ts, 0))>[]) new <if (isPrimitive(collTupleArg(ts, 0))) {><typeToString(collTupleType(ts, 0))><} else {>Object<}>[] { <use(appendToName([ "0", "1" ], collTupleArg(ts, 0)))> }
						<if (\map() := ts.ds) {>, (<typeToString(collTupleType(ts, 1))>[]) new <if (isPrimitive(collTupleArg(ts, 1))) {><typeToString(collTupleType(ts, 1))><} else {>Object<}>[] { <use(appendToName([ "0", "1" ], collTupleArg(ts, 1)))> }<}>);
	}

	<dec(ts.mask0)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash0))))>;
	<dec(ts.mask1)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash1))))>;

	if (mask0 != mask1) {
		// both nodes fit on same level
		<generate_bodyOf_mergeTwoValues(ts, artifact, op, positionBitmap())>
	} else {
		final <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> node = <toString(call(getDef(ts, trieNode(compactNode()), op), argsOverride = (ts.shift: plus(useExpr(ts.shift), call(getDef(ts, trieNode(compactNode()), bitPartitionSize()))))))>;
		// values fit on next level

		<generate_bodyOf_mergeOnNextLevel(ts, positionBitmap())>
	}";	
	
@index=2 str generate_bodyOf_mergeTwoKeyValPairs(TrieSpecifics ts, Artifact artifact, PredefOp op) = 
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
when isOptionEnabled(ts, usePathCompression());
		
default str generate_bodyOf_mergeNodeAndKeyValPair(TrieSpecifics ts) = 
	"<dec(ts.mask0)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash0))))>;
	<dec(ts.mask1)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.keyHash: useExpr(ts.keyHash1))))>;

	if (mask0 != mask1) {
		// both nodes fit on same level
		<generate_bodyOf_mergeNodeAndValue(ts, positionBitmap())>
	} else {
		// values fit on next level
		final <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> node = <toString(call(getDef(ts, trieNode(compactNode()), mergeNodeAndKeyValPair()), argsOverride = (ts.shift: plus(useExpr(ts.shift), call(getDef(ts, trieNode(compactNode()), bitPartitionSize()))))))>;
		
		<generate_bodyOf_mergeOnNextLevel(ts, positionBitmap())>
	}";	

@index=2 str generate_bodyOf_mergeNodeAndKeyValPair(TrieSpecifics ts) = 
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
when isOptionEnabled(ts, usePathCompression());


@index=2 str generate_bodyOf_getXXX(TrieSpecifics ts, Artifact artifact, str returnTypeString, str contentTypeString, str unsafeGetMethodNameFromType) {
	Expression callLogicalToPhysical = call(getDef(ts, artifact, logicalToPhysicalIndex()), argsOverride = (ts.contentType: exprFromString(contentTypeString)));

	return
		"try {
		'	final long[] arrayOffsets = (long[]) unsafe.getObject(this.getClass(), globalArrayOffsetsOffset);
		'	return (<returnTypeString>) unsafe.<unsafeGetMethodNameFromType>(this, arrayOffsets[<toString(callLogicalToPhysical)>]);
		'} catch (SecurityException e) {
		'	throw new RuntimeException(e);
		'}";	 
}


data PredefOp = getSlot();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::getSlot())
	= true when isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::getSlot()) 
	= generate_bodyOf_getXXX(ts, artifact, typeToString(object()), "ContentType.SLOT", unsafeGetMethodNameFromType(object())) 
when isOptionEnabled(ts, useSunMiscUnsafe());


//data PredefOp = getKeyFunction();
//
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), getKeyFunction())
//	= function(\return(ts.keyType), "getKey", generics = ts.tupleTypes, args = [classArgumentWithUpperBoundCompactNode(ts, "clazz"), jdtToVal(compactNode(ts), "instance"), ts.index], isActive = isOptionEnabled(ts, useSunMiscUnsafe()));
//
//bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::getKeyFunction())
//	= true when isOptionEnabled(ts, useSunMiscUnsafe());
//
//str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::getKeyFunction()) = 
//	"// TODO: remove try / catch and throw SecurityException instead
//	'try {
//	'	long keyOffset = arrayBase + (TUPLE_LENGTH * addressSize) * index;
//	'	return (<typeToString(ts.keyType)>) unsafe.getObject(instance, keyOffset);
//	'} catch (SecurityException e) {
//	'	throw new RuntimeException(e);
//	'}"
//when isOptionEnabled(ts, useSunMiscUnsafe());


data PredefOp = getContentFunction(ContentType ct);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp op:getContentFunction(ct:ctPayloadArg(0)))
	= function(\return(ct2type(ts)[ct]), "<contentAccessorMethodName(ct)>", generics = ts.tupleTypes, args = [classArgumentWithUpperBoundCompactNode(ts, "clazz"), jdtToVal(compactNode(ts), "instance"), ts.index], isActive = isOptionEnabled(ts, useSunMiscUnsafe()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:getContentFunction(ct:ctPayloadArg(0)))
	= true when isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:getContentFunction(ct:ctPayloadArg(0))) = 
	"// TODO: remove try / catch and throw SecurityException instead
	// TODO: generate global offset for begin of rare payload
	'try {
	'	long keyOffset = arrayBase <if (ct.isRare) {>+ (TUPLE_LENGTH * 4 * instance.payloadArity()) <}>+ (TUPLE_LENGTH * addressSize) * index;
	'	return (<typeToString(ct2type(ts)[ct])>) unsafe.<unsafeGetMethodNameFromType(ct2type(ts)[ct])>(instance, keyOffset);
	'} catch (SecurityException e) {
	'	throw new RuntimeException(e);
	'}"
when isOptionEnabled(ts, useSunMiscUnsafe());



// data PredefOp = getContent(ContentType ct);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), op:getContent(ContentType ct))
	= true when isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), op:getContent(ContentType ct)) 
	= generate_bodyOf_getXXX(ts, artifact, typeToString(ct2type(ts)[ct]), "ContentType.<prettyPrintContentType(ct)>", unsafeGetMethodNameFromType(ct2type(ts)[ct]))
when isOptionEnabled(ts, useSunMiscUnsafe());


data PredefOp = getKeyValueEntry();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::getKeyValueEntry())
	= true when isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::getKeyValueEntry()) 
	= "return entryOf(getKey(index), getVal(index));"
when isOptionEnabled(ts, useSunMiscUnsafe());


// TODO: move somewhere else
Argument classArgumentWithUpperBoundCompactNode(TrieSpecifics ts, str name) = val(specific("Class", typeArguments = [ upperBoundGeneric(specific("<compactNode(ts).typeName>")) ]), name);

data PredefOp = getNodeFunction();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), getNodeFunction())
	= function(\return(jdtToType(abstractNode(ts))), "getNode", generics = ts.tupleTypes, args = [classArgumentWithUpperBoundCompactNode(ts, "clazz"), jdtToVal(compactNode(ts), "instance"), ts.index], isActive = isOptionEnabled(ts, useSunMiscUnsafe()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::getNodeFunction())
	= true when isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::getNodeFunction()) = 
	"// TODO: remove try / catch and throw SecurityException instead
	'try {
	'	final long arrayOffsetLast = unsafe.getLong(clazz, globalArrayOffsetLastOffset);			
	'	final long nodeOffset = arrayOffsetLast - addressSize * index;
	'
	'	return (<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)>) unsafe.getObject(instance, nodeOffset);
	'} catch (SecurityException e) {
	'	throw new RuntimeException(e);
	'}"
when isOptionEnabled(ts, useSunMiscUnsafe());


data PredefOp = arrayOffsetsFunction();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::arrayOffsetsFunction())
	= function(\return(primitive("long", isArray = true)), "arrayOffsets", args = [ val(specific("Class"), "clazz"), val(specific("String", isArray = true), "fieldNames") ],  
		isActive = isOptionEnabled(ts, useSunMiscUnsafe()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::arrayOffsetsFunction()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::arrayOffsetsFunction()) {

	// TODO: remove duplication from args
	Argument clazz = val(specific("Class"), "clazz");
	Argument fieldNames = val(specific("String", isArray = true), "fieldNames");

return 
"try {
	long[] arrayOffsets = new long[<use(fieldNames)>.length];
	
	for (int i = 0; i \< <use(fieldNames)>.length; i++) {
		arrayOffsets[i] = unsafe.objectFieldOffset(<use(clazz)>.getDeclaredField(<use(fieldNames)>[i]));
	}
	
	<if (false) {>
	// I want a foldLeft in the Stream API ...
	boolean isSorted = true;
	// assumes INVALID_FIELD_OFFSET == -1;
	long minOffset = sun.misc.Unsafe.INVALID_FIELD_OFFSET; 
	for (long offset : arrayOffsets) {
		isSorted |= minOffset \< offset;
	}				
	assert isSorted;

	System.out.println(Arrays.toString(arrayOffsets));
	System.out.println();
	<}>
	
	return arrayOffsets;
} catch (NoSuchFieldException | SecurityException e) {
	throw new RuntimeException(e);
}";
}

data PredefOp = fieldOffsetFunction();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::fieldOffsetFunction())
	= function(\return(primitive("long")), "fieldOffset", args = [ val(specific("Class"), "clazz"), val(specific("String"), "fieldName") ], 
		isActive = isOptionEnabled(ts, useSunMiscUnsafe()));

// Default Value for Property
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::fieldOffsetFunction()) = true;

// TODO: does not work for untyped yet.
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::fieldOffsetFunction()) {
	Argument clazz = val(specific("Class"), "clazz");
	//Argument bitmap = val(specific("String"), "fieldName");

	Argument mapField = val(specific("java.util.Optional\<Field\>"), "fieldNameField");
return 
"try {
	List\<Class\> bottomUpHierarchy = new LinkedList\<\>();
	
	Class currentClass = <use(clazz)>;
	while (currentClass != null) {
		bottomUpHierarchy.add(currentClass);
		currentClass = currentClass.getSuperclass();
	}

	<dec(mapField)> = bottomUpHierarchy.stream()
		.flatMap(hierarchyClass -\> Stream.of(hierarchyClass.getDeclaredFields()))
		.filter(f -\> f.getName().equals(fieldName)).findFirst();
			
	if (<use(mapField)>.isPresent()) {
		<if (false) {>System.out.println(unsafe.objectFieldOffset(<use(mapField)>.get()));
		<}>
		if (java.lang.reflect.Modifier.isStatic(<use(mapField)>.get().getModifiers())) {
			return unsafe.staticFieldOffset(<use(mapField)>.get());
		} else {		
			return unsafe.objectFieldOffset(<use(mapField)>.get());
		}
	} else {
		<if (false) {>System.out.println(sun.misc.Unsafe.INVALID_FIELD_OFFSET);
		<}>return sun.misc.Unsafe.INVALID_FIELD_OFFSET;
	}
} catch (SecurityException e) {
	throw new RuntimeException(e);
}";
}


data PredefOp = logicalToPhysicalIndex();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), logicalToPhysicalIndex())
	= method(\return(primitive("int")), "logicalToPhysicalIndex", args = [ ts.contentType, ts.index ]);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::logicalToPhysicalIndex())
	= true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::logicalToPhysicalIndex()) = 
	"final int physicalIndex;

	switch (<use(ts.contentType)>) {
	case KEY:
		physicalIndex = TUPLE_LENGTH * index;
		break;
	case VAL:
		physicalIndex = TUPLE_LENGTH * index + 1;
		break;
	case RARE_KEY:
		physicalIndex = TUPLE_LENGTH * index + TUPLE_LENGTH
				* java.lang.Integer.bitCount(<toString(maskAndWidenBitmapCast(ts, exprFromString("dataMap()")))>);
		break;
	case RARE_VAL:
		physicalIndex = TUPLE_LENGTH * index + TUPLE_LENGTH
				* java.lang.Integer.bitCount(<toString(maskAndWidenBitmapCast(ts, exprFromString("dataMap()")))>) + 1;
		break;
	case NODE:
		physicalIndex = slotArity() - 1 - index;
		break;
	case SLOT:
		physicalIndex = index;
		break;
	default:
		throw new IllegalStateException(\"Cases not exhausted?\");
	}
	
	return physicalIndex;";


Expression callLogicalToPhysical(TrieSpecifics ts, Artifact artifact, ContentType ct, Expression idxExpr)
	= call(getDef(ts, artifact, logicalToPhysicalIndex()), argsOverride = (ts.contentType: ctToConstant(ct), ts.index: idxExpr));


data PredefOp = contentTypeEnum();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::contentTypeEnum())	
	= enum(\return(specific("ContentType")), "ContentType", options = [ ctKey(), ctVal(), ctKey(isRare = true), ctVal(isRare = true), ctNode(), ctSlot() ]);
		

@index=2 str generate_bodyOf_reflectFieldOffset(str classNameString, str fieldNameString) =
	"final Class\<<classNameString>\> dstClass = <classNameString>.class;
	'
	'try {			
	'	return unsafe.staticFieldOffset(dstClass.getDeclaredField(\"<fieldNameString>\"));			
	'} catch (NoSuchFieldException | SecurityException e) {
	'	throw new RuntimeException(e);
	'}";

	
data PredefOp = globalFieldOffset(str fieldName);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::globalFieldOffset(str fieldName))
	= property(\return(primitive("long")), "global<capitalize(fieldName)>Offset", isStateful = true, isConstant = true, hasGetter = false);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::globalFieldOffset(str fieldName)) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::globalFieldOffset(str fieldName))
	= "return fieldOffset(<className(ts, specializedBitmapIndexedNode(2, 0))>.class, \"<fieldName>\");";


data PredefOp = arrayBase();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::arrayBase())
	= property(\return(primitive("long")), "arrayBase", isStateful = true, isConstant = true, hasGetter = false);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::arrayBase()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::arrayBase()) =
	"try {		
	'	// assuems that both are of type Object and next to each other in memory
	'	return DataLayoutHelper.arrayOffsets[0];
	'} catch (SecurityException e) {
	'	throw new RuntimeException(e);
	'}";

	
data PredefOp = addressSize();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::addressSize())
	= property(\return(primitive("long")), "addressSize", isStateful = true, isConstant = true, hasGetter = false);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::addressSize()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::addressSize()) = 
	"try {		
	'	// assuems that both are of type Object and next to each other in memory
	'	return DataLayoutHelper.arrayOffsets[1] - DataLayoutHelper.arrayOffsets[0];
	'} catch (SecurityException e) {
	'	throw new RuntimeException(e);
	'}";


@index=2 str generate_bodyOf_reflectNextClassArray(TrieSpecifics ts, str mNext, str nNext) =
	"Class[][] next = new Class[<dim1>][<dim2>];
	'
	'try {
	'	for (int m = 0; m \<= <dim1-1>; m++) {
	'		for (int n = 0; n \<= <dim2-1>; n++) {
	'			int mNext = <mNext>;
	'			int nNext = <nNext>;
	'			
	'			// TODO: last expression is not properly generated yet and maybe incorrect
	'			if (mNext \< 0 || mNext \> <dim1-1> || nNext \< 0 || nNext \> <dim2-1> || Math.ceil(nNext / 2.0) + mNext \> <ts.nMax>) {
	'				next[m][n] = null;
	'			} else {
	'				next[m][n] = Class.forName(String.format(\"<targetBasePackage>.<ts.coreClassName>$Map%dTo%dNode<ts.classNamePostfix>\", mNext, nNext));
	'			}
	'		}
	'	}
	'} catch (ClassNotFoundException e) {
	'	throw new RuntimeException(e);
	'}
	'
	'return next;"
when partitionList := pscene_typedPayload_typedRarePayload_typedNodes_bounded_simplifyWith_psStripIfReferenceType()
		&& [ int dim1, int dim2 ] := mapper(partitionList, int(Partition p) { return uniqueStates(p); })
	; 

	
data PredefOp = specializationsByContentAndNodes();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::specializationsByContentAndNodes())
	= property(\return(specific("Class[][]")), "specializationsByContentAndNodes", isStateful = true, isConstant = true, hasGetter = false);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::specializationsByContentAndNodes()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::specializationsByContentAndNodes())
	= generate_bodyOf_reflectNextClassArray(ts, "m", "n");

	
data PredefOp = equalsFunctionUnsafe();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::equalsFunctionUnsafe())
	= function(\return(primitive("boolean")), "equals", generics = ts.genericTupleTypes, args = [ field(object(), "o1"), field(object(), "o2") ], visibility = "private", isActive = isOptionEnabled(ts, useSunMiscUnsafe()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::equalsFunctionUnsafe()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp op:equalsFunctionUnsafe()) = 	
	"if (null == o1 || null == o2) {
	'	return false;
	'}
	'if (o1 == o2) {
	'	return true;
	'}
	'if (o1.getClass() != o2.getClass()) {
	'	return false;
	'}
	'
	<generate_equalsFunctionUnsafe_generalPrelude(ts, artifact, op)>
			
	<generate_equalsFunctionUnsafe_compareBitmap(ts, artifact, op, lowLevelBitmapName(ts, 0))>					
					
	<generate_equalsFunctionUnsafe_compareBitmap(ts, artifact, op, lowLevelBitmapName(ts, 1))> 
													
	// compare payload range				
	<comparePayloadRange(ts, artifact, iconst(0), call(val(unknown(), "src"), getDef(ts, artifact, payloadArity(isRare = false))), indexIdentity, indexIdentity)>								

	// compare node range
	<compareNodeRange(ts, artifact, iconst(0), call(val(unknown(), "src"), getDef(ts, artifact, nodeArity())), indexIdentity, indexIdentity)>

	return true;";
	
	
data PredefOp = equals();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::equals()) = isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::equals()) = 	
	"return equals(this, other);";
	
	
data PredefOp = nodeArity();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::nodeArity()) = isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::nodeArity()) = 
	// generate_bodyOf_staticFieldByGlobalOffset(ts, primitive("int"), "globalNodeArityOffset");
	"return Integer.bitCount(<toString(maskAndWidenBitmapCast(ts, exprFromString("nodeMap()")))>);";
	
	
// data PredefOp = payloadArity();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::payloadArity(isRare = _)) = isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::payloadArity(isRare = false)) = 
	// generate_bodyOf_staticFieldByGlobalOffset(ts, primitive("int"), "globalPayloadArityOffset");
	"return Integer.bitCount(<toString(maskAndWidenBitmapCast(ts, exprFromString("dataMap()")))>);";	

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::payloadArity(isRare = true)) = 
	"return Integer.bitCount(<toString(maskAndWidenBitmapCast(ts, exprFromString("rareMap()")))>);";


data PredefOp = slotArity();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::slotArity()) = isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::slotArity()) = 
	generate_bodyOf_staticFieldByGlobalOffset(ts, primitive("int"), "globalSlotArityOffset");
	
	
data PredefOp = untypedSlotArity();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::untypedSlotArity()) = isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::untypedSlotArity()) = 
	generate_bodyOf_staticFieldByGlobalOffset(ts, primitive("int"), "globalUntypedSlotArityOffset");	
	

@index=2 str generate_bodyOf_staticFieldByGlobalOffset(TrieSpecifics ts, Type fieldType, str fieldName) = 
	"try {
	'	return unsafe.<unsafeGetMethodNameFromType(fieldType)>(this.getClass(), <fieldName>);
	'} catch (SecurityException e) {
	'	throw new RuntimeException(e);
	'}";
	
	
data PredefOp = hasNodes();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::hasNodes()) = isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::hasNodes()) = 
	"return nodeArity() != 0;";
	

data PredefOp = hasPayload();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::hasPayload()) = isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::hasPayload()) = 
	"return payloadArity() != 0;";


data PredefOp = hasSlots();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::hasSlots()) = isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:compactNode()), PredefOp::hasSlots()) = 
	"return slotArity() != 0;";
	
	
// data PredefOp = containsKey();	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:containsKey(), str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true; 

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:containsKey(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true; 
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:containsKey(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"<dec(ts.mask)> = <toString(call(getDef(ts, trieNode(compactNode()), mask())))>;
	'<dec(ts.bitposField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos())))>;
	'
	'<dec(val(chunkSizeToPrimitive(ts.bitPartitionSize), "dataMap"))> = <use(valmapMethod)>;
	'if (<isBitInBitmap("dataMap", "bitpos")>) {
	'	final int index = index(dataMap, mask, bitpos);
	'	return <eq(key(ts.keyType, "getKey(index)"), key(ts.keyType))>;
	'}
	'
	'<dec(val(chunkSizeToPrimitive(ts.bitPartitionSize), "nodeMap"))> = <use(bitmapMethod)>;
	'if (<isBitInBitmap("nodeMap", "bitpos")>) {
	'	final int index = index(nodeMap, mask, bitpos);
	'	return <toString(call(exprFromString("getNode(index)"), getDef(ts, artifact, containsKey(customComparator = op.customComparator)), 
			argsOverride = (ts.shift: plus(useExpr(ts.shift), call(getDef(ts, trieNode(compactNode()), bitPartitionSize()))))))>;	
	'}
	'
	'return false;"
when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:containsKey(isRare = bool isRareCase),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> instance = this;
	Class\<? extends <CompactNode(ts.ds)>\> clazz = instance.getClass();

	for (int shift0 = shift; true; shift0 += <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>) {
		<dec(ts.mask)> = <toString(call(getDef(ts, trieNode(compactNode()), mask()), argsOverride = (ts.shift: exprFromString("shift0"))))>;
		<dec(ts.bitposField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos())))>;				
		
		<dec(val(chunkSizeToPrimitive(ts.bitPartitionSize), "nodeMap"))> = instance.<use(bitmapMethod)>;
		// <dec(val(chunkSizeToPrimitive(ts.bitPartitionSize), "nodeMap"))> = unsafe.<unsafeGetMethodNameFromType(chunkSizeToPrimitive(ts.bitPartitionSize))>(instance, global<capitalize(lowLevelBitmapName(ts, 0))>Offset);
		if (<isBitInBitmap("nodeMap", "bitpos")>) {
			final int index = index(nodeMap, mask, bitpos);
			final <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> nestedInstance = getNode(clazz, instance, index);

			try {
				instance = (<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)>) nestedInstance;
				clazz = instance.getClass();
			} catch (ClassCastException unused) {
				return ((<hashCollisionNode(ts).typeName><GenericsStr(ts.tupleTypes)>) nestedInstance).containsKey(key, keyHash, 0);
			}					
		} else {
			<dec(val(chunkSizeToPrimitive(ts.bitPartitionSize), bitmapName(ctKey(isRare = isRareCase))))> = instance.<bitmapAccessor(ctKey(isRare = isRareCase))>;
			// <dec(val(chunkSizeToPrimitive(ts.bitPartitionSize), bitmapName(ctKey(isRare = isRareCase))))> = unsafe.<unsafeGetMethodNameFromType(chunkSizeToPrimitive(ts.bitPartitionSize))>(instance, global<capitalize(lowLevelBitmapName(ts, 1))>Offset);
			if (<isBitInBitmap(bitmapName(ctKey(isRare = isRareCase)), "bitpos")>) {
				final int index = index(<bitmapName(ctKey(isRare = isRareCase))>, mask, bitpos);
				return <eq(content(ts, ctKey(isRare = isRareCase), "get<if (isRareCase) {>Rare<}>Key(clazz, instance, index)"), content(ts, ctKey(isRare = isRareCase)))>;
			} else {
				return false;
			}
		}
	}"
when isOptionEnabled(ts, useSunMiscUnsafe());
	
/* 
		"
		'// check for inplace <if (isRareCase) {>(rare) <}>value
		'<dec(val(chunkSizeToPrimitive(ts.bitPartitionSize), bitmapName(ctKey(isRare = isRareCase))))> = <bitmapAccessor(ctKey(isRare = isRareCase))>;
		'if (<isBitInBitmap(bitmapName(ctKey(isRare = isRareCase)), "bitpos")>) {
		'	<dec(field(primitive("int"), "<bitmapPrefix(ctPayloadTuple(isRare = isRareCase))>Index"))> = index(<bitmapName(ctKey(isRare = isRareCase))>, mask, bitpos);
		'	<dec(content(ts, ctKey(isRare = isRareCase), "currentKey"))> = <contentAccessor(ctKey(isRare = isRareCase), "<bitmapPrefix(ctPayloadTuple(isRare = isRareCase))>Index")>;
		'
		'	<if (op.isRare == isRareCase) {>
		'	if (<selectEq(op)(content(ts, ctKey(isRare = isRareCase), "currentKey"), content(ts, ctKey(isRare = isRareCase)))>) {
		'		<updatedOn_KeysEqual(ts, artifact, op, isRareCase)>				
		'	} else {
		'		<updatedOn_KeysDifferent(ts, artifact, op, isRareCase)>					
		'	}
		'	<} else {>
		'	<updatedOn_KeysDifferent(ts, artifact, op, isRareCase)>
		'	<}>
		'}
		";
*/	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:get(), 
	str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:get(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"<dec(ts.mask)> = <toString(call(getDef(ts, trieNode(compactNode()), mask())))>;
	'<dec(ts.bitposField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos())))>;
	'
	'<dec(val(chunkSizeToPrimitive(ts.bitPartitionSize), bitmapName(ctKey(isRare = op.isRare))))> = this.<bitmapAccessor(ctKey(isRare = op.isRare))>;
	'if (<isBitInBitmap(bitmapName(ctPayloadTuple(isRare = op.isRare)), "bitpos")>) { // inplace value
	'	<dec(ts.index)> = index(<bitmapName(ctKey(isRare = op.isRare))>, mask, bitpos);
	'
	'	if (<eq(content(ts, ctPayloadArg(0, isRare = op.isRare), "<contentAccessorMethodName(ctPayloadArg(0, isRare = op.isRare))>(<use(ts.index)>)"), content(ts, ctPayloadArg(0, isRare = op.isRare)))>) {
	'		<if (\set() := ts.ds) {>return Optional.of(getKey(<use(ts.index)>));<} else {><dec(result)> = <toString(call(getDef(ts, trieNode(abstractNode()), getContent(ctPayloadArg(1, isRare = op.isRare)))))>; 
	'
	'		return Optional.of(<use(result)>);<}>
	'	}
	'
	'	return Optional.empty();
	'}
	'
	'if ((<use(bitmapMethod)> & bitpos) != 0) { // node (not value)
	'	final <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> subNode = nodeAt(bitpos);
	'
	'	return subNode.findByKey(key, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);
	'}
	'
	'return Optional.empty();"
when result := val(primitiveToClass(dsAtFunction__range_type(ts)), "result");	
	
	
	
	
	
	
