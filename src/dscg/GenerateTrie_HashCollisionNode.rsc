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
module dscg::GenerateTrie_HashCollisionNode

import dscg::Common;
import dscg::ArrayUtils;

default str generateHashCollisionNodeClassString(TrieSpecifics ts) 
	= generateJdtString(ts, jdt, hashCollisionNode())
when jdt := hashCollisionNode(ts, modifierList = [ "private", "static" ]);

lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, TrieNodeType nodeType:hashCollisionNode()) 
	= [ <nodeType,method> | method <- declaredMethodsByHashCollisionNode ];
		
list[PredefOp] declaredMethodsByHashCollisionNode = [

	hashCollisionHashCode(),
	hashCollisionContentArray(ctPayloadArg(0)),
	hashCollisionContentArray(ctPayloadArg(1)),
	
	hashCollisionNodeConstructor()
	
];




data PredefOp = sizePredicate();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::sizePredicate()) = true;
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::sizePredicate())
	= result(call(getDef(ts, trieNode(compactNode()), sizeMoreThanOne())));


data PredefOp = getKeyValueEntry();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getKeyValueEntry())  = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getKeyValueEntry()) = 
	"return entryOf(keys[index], vals[index]);";

//@deprecated
//str __generateHashCollisionNodeClassString(TrieSpecifics ts, bool isLegacy = true) {
//
//	//TrieSpecifics ts = setArtifact(tsSuper, trieNode(hashCollisionNode()));
//
//	arrays = [ field(asArray(nodeTupleArg(ts, 0).\type), "keys") ];
//	if (\map() := ts.ds) {
//		arrays = arrays + [ field(asArray(nodeTupleArg(ts, 1).\type), "vals")];
//	}
//
//	return  
//	"private static final class <hashCollisionNode(ts).typeName><GenericsStr(ts.tupleTypes)> extends <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> {
//		private <dec(arrays[0])>;		
//		<if (\map() := ts.ds) {>private <dec(arrays[1])>;<}>
//		private final int hash;
//
//		<hashCollisionNode(ts).typeName>(final int hash, <dec(arrays[0])><if (\map() := ts.ds) {>, <dec(arrays[1])><}>) {
//			this.keys = keys;
//			<if (\map() := ts.ds) {>this.vals = vals;<}>
//			this.hash = hash;
//
//			assert payloadArity() \>= 2;
//		}
//
//		<implOrOverride(getDef(ts, trieNode(abstractNode()), payloadIterator()),
//			generate_bodyOf_payloadIterator(ts))>
//
//		<if (false) {>
//		@Override
//		public String toString() {			
//			<if (\map() := ts.ds) {>final Object[] keysAndVals = new Object[keys.length + vals.length];
//			for (int i = 0; i \< keys.length; i++) {
//				keysAndVals[2 * i] = keys[i];
//				keysAndVals[2 * i + 1] = vals[i];
//			}
//			return Arrays.toString(keysAndVals);<} else {>return Arrays.toString(keys);<}>
//		}
//
//		@Override
//		Iterator\<<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)>\> nodeIterator() {
//			return Collections.emptyIterator();
//		}
//		<}>
//
//	<impl(ts, trieNode(hashCollisionNode()), containsKey())>
//	<impl(ts, trieNode(hashCollisionNode()), containsKey(customComparator = true))>
//
//	<impl(ts, trieNode(hashCollisionNode()), get())>
//	<impl(ts, trieNode(hashCollisionNode()), get(customComparator = true))>
//
//	<impl(ts, trieNode(hashCollisionNode()), insertTuple(false, false))>
//	<impl(ts, trieNode(hashCollisionNode()), insertTuple(false, true))>
//
//	<impl(ts, trieNode(hashCollisionNode()), removeTuple())>
//	<impl(ts, trieNode(hashCollisionNode()), removeTuple(customComparator = true))>
//
//		/* DONE */ @Override
//		boolean hasPayload() {
//			return true;
//		}
//
//		/* DONE */ @Override
//		int payloadArity() {
//			return keys.length;
//		}
//
//		/* DONE */ @Override
//		boolean hasNodes() {
//			return false;
//		}
//
//		/* DONE */ @Override
//		int nodeArity() {
//			return 0;
//		}
//
//		@Override
//		int arity() {
//			return payloadArity();
//		}
//
//		@Override
//		byte sizePredicate() {
//			return sizeMoreThanOne();
//		}
//
//		/* DONE */ <implOrOverride(getDef(ts, trieNode(abstractNode()), getContent(ctPayloadArg(0))),
//			"return keys[index];")>		
//
//		/* DONE */ <implOrOverride(getDef(ts, trieNode(abstractNode()), getContent(ctPayloadArg(1))), 
//			"return vals[index];")>		
//
//		/* DONE */ <impl(ts, trieNode(hashCollisionNode()), getKeyValueEntry())>
//
//		/* DONE */ @Override
//		public <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> getNode(int index) {
//			throw new IllegalStateException(\"Is leaf node.\");
//		}
//		
//		/* DONE */ <implOrOverride(getDef(ts, trieNode(abstractNode()), getSlot()),
//			UNSUPPORTED_OPERATION_EXCEPTION)>
//
//		/* DONE */ <implOrOverride(getDef(ts, trieNode(abstractNode()), hasSlots()),
//			UNSUPPORTED_OPERATION_EXCEPTION)>
//
//		/* DONE */ <implOrOverride(getDef(ts, trieNode(abstractNode()), slotArity()),
//			UNSUPPORTED_OPERATION_EXCEPTION)>
//
//		<if (isOptionEnabled(ts.setup, useStructuralEquality())) {>
//		/* DONE */ @Override
//		public int hashCode() {
//			final int prime = 31;
//			int result = 0;
//			result = prime * result + hash;
//			result = prime * result + Arrays.hashCode(keys);<if (\map() := ts.ds) {>result = prime * result + Arrays.hashCode(vals);<}>
//			return result;
//		}
//
//		/* DONE */ @Override
//		public boolean equals(Object other) {
//			if (null == other) {
//				return false;
//			}
//			if (this == other) {
//				return true;
//			}
//			if (getClass() != other.getClass()) {
//				return false;
//			}
//
//			<hashCollisionNode(ts).typeName><QuestionMarkGenerics(ts.ds, ts.tupleTypes)> that = (<hashCollisionNode(ts).typeName><QuestionMarkGenerics(ts.ds, ts.tupleTypes)>) other;
//
//			if (hash != that.hash) {
//				return false;
//			}
//
//			if (arity() != that.arity()) {
//				return false;
//			}
//
//			/*
//			 * Linear scan for each key, because of arbitrary element order.
//			 */
//			outerLoop: for (int i = 0; i \< that.payloadArity(); i++) {
//				<if (\map() := ts.ds) {><if (isPrimitive(ts.keyType)) {><dec(field(ts.keyType, "otherKey"))><} else {><dec(field(object(), "otherKey"))><}> = that.getKey(i);
//				<if (isPrimitive(ts.valType)) {><dec(field(ts.valType, "otherVal"))><} else {><dec(field(object(), "otherVal"))><}> = that.getVal(i);
//
//				for (int j = 0; j \< keys.length; j++) {
//					<dec(key(ts.keyType))> = keys[j];
//					<dec(nodeTupleArg(ts, 1))> = vals[j];
//
//					if (<equalityDefaultForArguments(key(ts.keyType), key(ts.keyType, "otherKey"))> && <equalityDefaultForArguments(nodeTupleArg(ts, 1), val(ts.valType, "otherVal"))>) {
//						continue outerLoop;
//					}
//				}
//				return false;<} else {><if (isPrimitive(ts.keyType)) {><dec(field(ts.keyType, "otherKey"))><} else {><dec(field(object(), "otherKey"))><}> = that.getKey(i);
//
//				for (int j = 0; j \< keys.length; j++) {
//					<dec(key(ts.keyType))> = keys[j];
//
//					if (<equalityDefaultForArguments(key(ts.keyType), key(ts.keyType, "otherKey"))>) {
//						continue outerLoop;
//					}
//				}
//				return false;
//				<}>
//			}
//
//			return true;
//		}
//		<}>
//					
//		/* DONE */ <implOrOverride(getDef(ts, trieNode(compactNode()), toString()), 
//			NOT_YET_IMPLEMENTED_EXCEPTION)>					
//					
//		<impl(ts, trieNode(hashCollisionNode()), isTrieStructureValid())>
//		
//		<if (isOptionEnabled(ts.setup, useHeterogeneousEncoding())) {>
//		/* DONE */ <implOrOverride(getDef(ts, trieNode(compactNode()), getContent(ctPayloadArg(0, isRare = true))), 
//			UNSUPPORTED_OPERATION_EXCEPTION)>
//			
//		/* DONE */ <implOrOverride(getDef(ts, trieNode(compactNode()), getContent(ctPayloadArg(1, isRare = true))),
//			UNSUPPORTED_OPERATION_EXCEPTION)>
//		<}>						
//	}"
//	;
//}

@index=2 bool exists_bodyOf_payloadIterator(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound))  = true;
@index=2 str generate_bodyOf_payloadIterator(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound)) = 
	"
		// TODO: change representation of keys and values
		assert keys.length == vals.length;
	
		final Object[] keysAndVals = new Object[keys.length + vals.length];
		for (int i = 0; i \< keys.length; i++) {
			keysAndVals[2 * i] = keys[i];
			keysAndVals[2 * i + 1] = vals[i];
		}
	
		return ArrayKeyValueSupplierIterator.of(keysAndVals);
	"
	;
	
@index=2 bool exists_bodyOf_payloadIterator(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound))  = true;
@index=2 str generate_bodyOf_payloadIterator(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound)) = 
	"
		final Object[] keysAndVals = new Object[2 * keys.length];
		for (int i = 0; i \< keys.length; i++) {
			keysAndVals[2 * i] = keys[i];
			keysAndVals[2 * i + 1] = keys[i];
		}
	
		return ArrayKeyValueSupplierIterator.of(keysAndVals);
	"
	;	

	
data PredefOp = hashCollisionContentArray(ContentType ct);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::hashCollisionContentArray(ct:ctPayloadArg(0)))
	= property(\return(asArray(ts.ct2type[ct])), "keys", isStateful = true, isConstant = false, hasGetter = false, initializeAtConstruction = true);
	
@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::hashCollisionContentArray(ct:ctPayloadArg(1)))
	= property(\return(asArray(ts.ct2type[ct])), "vals", isStateful = true, isConstant = false, hasGetter = false, initializeAtConstruction = true, isActive = \map() := ts.ds);	
	
	
data PredefOp = hashCollisionHashCode();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::hashCollisionHashCode())
	= property(\return(primitive("int", isArray = false)), "hash", isStateful = true, isConstant = false, hasGetter = false, initializeAtConstruction = true);
	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), hasPayload()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), hasPayload()) =
	"return true;";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), payloadArity()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), payloadArity()) =
	"return keys.length;";
	

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), hasNodes()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), hasNodes()) =
	"return false;";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), nodeArity()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), nodeArity()) =
	"return 0;";
	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), hasSlots()) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), hasSlots()) =
	UNSUPPORTED_OPERATION_EXCEPTION;


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), slotArity()) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), slotArity()) =
	UNSUPPORTED_OPERATION_EXCEPTION;
	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), untypedSlotArity()) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), untypedSlotArity()) =
	UNSUPPORTED_OPERATION_EXCEPTION;	


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getSlot()) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getSlot()) =
	UNSUPPORTED_OPERATION_EXCEPTION;
	

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getContent(ctPayloadArg(idx, isRare = true))) = true;

// TODO: isOptionEnabled(ts.setup, useHeterogeneousEncoding())
Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getContent(ctPayloadArg(idx, isRare = true))) = 
	UNSUPPORTED_OPERATION_EXCEPTION;


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getContent(ctPayloadArg(idx))) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getContent(ctPayloadArg(idx))) = 
	"return <if (idx == 0) {>keys<} else {>vals<}>[index];";
	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getContent(ctNode())) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getContent(ctNode())) = 
	ILLEGAL_STATE_EXCEPTION("Is leaf node.");
	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), toString()) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), toString()) = 
	NOT_YET_IMPLEMENTED_EXCEPTION;

		
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), hashCode()) = true;

// TODO: isOptionEnabled(ts.setup, useStructuralEquality())
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), hashCode()) =
	"final int prime = 31;
	int result = 0;
	result = prime * result + hash;
	result = prime * result + Arrays.hashCode(keys);<if (\map() := ts.ds) {>result = prime * result + Arrays.hashCode(vals);<}>
	return result;";
	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), equals()) = true;

// TODO: isOptionEnabled(ts.setup, useStructuralEquality())
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), equals()) =
	"if (null == other) {
		return false;
	}
	if (this == other) {
		return true;
	}
	if (getClass() != other.getClass()) {
		return false;
	}

	<hashCollisionNode(ts).typeName><QuestionMarkGenerics(ts.ds, ts.tupleTypes)> that = (<hashCollisionNode(ts).typeName><QuestionMarkGenerics(ts.ds, ts.tupleTypes)>) other;

	if (hash != that.hash) {
		return false;
	}

	if (arity() != that.arity()) {
		return false;
	}

	/*
	 * Linear scan for each key, because of arbitrary element order.
	 */
	outerLoop: for (int i = 0; i \< that.payloadArity(); i++) {
		<if (\map() := ts.ds) {><if (isPrimitive(ts.keyType)) {><dec(field(ts.keyType, "otherKey"))><} else {><dec(field(object(), "otherKey"))><}> = that.getKey(i);
		<if (isPrimitive(ts.valType)) {><dec(field(ts.valType, "otherVal"))><} else {><dec(field(object(), "otherVal"))><}> = that.getVal(i);

		for (int j = 0; j \< keys.length; j++) {
			<dec(key(ts.keyType))> = keys[j];
			<dec(nodeTupleArg(ts, 1))> = vals[j];

			if (<equalityDefaultForArguments(key(ts.keyType), key(ts.keyType, "otherKey"))> && <equalityDefaultForArguments(nodeTupleArg(ts, 1), val(ts.valType, "otherVal"))>) {
				continue outerLoop;
			}
		}
		return false;<} else {><if (isPrimitive(ts.keyType)) {><dec(field(ts.keyType, "otherKey"))><} else {><dec(field(object(), "otherKey"))><}> = that.getKey(i);

		for (int j = 0; j \< keys.length; j++) {
			<dec(key(ts.keyType))> = keys[j];

			if (<equalityDefaultForArguments(key(ts.keyType), key(ts.keyType, "otherKey"))>) {
				continue outerLoop;
			}
		}
		return false;
		<}>
	}

	return true;";		

	
// TODO: factor logic used in this constructor implementation
data PredefOp = hashCollisionNodeConstructor();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:hashCollisionNode()), PredefOp::hashCollisionNodeConstructor())
	= constructor(\return(\type), jdt.typeName, args = [ *fieldList ], visibility = "private", argsFilter = ts.argsFilter) // metadataArguments(ts)
when jdt := hashCollisionNode(ts) && 
		\type := jdtToType(jdt) &&
		fieldList := [ predefOpToArgument(ts, artifact, op) | op <- declares(ts, nodeType)<1>, op != hashCollisionNodeConstructor(), opDef := getDef(ts, artifact, op), opDef is property, opDef.isActive && opDef.isStateful && opDef.initializeAtConstruction ];

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:hashCollisionNode()), PredefOp::hashCollisionNodeConstructor()) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:hashCollisionNode()), PredefOp::hashCollisionNodeConstructor())
	= compoundStatement([
		uncheckedStringStatement(initFieldsWithIdendity(fieldList)), // TODO: automatically infer which def.args need to be initialized
		uncheckedStringStatement("assert payloadArity() \>= 2;")
	])
when def := getDef(ts, artifact, hashCollisionNodeConstructor()) &&
		fieldList := [ predefOpToArgument(ts, artifact, op) | op <- declares(ts, nodeType)<1>, op != hashCollisionNodeConstructor(), opDef := getDef(ts, artifact, op), opDef is property, opDef.isActive && opDef.isStateful && opDef.initializeAtConstruction ];
	