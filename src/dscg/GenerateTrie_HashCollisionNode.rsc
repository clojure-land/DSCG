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
	hashCollisionContentArray(ctCollectionArg(0)),
	hashCollisionContentArray(ctCollectionArg(1)),
	
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
//	arrays = [ field(asArray(collTupleArg(ts, 0).\type), "keys") ];
//	if (\map() := ts.ds) {
//		arrays = arrays + [ field(asArray(collTupleArg(ts, 1).\type), "vals")];
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
//		<if (isOptionEnabled(ts, useStructuralEquality())) {>
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
//				<if (\map() := ts.ds) {><if (isPrimitive(ct2type(ts)[ctCollectionArg(0)])) {><dec(field(ct2type(ts)[ctCollectionArg(0)], "otherKey"))><} else {><dec(field(object(), "otherKey"))><}> = that.getKey(i);
//				<if (isPrimitive(ct2type(ts)[ctCollectionArg(1)])) {><dec(field(ct2type(ts)[ctCollectionArg(1)], "otherVal"))><} else {><dec(field(object(), "otherVal"))><}> = that.getVal(i);
//
//				for (int j = 0; j \< keys.length; j++) {
//					<dec(key(ct2type(ts)[ctCollectionArg(0)]))> = keys[j];
//					<dec(collTupleArg(ts, 1))> = vals[j];
//
//					if (<equalityDefaultForArguments(key(ct2type(ts)[ctCollectionArg(0)]), key(ct2type(ts)[ctCollectionArg(0)], "otherKey"))> && <equalityDefaultForArguments(collTupleArg(ts, 1), val(ct2type(ts)[ctCollectionArg(1)], "otherVal"))>) {
//						continue outerLoop;
//					}
//				}
//				return false;<} else {><if (isPrimitive(ct2type(ts)[ctCollectionArg(0)])) {><dec(field(ct2type(ts)[ctCollectionArg(0)], "otherKey"))><} else {><dec(field(object(), "otherKey"))><}> = that.getKey(i);
//
//				for (int j = 0; j \< keys.length; j++) {
//					<dec(key(ct2type(ts)[ctCollectionArg(0)]))> = keys[j];
//
//					if (<equalityDefaultForArguments(key(ct2type(ts)[ctCollectionArg(0)]), key(ct2type(ts)[ctCollectionArg(0)], "otherKey"))>) {
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
//		/* DONE */ <implOrOverride(getDef(ts, trieNode(compactNode()), opToString()), 
//			NOT_YET_IMPLEMENTED_EXCEPTION)>					
//					
//		<impl(ts, trieNode(hashCollisionNode()), isTrieStructureValid())>
//		
//		<if (isOptionEnabled(ts, useHeterogeneousEncoding())) {>
//		/* DONE */ <implOrOverride(getDef(ts, trieNode(compactNode()), getContent(ctPayloadArg(0, isRare = true))), 
//			UNSUPPORTED_OPERATION_EXCEPTION)>
//			
//		/* DONE */ <implOrOverride(getDef(ts, trieNode(compactNode()), getContent(ctPayloadArg(1, isRare = true))),
//			UNSUPPORTED_OPERATION_EXCEPTION)>
//		<}>						
//	}"
//	;
//}

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

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::hashCollisionContentArray(ct:ctCollectionArg(0)))
	= property(\return(asArray(ct2type(ts)[ct])), "keys", isStateful = true, isConstant = false, hasGetter = false, initializeAtConstruction = true);
	
@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::hashCollisionContentArray(ct:ctCollectionArg(1)))
	= property(\return(asArray(ct2type(ts)[ct])), "vals", isStateful = true, isConstant = false, hasGetter = false, initializeAtConstruction = true, isActive = \map() := ts.ds);	
	
	
data PredefOp = hashCollisionHashCode();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::hashCollisionHashCode())
	= property(\return(primitive("int", isArray = false)), "hash", isStateful = true, isConstant = false, hasGetter = false, initializeAtConstruction = true);
	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), hasPayload()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), hasPayload()) =
	"return true;";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), payloadArity(isRare = _)) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), payloadArity(isRare = false)) =
	"return 0;";
	
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), payloadArity(isRare = true)) =
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

// TODO: isOptionEnabled(ts, useHeterogeneousEncoding())
Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getContent(ctPayloadArg(idx, isRare = true))) = 
	UNSUPPORTED_OPERATION_EXCEPTION;


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getContent(ctPayloadArg(idx, isRare = _))) = true;

@index=2 Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getContent(ctPayloadArg(idx, isRare = false))) = 
	ILLEGAL_STATE_EXCEPTION("Converted to `rarePayload`.");

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getContent(ctPayloadArg(idx, isRare = true))) = 
	"return <if (idx == 0) {>keys<} else {>vals<}>[index];";
	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getContent(ctNode())) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getContent(ctNode())) = 
	ILLEGAL_STATE_EXCEPTION("Is leaf node.");
	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), opToString()) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), opToString()) = 
	NOT_YET_IMPLEMENTED_EXCEPTION;

		
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), hashCode()) = true;

// TODO: isOptionEnabled(ts, useStructuralEquality())
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), hashCode()) =
	"final int prime = 31;
	int result = 0;
	result = prime * result + hash;
	result = prime * result + Arrays.hashCode(keys);<if (\map() := ts.ds) {>result = prime * result + Arrays.hashCode(vals);<}>
	return result;";
	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), equals()) = true;

// TODO: isOptionEnabled(ts, useStructuralEquality())
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
		<if (\map() := ts.ds) {><if (isPrimitive(ct2type(ts)[ctCollectionArg(0)])) {><dec(field(ct2type(ts)[ctCollectionArg(0)], "otherKey"))><} else {><dec(field(object(), "otherKey"))><}> = that.getKey(i);
		<if (isPrimitive(ct2type(ts)[ctCollectionArg(1)])) {><dec(field(ct2type(ts)[ctCollectionArg(1)], "otherVal"))><} else {><dec(field(object(), "otherVal"))><}> = that.getVal(i);

		for (int j = 0; j \< keys.length; j++) {
			<dec(key(ct2type(ts)[ctCollectionArg(0)]))> = keys[j];
			<dec(collTupleArg(ts, 1))> = vals[j];

			if (<equalityDefaultForArguments(key(ct2type(ts)[ctCollectionArg(0)]), key(ct2type(ts)[ctCollectionArg(0)], "otherKey"))> && <equalityDefaultForArguments(collTupleArg(ts, 1), val(ct2type(ts)[ctCollectionArg(1)], "otherVal"))>) {
				continue outerLoop;
			}
		}
		return false;<} else {><if (isPrimitive(ct2type(ts)[ctCollectionArg(0)])) {><dec(field(ct2type(ts)[ctCollectionArg(0)], "otherKey"))><} else {><dec(field(object(), "otherKey"))><}> = that.getKey(i);

		for (int j = 0; j \< keys.length; j++) {
			<dec(key(ct2type(ts)[ctCollectionArg(0)]))> = keys[j];

			if (<equalityDefaultForArguments(key(ct2type(ts)[ctCollectionArg(0)]), key(ct2type(ts)[ctCollectionArg(0)], "otherKey"))>) {
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
	= constructor(\return(\type), jdt.typeName, args = [ *fieldList ], visibility = "private", argsFilter = argsFilter(ts)) // metadataArguments(ts)
when jdt := hashCollisionNode(ts) && 
		\type := jdtToType(jdt) &&
		fieldList := [ predefOpToArgument(ts, artifact, op) | op <- declares(ts, nodeType)<1>, op != hashCollisionNodeConstructor(), opDef := getDef(ts, artifact, op), opDef is property, opDef.isActive && opDef.isStateful && opDef.initializeAtConstruction ];

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:hashCollisionNode()), PredefOp::hashCollisionNodeConstructor()) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:hashCollisionNode()), PredefOp::hashCollisionNodeConstructor())
	= compoundStatement([
		uncheckedStringStatement(initFieldsWithIdendity(fieldList)) // TODO: automatically infer which def.args need to be initialized
		//, uncheckedStringStatement("assert payloadArity() \>= 2;")
	])
when def := getDef(ts, artifact, hashCollisionNodeConstructor()) &&
		fieldList := [ predefOpToArgument(ts, artifact, op) | op <- declares(ts, nodeType)<1>, op != hashCollisionNodeConstructor(), opDef := getDef(ts, artifact, op), opDef is property, opDef.isActive && opDef.isStateful && opDef.initializeAtConstruction ];
	
	
	
	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:get(), 
	str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:get(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"for (int i = 0; i \< keys.length; i++) {
		<dec(key(ct2type(ts)[ctCollectionArg(0)], "_key"))> = keys[i];
		if (<eq(key(ct2type(ts)[ctCollectionArg(0)], "_key"), key(ct2type(ts)[ctCollectionArg(0)]))>) {
			<if(\set() := ts.ds) {>return Optional.of(_key);<} else {><dec(collTupleArg(ts, 1))> = vals[i]; return Optional.of(<use(collTupleArg(ts, 1))>);<}>				
		}
	}
	return Optional.empty();";

// previously used arguments (int n, int m)
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:containsKey(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true; 
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:containsKey(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) =
	"if (this.hash == keyHash) {
		for (<typeToString(ct2type(ts)[ctCollectionArg(0)])> k : keys) {
			if (<eq(key(ct2type(ts)[ctCollectionArg(0)], "k"), key(ct2type(ts)[ctCollectionArg(0)]))>) {
				return true;
			}
		}
	}
	return false;";

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = true; 
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	//if (this.hash != keyHash) {
	//	details.modified();
	//	return mergeNodeAndKeyValPair(this, this.hash, <use(ts.payloadTuple)>, keyHash, shift);
	//}

	"assert this.hash == keyHash;
	
	for (int idx = 0; idx \< keys.length; idx++) {
		if (<eq(key(ct2type(ts)[ctCollectionArg(0)], "keys[idx]"), key(ct2type(ts)[ctCollectionArg(0)]))>) {
			<updatedOn_KeysEqual(ts, artifact, op, eq)>
		}
	}
	
	<updatedOn_NoTuple(ts, artifact, op, eq)>";



str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_), 
		str(Argument, Argument) eq) = 
	"return this;" 
when \set() := ts.ds;	
	
// TODO: rewrite as ASTs and merge both cases for maps
str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_), 
		str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"<dec(prependToName(collTupleArg(ts, 1), "current"))> = vals[idx];

	if (<eq(prependToName(collTupleArg(ts, 1), "current"), val(ct2type(ts)[ctCollectionArg(1)]))>) {
		return this;
	} else {
		// add new mapping	
		<dec(field(asArray(collTupleType(ts, 1)), "src"))> = this.vals;
		<arraycopyAndSetTuple(val(asArray(collTupleType(ts, 1)), "src"), val(asArray(collTupleType(ts, 1)), "dst"), 1, [val(ct2type(ts)[ctCollectionArg(1)])], field(primitive("int"), "idx"))>
	
		final <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> thisNew = new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(this.hash, this.keys, dst);
	
		details.updated(<boxPayloadTupleArg1(ts, "currentVal", true)>);
		return thisNew;
	}"
when \map(multi = false) := ts.ds;

// TODO: rewrite as ASTs and merge both cases for maps
str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_), 
		str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"<dec(prependToName(collTupleArg(ts, 1), "current"))> = vals[idx];

	if (<toString(call(prependToName(collTupleArg(ts, 1), "current"), getDef(tsSet, core(immutable()), containsKey(customComparator = op.customComparator)), 
			labeledArgsOverride = (payloadTuple(): useExpr(val(ct2type(ts)[ctCollectionArg(1)])))))>) {
		return this;
	} else {
		// add new mapping
		<dec(appendToName(collTupleArg(ts, 1), "New"))> = <toString(call(prependToName(collTupleArg(ts, 1), "current"), getDef(tsSet, core(immutable()), insertTuple(isRare, customOperator)), 
				labeledArgsOverride = (payloadTuple(): useExpr(val(ct2type(ts)[ctCollectionArg(1)])))))>;				
	
		<dec(field(asArray(collTupleType(ts, 1)), "src"))> = this.vals;
		<arraycopyAndSetTuple(val(asArray(collTupleType(ts, 1)), "src"), val(asArray(collTupleType(ts, 1)), "dst"), 1, [ appendToName(collTupleArg(ts, 1), "New") ], field(primitive("int"), "idx"))>
	
		final <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> thisNew = new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(this.hash, this.keys, dst);
	
		details.modified();
		return thisNew;
	}" 
when \map(multi = true) := ts.ds;	

default str updatedOn_NoTuple(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_), str(Argument, Argument) eq) = 
	"<arraycopyAndInsertTuple(field(asArray(ct2type(ts)[ctCollectionArg(0)]), "this.keys"), field(asArray(ct2type(ts)[ctCollectionArg(0)]), "keysNew"), 1, [key(ct2type(ts)[ctCollectionArg(0)])], field(primitive("int"), "keys.length"))>
	<if (\map() := ts.ds) {><arraycopyAndInsertTuple(field(asArray(ct2type(ts)[ctCollectionArg(1)]), "this.vals"), field(asArray(ct2type(ts)[ctCollectionArg(1)]), "valsNew"), 1, [val(ct2type(ts)[ctCollectionArg(1)])], field(primitive("int"), "vals.length"))><}>
	
	details.modified();
	return new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (\map() := ts.ds) {>, valsNew<}>);";
		
str updatedOn_NoTuple(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_), str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"// add new tuple
	'<dec(appendToName(collTupleArg(ts, 1), "New"))> = <tsSet.coreSpecializedClassName>.setOf(<use(val(ct2type(ts)[ctCollectionArg(1)]))>);	
	'
	'<arraycopyAndInsertTuple(val(asArray(ct2type(ts)[ctCollectionArg(0)]), "this.keys"), val(asArray(ct2type(ts)[ctCollectionArg(0)]), "keysNew"), 1, [key(ct2type(ts)[ctCollectionArg(0)])], field(primitive("int"), "keys.length"))>
	'<arraycopyAndInsertTuple(val(asArray(collTupleType(ts, 1)), "this.vals"), val(asArray(collTupleType(ts, 1)), "valsNew"), 1, [appendToName(collTupleArg(ts, 1), "New")], field(primitive("int"), "vals.length"))>
	
	details.modified();
	return new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (\map() := ts.ds) {>, valsNew<}>);"
when \map(multi = true) := ts.ds;



@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:removeTuple(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:removeTuple(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"for (int idx = 0; idx \< keys.length; idx++) {
		if (<eq(key(ct2type(ts)[ctCollectionArg(0)], "keys[idx]"), key(ct2type(ts)[ctCollectionArg(0)]))>) {
			<if (\map() := ts.ds) {><dec(val(ct2type(ts)[ctCollectionArg(1)], "currentVal"))> = vals[idx]; details.updated(<boxPayloadTupleArg1(ts, "currentVal", true)>);<}><if (\set() := ts.ds) {>details.modified();<}>
			
			if (this.arity() == 1) {						
				return <toString(call(getDef(ts, core(immutable()), emptyTrieNodeConstant())))>;
			} else if (this.arity() == 2) {
				/*
				 * Create root node with singleton element. This node
				 * will be a) either be the new root returned, or b)
				 * unwrapped and inlined.
				 */
				<dec(key(ct2type(ts)[ctCollectionArg(0)], "theOtherKey"))> = (idx == 0) ? keys[1] : keys[0];
				<if (\map() := ts.ds) {><dec(val(ct2type(ts)[ctCollectionArg(1)], "theOtherVal"))> = (idx == 0) ? vals[1] : vals[0];<}>
				return <toString(call(getDef(ts, core(immutable()), emptyTrieNodeConstant())))>.updated(mutator,
								theOtherKey<if (\map() := ts.ds) {>, theOtherVal<}>, keyHash, 0, details<if (!(eq == equalityDefaultForArguments)) {>, cmp<}>);
			} else {
				<arraycopyAndRemoveTuple(field(asArray(ct2type(ts)[ctCollectionArg(0)]), "this.keys"), field(asArray(ct2type(ts)[ctCollectionArg(0)]), "keysNew"), 1, field(primitive("int"), "idx"))>
				<if (\map() := ts.ds) {><arraycopyAndRemoveTuple(field(asArray(ct2type(ts)[ctCollectionArg(1)]), "this.vals"), field(asArray(ct2type(ts)[ctCollectionArg(1)]), "valsNew"), 1, field(primitive("int"), "idx"))><}>
	
				return new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (\map() := ts.ds) {>, valsNew<}>);
			}
		}
	}
	return this;"
when !(\map(multi = true) := ts.ds);
	

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:removeTuple(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:removeTuple(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"for (int idx = 0; idx \< keys.length; idx++) {
		if (<eq(key(ct2type(ts)[ctCollectionArg(0)], "keys[idx]"), key(ct2type(ts)[ctCollectionArg(0)]))>) {					
			<removedOn_TupleFound(ts, artifact, op, eq)>
		}
	}
	return this;"
when \map(multi = true) := ts.ds;	

	
str removedOn_TupleFound(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:removeTuple(), 
		str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"<dec(collCur)> = getVal(idx);
	'
	'if(<toString(call(collCur, getDef(tsSet, core(immutable()), containsKey()) 
					labeledArgsOverride = (payloadTuple(): useExpr(val(ct2type(ts)[ctCollectionArg(1)])))))>) {
	'	details.updated(<boxPayloadTupleArg1(ts, "currentVal", true)>);
	'	
	'	// remove tuple
	'	<dec(collNew)> = <toString(call(collCur, getDef(tsSet, core(immutable()), removeTuple(customComparator = op.customComparator)), 
					labeledArgsOverride = (payloadTuple(): useExpr(val(ct2type(ts)[ctCollectionArg(1)])))))>;
	'	
	'	if (<toString(call(collNew, getDef(tsSet, core(immutable()), size())))> != 0) {
	'		// update mapping
			<arraycopyAndSetTuple(val(asArray(collTupleType(ts, 1)), "this.vals"), field(asArray(collTupleType(ts, 1)), "valsNew"), 1, [collNew], field(primitive("int"), "idx"))>
	
			return new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keys, valsNew);
	'	} else {
	'		// drop mapping
			if (this.arity() == 2) {
				/*
				 * Create root node with singleton element. This node
				 * will be a) either be the new root returned, or b)
				 * unwrapped and inlined.
				 */
				<dec(key(ct2type(ts)[ctCollectionArg(0)], "theOtherKey"))> = (idx == 0) ? keys[1] : keys[0];
				<dec(val(collType, "theOtherVal"))> = (idx == 0) ? vals[1] : vals[0];
				
				<dec(ts.bitmapField)> = 0;
				<dec(ts.valmapField)> = bitpos(mask(hash, 0));
								
				return <AbstractNode(ts.ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator, <use(ts.bitmapField)>, <use(ts.valmapField)>, theOtherKey, theOtherVal);
			} else {
	'			<arraycopyAndRemoveTuple(field(asArray(ct2type(ts)[ctCollectionArg(0)]), "this.keys"), field(asArray(ct2type(ts)[ctCollectionArg(0)]), "keysNew"), 1, field(primitive("int"), "idx"))>
	'			<arraycopyAndRemoveTuple(field(asArray(collType), "this.vals"), field(asArray(collType), "valsNew"), 1, field(primitive("int"), "idx"))>
	'
	'			return new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (\map() := ts.ds) {>, valsNew<}>);		
	'		}
	'	}	
	'} else {	
	'	return this;
	'}" 
when \map(multi = true) := ts.ds 
		&& collCur := prependToName(collTupleArg(ts, 1), "current")
		&& collNew := appendToName(collTupleArg(ts, 1), "New")
		&& collType := collTupleType(ts, 1);
		
		
		
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::isTrieStructureValid())  = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::isTrieStructureValid()) =
	"return true;";
		