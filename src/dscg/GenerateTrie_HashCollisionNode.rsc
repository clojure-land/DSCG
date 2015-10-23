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


data PredefOp = sizePredicate();

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::sizePredicate()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::sizePredicate())
	= result(call(getDef(ts, trieNode(compactNode()), sizeMoreThanOne())));


data PredefOp = getKeyValueEntry();

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getKeyValueEntry())  = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getKeyValueEntry()) = 
	"return entryOf(keys[index], vals[index]);";


str generateHashCollisionNodeClassString(TrieSpecifics ts, bool isLegacy = true) {

	//TrieSpecifics ts = setArtifact(tsSuper, trieNode(hashCollisionNode()));

	arrays = [ field(asArray(nodeTupleArg(ts, 0).\type), "keys") ];
	if (\map() := ts.ds) {
		arrays = arrays + [ field(asArray(nodeTupleArg(ts, 1).\type), "vals")];
	}

	return  
	"private static final class <hashCollisionNode(ts).typeName><GenericsStr(ts.tupleTypes)> extends <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> {
		private <dec(arrays[0])>;		
		<if (\map() := ts.ds) {>private <dec(arrays[1])>;<}>
		private final int hash;

		<hashCollisionNode(ts).typeName>(final int hash, <dec(arrays[0])><if (\map() := ts.ds) {>, <dec(arrays[1])><}>) {
			this.keys = keys;
			<if (\map() := ts.ds) {>this.vals = vals;<}>
			this.hash = hash;

			assert payloadArity() \>= 2;
		}

		<implOrOverride(getDef(ts, trieNode(abstractNode()), payloadIterator()),
			generate_bodyOf_payloadIterator(ts))>

		<if (false) {>
		@Override
		public String toString() {			
			<if (\map() := ts.ds) {>final Object[] keysAndVals = new Object[keys.length + vals.length];
			for (int i = 0; i \< keys.length; i++) {
				keysAndVals[2 * i] = keys[i];
				keysAndVals[2 * i + 1] = vals[i];
			}
			return Arrays.toString(keysAndVals);<} else {>return Arrays.toString(keys);<}>
		}

		@Override
		Iterator\<<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)>\> nodeIterator() {
			return Collections.emptyIterator();
		}
		<}>

	<impl(ts, trieNode(hashCollisionNode()), containsKey())>
	<impl(ts, trieNode(hashCollisionNode()), containsKey(customComparator = true))>

	<impl(ts, trieNode(hashCollisionNode()), get())>
	<impl(ts, trieNode(hashCollisionNode()), get(customComparator = true))>

	<impl(ts, trieNode(hashCollisionNode()), insertTuple(false, false))>
	<impl(ts, trieNode(hashCollisionNode()), insertTuple(false, true))>

	<impl(ts, trieNode(hashCollisionNode()), removeTuple())>
	<impl(ts, trieNode(hashCollisionNode()), removeTuple(customComparator = true))>

		@Override
		boolean hasPayload() {
			return true;
		}

		@Override
		int payloadArity() {
			return keys.length;
		}

		@Override
		boolean hasNodes() {
			return false;
		}

		@Override
		int nodeArity() {
			return 0;
		}

		@Override
		int arity() {
			return payloadArity();
		}

		@Override
		byte sizePredicate() {
			return sizeMoreThanOne();
		}

		<implOrOverride(getDef(ts, trieNode(abstractNode()), getContent(ctPayloadArg(0))),
			"return keys[index];")>		

		<implOrOverride(getDef(ts, trieNode(abstractNode()), getContent(ctPayloadArg(1))), 
			"return vals[index];")>		

		<impl(ts, trieNode(hashCollisionNode()), getKeyValueEntry())>

		@Override
		public <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> getNode(int index) {
			throw new IllegalStateException(\"Is leaf node.\");
		}
		
		<implOrOverride(getDef(ts, trieNode(abstractNode()), getSlot()),
			UNSUPPORTED_OPERATION_EXCEPTION)>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), hasSlots()),
			UNSUPPORTED_OPERATION_EXCEPTION)>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), slotArity()),
			UNSUPPORTED_OPERATION_EXCEPTION)>

		<if (isOptionEnabled(ts.setup, useStructuralEquality())) {>
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 0;
			result = prime * result + hash;
			result = prime * result + Arrays.hashCode(keys);<if (\map() := ts.ds) {>result = prime * result + Arrays.hashCode(vals);<}>
			return result;
		}

		@Override
		public boolean equals(Object other) {
			if (null == other) {
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

			return true;
		}
		<}>
					
		<impl(ts, trieNode(hashCollisionNode()), isTrieStructureValid())>
		
		<implOrOverride(getDef(ts, trieNode(compactNode()), getContent(ctPayloadArg(0, isRare = true))), 
			UNSUPPORTED_OPERATION_EXCEPTION)>
			
		<implOrOverride(getDef(ts, trieNode(compactNode()), getContent(ctPayloadArg(1, isRare = true))),
			UNSUPPORTED_OPERATION_EXCEPTION)>
						
	}"
	;
}

bool exists_bodyOf_payloadIterator(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound))  = true;
str generate_bodyOf_payloadIterator(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound)) = 
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
	
bool exists_bodyOf_payloadIterator(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound))  = true;
str generate_bodyOf_payloadIterator(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound)) = 
	"
		final Object[] keysAndVals = new Object[2 * keys.length];
		for (int i = 0; i \< keys.length; i++) {
			keysAndVals[2 * i] = keys[i];
			keysAndVals[2 * i + 1] = keys[i];
		}
	
		return ArrayKeyValueSupplierIterator.of(keysAndVals);
	"
	;	
