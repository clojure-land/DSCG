/**
 * Copyright (c) 2015 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 */
module dscg::GenerateTrie_LeafNode

import dscg::Common;
import dscg::ArrayUtils;

str generateLeafNodeClassString(TrieSpecifics ts) {

//	"private static final class LeafNode<GenericsStr(ts.tupleTypes)> extends <CompactNode(ds)><GenericsStr(ts.tupleTypes)> implements Map.Entry<GenericsStr(ts.tupleTypes)> {
//
//		private final K key;
//		private final V val;
//		private final int keyHash;
//
//		LeafNode(K key, int keyHash, V val) {
//			this.key = key;
//			this.val = val;
//			this.keyHash = keyHash;
//		}
//
//		@Override
//		Result<GenericsStr(ts.tupleTypes)> updated(AtomicReference\<Thread\> mutator, K key, int keyHash, V val, int shift,
//						Comparator\<Object\> cmp) {
//			if (this.keyHash != keyHash)
//				// insert (no collision)
//				return <ts.ResultStr>.modified(mergeNodes(this, this.keyHash, new LeafNode<GenericsStr(ts.tupleTypes)>(key,
//								keyHash, val), keyHash, shift));
//
//			if (cmp.compare(this.key, key) != 0)
//				// insert (hash collision)
//				return <ts.ResultStr>.modified(new LeafleafNode<GenericsStr(ts.tupleTypes)>(keyHash, new LeafNode[] {
//								this, new LeafNode<GenericsStr(ts.tupleTypes)>(key, keyHash, val) }));
//
//			if (cmp.compare(this.val, val) != 0)
//				// value replaced
//				return <ts.ResultStr>.updated(new LeafNode<GenericsStr(ts.tupleTypes)>(key, keyHash, val), val);
//
//			return <ts.ResultStr>.unchanged(this);
//		}
//
//		@Override
//		Result<GenericsStr(ts.tupleTypes)> removed(AtomicReference\<Thread\> mutator, K key, int hash, int shift,
//						Comparator\<Object\> cmp) {
//			if (cmp.compare(this.key, key) == 0) {
//				return <ts.ResultStr>.modified(EMPTY_INDEX_NODE);
//			} else {
//				return <ts.ResultStr>.unchanged(this);
//			}
//		}
//
//		@Override
//		boolean <containsKeyMethodName(ds)>(Object key, int hash, int shift, Comparator\<Object\> cmp) {
//			return this.keyHash == hash && cmp.compare(this.key, key) == 0;
//		}
//
//		@Override
//		Optional<MapsToGenerics> findByKey(Object key, int hash, int shift, Comparator\<Object\> cmp) {
//			if (this.keyHash == hash && cmp.compare(this.key, key) == 0) {
//				return Optional.of((Map.Entry<GenericsStr(ts.tupleTypes)>) this); // TODO: not correct
//			} else {
//				return Optional.empty();
//			}
//		}
//
//		@Override
//		public K getKey() {
//			return key;
//		}
//
//		@Override
//		public V getValue() {
//			return val;
//		}
//
//		@Override
//		public V setValue(V value) {
//			throw new UnsupportedOperationException();
//		}
//
//		@Override
//		int arity() {
//			return 1;
//		}
//
//		@Override
//		public int size() {
//			return 1;
//		}
//
//		@Override
//		boolean hasNodes() {
//			return false;
//		}
//
//		@Override
//		Iterator\<<AbstractNode(ds)><GenericsStr(ts.tupleTypes)>\> nodeIterator() {
//			return Collections.emptyIterator();
//		}
//
//		@Override
//		int nodeArity() {
//			return 0;
//		}
//
//		@Override
//		boolean hasPayload() {
//			return true;
//		}
//
//		@Override
//		SupplierIterator<SupplierIteratorGenerics(ds)> payloadIterator() {
//			return ArrayKeyValueSupplierIterator.of(new Object[] { key, val }, 0, 2);
//		}
//
//		@Override
//		int payloadArity() {
//			return 1;
//		}
//
//		@Override
//		public String toString() {
//			return key + \"=\" + val;
//		}
//
//		@Override
//		public int hashCode() {
//			final int prime = 31;
//			int result = keyHash;
//			result = prime * result + key.hashCode();
//			result = prime * result + ((val == null) ? 0 : val.hashCode());
//			return result;
//		}
//
//		@Override
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
//			LeafNode that = (LeafNode) other;
//			if (keyHash != that.keyHash) {
//				return false;
//			}
//			if (!key.equals(that.key)) {
//				return false;
//			}
//			if (!Objects.equals(val, that.val)) {
//				return false;
//			}
//			return true;
//		}
//	}"

// private static final class LeafNode<GenericsStr(ts.tupleTypes)> extends <CompactNode(ds)><GenericsStr(ts.tupleTypes)> implements Map.Entry<GenericsStr(ts.tupleTypes)> {

	return  
	"private static final class <leafNode(ts).typeName><GenericsStr(ts.tupleTypes)> extends <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> {
		private <dec(arrays[0])>;		
		<if (\map() := ts.ds) {>private <dec(arrays[1])>;<}>
		private final int hash;

		<leafNode(ts).typeName>(final int hash, <dec(arrays[0])><if (\map() := ts.ds) {>, <dec(arrays[1])><}>) {
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
		Iterator\<<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)>\> nodeIterator() {
			return Collections.emptyIterator();
		}

		@Override
		<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator,
						<dec(ts.bitposField)>, <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> node) {
			throw new UnsupportedOperationException();
		}

		@Override
		<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator,
						<dec(ts.bitposField)>, <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> node) {
			throw new UnsupportedOperationException();
		}		
		<}>

	<impl(ts, trieNode(leafNode()), containsKey())>
	<impl(ts, trieNode(leafNode()), containsKey(customComparator = true))>

	<impl(ts, trieNode(leafNode()), get())>
	<impl(ts, trieNode(leafNode()), get(customComparator = true))>

	<impl(ts, trieNode(leafNode()), insertTuple())>
	<impl(ts, trieNode(leafNode()), insertTuple(customComparator = true))>

	<impl(ts, trieNode(leafNode()), removeTuple())>
	<impl(ts, trieNode(leafNode()), removeTuple(customComparator = true))>

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
			return SIZE_MORE_THAN_ONE;
		}

		<implOrOverride(getDef(ts, trieNode(abstractNode()), getKey()),
			"return keys[index];")>		

		<implOrOverride(getDef(ts, trieNode(abstractNode()), getValue()), 
			"return vals[index];")>		

		<impl(ts, trieNode(leafNode()), getKeyValueEntry())>

		@Override
		public <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> getNode(int index) {
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

			<leafNode(ts).typeName><QuestionMarkGenerics(ts.ds, ts.tupleTypes)> that = (<leafNode(ts).typeName><QuestionMarkGenerics(ts.ds, ts.tupleTypes)>) other;

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
				<if (isPrimitive(ts.valType)) {><dec(field(ts.valType, "otherVal"))><} else {><dec(field(object(), "otherVal"))><}> = that.getValue(i);

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

		<implOrOverride(getDef(ts, trieNode(compactNode()), copyAndSetValue()), 
			UNSUPPORTED_OPERATION_EXCEPTION)>

		<implOrOverride(getDef(ts, trieNode(compactNode()), copyAndInsertValue()), 
			UNSUPPORTED_OPERATION_EXCEPTION)>

		<implOrOverride(getDef(ts, trieNode(compactNode()), copyAndRemoveValue()), 
			UNSUPPORTED_OPERATION_EXCEPTION)>

		<implOrOverride(getDef(ts, trieNode(compactNode()), copyAndSetNode()), 
			UNSUPPORTED_OPERATION_EXCEPTION)>

		<implOrOverride(getDef(ts, trieNode(compactNode()), copyAndMigrateFromInlineToNode()), 
			UNSUPPORTED_OPERATION_EXCEPTION)>
		
		<implOrOverride(getDef(ts, trieNode(compactNode()), copyAndMigrateFromNodeToInline()), 
			UNSUPPORTED_OPERATION_EXCEPTION)>
		
		<implOrOverride(getDef(ts, trieNode(compactNode()), removeInplaceValueAndConvertToSpecializedNode()), 
			UNSUPPORTED_OPERATION_EXCEPTION)>	
			
		<implOrOverride(getDef(ts, trieNode(compactNode()), nodeMap()), 
			UNSUPPORTED_OPERATION_EXCEPTION)>
			
		<implOrOverride(getDef(ts, trieNode(compactNode()), dataMap()),
			UNSUPPORTED_OPERATION_EXCEPTION)>
					
		<impl(ts, trieNode(leafNode()), isTrieStructureValid())>					
						
	}"
	;
}