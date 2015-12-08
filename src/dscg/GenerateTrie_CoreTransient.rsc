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
module dscg::GenerateTrie_CoreTransient

import List;
import String;
import dscg::Common;
import dscg::Common_Iterator;
import dscg::GenerateTrie_Core_Common;

str generateCoreTransientClassString(TrieSpecifics ts) { 
	
	//TrieSpecifics ts = setArtifact(tsSuper, core(transient()));
	
	str classNameStr = "TransientTrie<toString(ts.ds)><ts.classNamePostfix>";	
	str persistentClassName = "Trie<toString(ts.ds)><ts.classNamePostfix>";
	
	return
	"static final class <classNameStr><GenericsStr(ts.tupleTypes)> implements
					Transient<toString(ts.ds)><CollectionGenericsExpandedStr(ts)> {
		final private AtomicReference\<Thread\> mutator;
		private <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> rootNode;
		private int hashCode;
		private int cachedSize;

		<classNameStr>(<persistentClassName><GenericsStr(ts.tupleTypes)> <uncapitalize(persistentClassName)>) {
			this.mutator    = new AtomicReference\<Thread\>(Thread.currentThread());
			this.rootNode   = <uncapitalize(persistentClassName)>.rootNode;
			this.hashCode   = <uncapitalize(persistentClassName)>.hashCode;
			this.cachedSize = <uncapitalize(persistentClassName)>.cachedSize;
			if (DEBUG) {
				assert checkHashCodeAndSize(hashCode, cachedSize);
			}
		}
		
		<generate_checkHashCodeAndSize(ts)>	

		<impl(ts, core(transient()), put())>
		<impl(ts, core(transient()), putAll())>

		<impl(ts, core(transient()), add())>
		<impl(ts, core(transient()), addAll())>

		<impl(ts, core(transient()), clear())>
		<impl(ts, core(transient()), remove())>
		
		<impl(ts, core(transient()), removeAll())>
		<impl(ts, core(transient()), retainAll())>
		
		<impl(ts, core(transient()), containsKey(isRare = false, customComparator = false))>
		<impl(ts, core(transient()), containsKey(isRare = false, customComparator = true))>
		<impl(ts, core(transient()), containsKey(isRare = true, customComparator = false))>
		<impl(ts, core(transient()), containsKey(isRare = true, customComparator = true))>	

		<impl(ts, core(transient()), containsValue())>
		<impl(ts, core(transient()), containsValue(customComparator = true))>

		<impl(ts, core(transient()), containsEntry())>
		<impl(ts, core(transient()), containsEntry(customComparator = true))>
		
		<impl(ts, core(transient()), get(isRare = false, customComparator = false))>
		<impl(ts, core(transient()), get(isRare = false, customComparator = true))>
		
		<impl(ts, core(transient()), insertTuple(false, false))>
		<impl(ts, core(transient()), insertTuple(false, true))>
		<impl(ts, core(transient()), insertTuple(true, false))>
		<impl(ts, core(transient()), insertTuple(true, true))>	

		<impl(ts, core(transient()), insertCollection())>
		<impl(ts, core(transient()), insertCollection(customComparator = true))>

		<impl(ts, core(transient()), removeTuple(isRare = false, customComparator = false))>
		<impl(ts, core(transient()), removeTuple(isRare = false, customComparator = true))>
		<impl(ts, core(transient()), removeTuple(isRare = true, customComparator = false))>
		<impl(ts, core(transient()), removeTuple(isRare = true, customComparator = true))>

		<impl(ts, core(transient()), removeCollection())>
		<impl(ts, core(transient()), removeCollection(customComparator = true))>
	
		<impl(ts, core(transient()), retainCollection())>
		<impl(ts, core(transient()), retainCollection(customComparator = true))>
	
		<if (ts.ds == \set()) {>
		@Override
		public boolean containsAll(Collection\<?\> c) {
			for (Object item : c) {
				if (!contains(item)) {
					return false;
				}
			}
			return true;
		}
		
		@Override
		public boolean containsAllEquivalent(Collection\<?\> c, Comparator\<Object\> cmp) {
			for (Object item : c) {
				if (!containsEquivalent(item, cmp)) {
					return false;
				}
			}
			return true;
		}	
		<}>

		<impl(ts, core(transient()), size())>
	
		<impl(ts, core(transient()), isEmpty())>
	
		<impl(ts, core(transient()), iterator())>

		<impl(ts, core(transient()), keyIterator())>

		<impl(ts, core(transient()), valueIterator())>

		<impl(ts, core(transient()), entryIterator())>

		<impl(ts, core(transient()), tupleIterator())>

		<impl(ts, core(transient()), valueCollectionsSpliterator())>
	
		<impl(ts, core(transient()), valueCollectionsStream())>


		<declareJdt(ts, keyIterator(core(transient())))>
		
		<declareJdt(ts, valueIterator(core(transient())))>
		
		<declareJdt(ts, entryIterator(core(transient())))>
		
		<declareJdt(ts, tupleIterator(core(transient())))>

		<implOrOverride(getDef(ts, core(transient()), keySet()),
			generate_bodyOf_jul_Map_keySet(ts, ts.coreTransientClassName))>
			
		<implOrOverride(getDef(ts, core(transient()), values()), 
			generate_bodyOf_jul_Map_values(ts, ts.coreTransientClassName))>

		<implOrOverride(getDef(ts, core(transient()), entrySet()),
			generate_bodyOf_jul_Map_entrySet(ts, ts.coreTransientClassName))>
	
		<implOrOverride(getDef(ts, core(transient()), toObjectArray()),
			generate_bodyOf_jul_Collection_toObjectArray(ts))>
		
		<implOrOverride(getDef(ts, core(transient()), toGenericArray()),		
			generate_bodyOf_jul_Collection_toGenericArray(ts))>

		<implOrOverride(getDef(ts, core(transient()), equals()),
			generate_bodyOf_CoreCommon_equals(ts, ts.coreTransientClassName))>

		@Override
		public int hashCode() {
			return hashCode;
		}

		@Override
		public Immutable<toString(ts.ds)><CollectionGenericsExpandedStr(ts)> freeze() {
			if (mutator.get() == null) {
				throw new IllegalStateException(\"Transient already frozen.\");
			}

			mutator.set(null);
			return new <persistentClassName><GenericsStr(ts.tupleTypes)>(rootNode, hashCode, cachedSize);
		}
	}"
	;
}


/*
 * TODO: Merge with function above; major differences are:
 * 			* return types;
 * 			* additional cases for handling value replacement.
 */
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_)) = true; 
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_)) =
	"if (mutator.get() == null) {
		throw new IllegalStateException(\"Transient already frozen.\");
	}

	<dec(ts.keyHash)> = <hashCode(content(ts, ctPayloadArg(0, isRare = op.isRare)))>;
	<dec(ts.details)> = <ts.ResultStr>.unchanged();
	
	<dec(\inode(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), insertTuple(op.isRare, op.customComparator)), 
					argsOverride = (ts.keyHash: call(getDef(ts, artifact, PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ts.keyHash))), 
					ts.shift: constant(ts.shift.\type, "0"))))>;

	if (<use(ts.details)>.isModified()) {
		<if (\map(multi = false) := ts.ds) {>if (<use(ts.details)>.hasReplacedValue()) {
			<dec(replacedValueHandle)> = <toString(replacedValueExpr)>;

			<dec(valHashOld)> = <toString(hashCodeExpr(
					ts, 
					replacedValueHandle))>;
			<dec(valHashNew)> = <toString(hashCodeExpr(
					ts, 
					content(ts, ctPayloadArg(1, isRare = op.isRare))))>;
						
			rootNode = newRootNode;
			<toString(expressionStatement(assign(ts.hashCodeProperty, updateProperty(ts, op, hashCodeProperty(), onReplacedValue(), tupleHashesOld = cutToTupleSize(ts, [ useExpr(ts.keyHash), useExpr(valHashOld) ]), tupleHashesNew = cutToTupleSize(ts, [ useExpr(ts.keyHash), useExpr(valHashNew) ])))))>
			<toString(expressionStatement(assign(ts.sizeProperty, updateProperty(ts, op, sizeProperty(), onReplacedValue()))))>

			if (DEBUG) {
				assert checkHashCodeAndSize(hashCode, cachedSize);
			}
			return <toString(resultOf(ts, artifact, op, onReplacedValue(), payloadTupleExprList = cutToTupleSize(ts, [ useExpr(content(ts, ctPayloadArg(0, isRare = op.isRare))), useExpr(replacedValueHandle) ])))>;
		} else {<}>			
			<if (\map() := ts.ds) {><dec(valHashNew)> = <hashCode(content(ts, ctPayloadArg(1, isRare = op.isRare)))>;<}>rootNode = newRootNode;
			<toString(expressionStatement(assign(ts.hashCodeProperty, updateProperty(ts, op, hashCodeProperty(), onInsert(), tupleHashesNew = cutToTupleSize(ts, [ useExpr(ts.keyHash), useExpr(valHashNew) ])))))>
			<toString(expressionStatement(assign(ts.sizeProperty, updateProperty(ts, op, sizeProperty(), onInsert()))))>
		
			if (DEBUG) {
				assert checkHashCodeAndSize(hashCode, cachedSize);
			}
			return <toString(resultOf(ts, artifact, op, onInsert()))>;
		<if (\map(multi = false) := ts.ds) {>}<}>
	}

	if (DEBUG) {
		assert checkHashCodeAndSize(hashCode, cachedSize);
	}
	return <toString(resultOf(ts, artifact, op, onInsertAlreadyPresent()))>;"
when valHashOld := val(primitive("int"), "valHashOld")
		&& valHashNew := val(primitive("int"), "valHashNew")
		&& replacedValueExpr := eitherTypeAsSpecificTypeCast(
									exprFromString("<use(ts.details)>.getReplacedValue()"), 					
									__new__internalPayloadTupleTypes__(ts)[1],					 
									content(ts, ctPayloadArg(1, isRare = op.isRare)).\type)
		&& replacedValueHandle := content(ts, ctPayloadArg(1, isRare = op.isRare), "replacedValue")								
		&& rootNode := jdtToVal(abstractNode(ts), "rootNode");

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_), onInsert(), list[Expression] payloadTupleExprList = []) 
	= NULL()
when \map(multi = false) := ts.ds;

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_), onInsert(), list[Expression] payloadTupleExprList = []) 
	= boolean(true)
when !(\map(multi = false) := ts.ds);

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_), onReplacedValue(), list[Expression] payloadTupleExprList = []) 
	= payloadTupleExprList[1]
when \map(multi = false) := ts.ds;

//Expression resultOf(TrieSpecifics ts, op:insertTuple(isRare:_, customComparator:_), onReplacedValue(), list[Expression] payloadTupleExprList = []) 
//	= NULL()
//when core(transient()) := artifact && !(\map(multi = false) := ts.ds);

// implies: either newly inserted, or exact tuple already present
Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_), onInsertAlreadyPresent(), list[Expression] payloadTupleExprList = []) 
	= NULL()
when \map(multi = false) := ts.ds;

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_), onInsertAlreadyPresent(), list[Expression] payloadTupleExprList = []) 
	= boolean(false)
when !(\map(multi = false) := ts.ds);


/*
 * TODO: use different return types dependent on data types. 
 */
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:removeTuple()) = true; 
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:removeTuple()) =
	"if (mutator.get() == null) {
		throw new IllegalStateException(\"Transient already frozen.\");
	}

	<dec(ts.keyHash)> = <hashCode(content(ts, ctPayloadArg(0, isRare = op.isRare)))>;
	<dec(ts.details)> = <ts.ResultStr>.unchanged();
	
	<dec(\inode(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), removeTuple(customComparator = op.customComparator)), 
					argsOverride = (ts.keyHash: call(getDef(ts, artifact, PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ts.keyHash))), ts.shift: constant(ts.shift.\type, "0"))))>;

	if (<use(ts.details)>.isModified()) {
		<if (\map() := ts.ds) {>assert <use(ts.details)>.hasReplacedValue(); 
			<dec(ts.valHash)> = 
				<toString(hashCodeExpr(
					ts, 
					eitherTypeAsSpecificTypeCast(
						exprFromString("<use(ts.details)>.getReplacedValue()"), 					
						__new__internalPayloadTupleTypes__(ts)[1],					 
						content(ts, ctPayloadArg(1, isRare = op.isRare)).\type)))>;
		<}>
		rootNode = newRootNode;
		<toString(expressionStatement(assign(ts.hashCodeProperty, updateProperty(ts, op, hashCodeProperty(), onRemove(), tupleHashesNew = cutToTupleSize(ts, [ useExpr(ts.keyHash), useExpr(ts.valHash) ])))))>
		<toString(expressionStatement(assign(ts.sizeProperty, updateProperty(ts, op, sizeProperty(), onRemove()))))>
	
		if (DEBUG) {
			assert checkHashCodeAndSize(hashCode, cachedSize);
		}
		return <eval(resultOf(ts, artifact, op, onRemove(), payloadTupleExprList = cutToTupleSize(ts, [ useExpr(content(ts, ctPayloadArg(0, isRare = op.isRare))), replacedValueExpr ])))>;
	}

	if (DEBUG) {
		assert checkHashCodeAndSize(hashCode, cachedSize);
	}
	
	return <toString(resultOf(ts, artifact, op, onRemoveNotFound()))>;"
when replacedValueExpr := exprFromString("<use(ts.details)>.getReplacedValue()")
		&& rootNode := jdtToVal(abstractNode(ts), "rootNode");
		
Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:removeTuple(), onRemove(), list[Expression] payloadTupleExprList = []) 
	= payloadTupleExprList[1]
when \map(multi = false) := ts.ds;

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:removeTuple(), onRemove(), list[Expression] payloadTupleExprList = []) 
	= boolean(true)
when !(\map(multi = false) := ts.ds);

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:removeTuple(), onRemoveNotFound(), list[Expression] payloadTupleExprList = []) 
	= NULL()
when \map(multi = false) := ts.ds;

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:removeTuple(), onRemoveNotFound(), list[Expression] payloadTupleExprList = []) 
	= boolean(false)
when !(\map(multi = false) := ts.ds);


Argument typeDependentEntryOfCollection(Argument collection, str tupleName) 
	= val(specific("Map.Entry", typeArguments = collection.\type.typeArguments), tupleName)
when specific(_, typeArguments = args) := collection.\type; 

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertCollection(),
		Argument transientColl = exactBoundCollectionArg(ts, transient()),
		Argument entry = typeDependentEntryOfCollection(transientColl, "entry")) = true; 
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertCollection(),
		Argument transientColl = exactBoundCollectionArg(ts, transient()),
		Argument entry = typeDependentEntryOfCollection(transientColl, "entry")) = 
	"boolean modified = false;

	for (Map.Entry<GenericsExpandedUpperBoundedStr(ts)> entry : <uncapitalize(toString(ts.ds))>.entrySet()) {
		<if(\map(multi = false) := ts.ds) {>final boolean isPresent = <toString(
			call(this(), getDef(ts, artifact, containsKey(customComparator = op.customComparator)), 
					labeledArgsOverride = (payloadTuple(): unboxPayloadFromTuple(ts, entry)[0])))>;
		<dec(content(ts, ctCollectionArg(1), "replaced"))> = <toString(call(this(), getDef(ts, artifact, insertTuple(false, op.customComparator)), 
																			labeledArgsOverride = (payloadTuple(): compoundExpr(unboxPayloadFromTuple(ts, entry)))))>;
		
		if (!isPresent || replaced != null) {
			modified = true;
		}<} else {>modified |= <toString(call(this(), getDef(ts, artifact, insertTuple(op.isRare, op.customComparator)), 
																			labeledArgsOverride = (payloadTuple(): compoundExpr(unboxPayloadFromTuple(ts, entry)))))>;<}>
	}

	return modified;"
when \map() := ts.ds;

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertCollection()) = true; 

// TODO: how to do a batch insert in a heterogeneous case?
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertCollection()) = 
	"boolean modified = false;

	for (<dec(key(ts.keyType))> : <uncapitalize(toString(ts.ds))>) {
		modified |= <toString(call(this(), getDef(ts, artifact, insertTuple(false, op.customComparator))))>;
	}
		
	return modified;"
when \set() := ts.ds;