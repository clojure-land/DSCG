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
module dscg::GenerateTrie_Core

import List;
import String;
extend dscg::Common;
extend dscg::Common_Iterator;
import dscg::GenerateTrie_Core_Common;

str generateCoreClassString(ts, str innerClassesString) {
	
	//TrieSpecifics ts = setArtifact(tsSuper, core(immutable()));
	
	str emptyCollectionConstantName = "EMPTY_<toUpperCase(toString(ts.ds))>";
	
	Argument rootNode = jdtToVal(abstractNode(ts), "rootNode");
	
	Argument immutableResult = jdtToVal(immutableInterface(ts), "result");
	Argument transientResult = jdtToVal(transientInterface(ts), "result");
	
return 
"/*******************************************************************************
 * Copyright (c) 2013-2015 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 *******************************************************************************/
package <targetBasePackage>;

import static <targetBasePackage>.AbstractSpecialisedImmutableMap.entryOf;

import java.lang.reflect.Field;
import java.text.DecimalFormat;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.AbstractCollection;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.List;
import java.util.LinkedList;
import java.util.Objects;
import java.util.ArrayList;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;


@SuppressWarnings(\"rawtypes\")
public class <ts.coreClassName><GenericsStr(ts.tupleTypes)> implements Immutable<toString(ts.ds)><CollectionGenericsExpandedStr(ts)> {

	<impl(ts, core(immutable()), emptyTrieNodeConstant())>

	<toString(UNCHECKED_ANNOTATION())>
	private static final <ts.coreClassName> <emptyCollectionConstantName> = new <ts.coreClassName>(<toString(call(getDef(ts, core(unknownUpdateSemantic()), emptyTrieNodeConstant())))>, 0, 0);

	private static final boolean DEBUG = false;

	private final <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> rootNode;
	private final int hashCode;
	private final int cachedSize;

	<ts.coreClassName>(<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> rootNode, int hashCode, int cachedSize) {
		this.rootNode = rootNode;
		this.hashCode = hashCode;
		this.cachedSize = cachedSize;
		if (DEBUG) {
			assert checkHashCodeAndSize(hashCode, cachedSize);
		}
	}

	<toString(UNCHECKED_ANNOTATION())>
	public static final <GenericsStr(ts.tupleTypes)> Immutable<toString(ts.ds)><CollectionGenericsExpandedStr(ts)> of() {
		return <ts.coreClassName>.<emptyCollectionConstantName>;
	}

	<if (\map() := ts.ds) {>
	<toString(UNCHECKED_ANNOTATION())>
	public static final <GenericsStr(ts.tupleTypes)> Immutable<toString(ts.ds)><CollectionGenericsExpandedStr(ts)> of(Object... keyValuePairs) {
		if (keyValuePairs.length % 2 != 0) {
			throw new IllegalArgumentException(\"Length of argument list is uneven: no key/value pairs.\");
		}

		<dec(asVar(immutableResult))> = <ts.coreClassName>.<emptyCollectionConstantName>; 

		for (int i = 0; i \< keyValuePairs.length; i += 2) {
			<dec(key(ts.keyType))> = (<typeToString(ts.keyType)>) keyValuePairs[i];
			<dec(val(ts.valType))> = (<typeToString(ts.valType)>) keyValuePairs[i + 1];

			<use(immutableResult)> = <toString(call(immutableResult, getDef(ts, core(immutable()), insertTuple(false, false))))>;
		}

		return <use(immutableResult)>;
	}
	<}>
	
	<if (ts.ds == \set()) {>
	<toString(UNCHECKED_ANNOTATION())>
	public static final <GenericsStr(ts.tupleTypes)> Immutable<toString(ts.ds)><CollectionGenericsExpandedStr(ts)> of(<typeToString(ts.keyType)>... keys) {
		<dec(asVar(immutableResult))> = <ts.coreClassName>.<emptyCollectionConstantName>;

		for (<dec(key(ts.keyType))> : keys) {
			<use(immutableResult)> = <toString(call(immutableResult, getDef(ts, core(immutable()), insertTuple(false, false))))>;
		}

		return result;
	}
	<}>
	
	<toString(UNCHECKED_ANNOTATION())>
	public static final <GenericsStr(ts.tupleTypes)> Transient<toString(ts.ds)><CollectionGenericsExpandedStr(ts)> transientOf() {
		return <ts.coreClassName>.<emptyCollectionConstantName>.asTransient();
	}

	<if (\map() := ts.ds) {>
	<toString(UNCHECKED_ANNOTATION())>
	public static final <GenericsStr(ts.tupleTypes)> Transient<toString(ts.ds)><CollectionGenericsExpandedStr(ts)> transientOf(Object... keyValuePairs) {
		if (keyValuePairs.length % 2 != 0) {
			throw new IllegalArgumentException(
							\"Length of argument list is uneven: no key/value pairs.\");
		}

		<dec(transientResult)> = <ts.coreClassName>.<emptyCollectionConstantName>.asTransient();

		for (int i = 0; i \< keyValuePairs.length; i += 2) {
			<dec(key(ts.keyType))> = (<typeToString(ts.keyType)>) keyValuePairs[i];
			<dec(val(ts.valType))> = (<typeToString(ts.valType)>) keyValuePairs[i + 1];

			<toString(call(transientResult, getDef(ts, core(transient()), insertTuple(false, false))))>;			
		}

		return <use(transientResult)>;
	}
	<}>
	
	<if (ts.ds == \set()) {>
	<toString(UNCHECKED_ANNOTATION())>
	public static final <GenericsStr(ts.tupleTypes)> Transient<toString(ts.ds)><CollectionGenericsExpandedStr(ts)> transientOf(<typeToString(ts.keyType)>... keys) {
		final Transient<toString(ts.ds)><CollectionGenericsExpandedStr(ts)> result = <ts.coreClassName>.<emptyCollectionConstantName>.asTransient();

		for (<dec(key(ts.keyType))> : keys) {
			<toString(call(transientResult, getDef(ts, core(transient()), insertTuple(false, false))))>;
		}

		return result;
	}
	<}>

	<generate_checkHashCodeAndSize(ts)>
	
	<impl(ts, core(immutable()), transformHashCode())>

	<impl(ts, core(immutable()), containsKey(isRare = false, customComparator = false))>
	<impl(ts, core(immutable()), containsKey(isRare = false, customComparator = true))>
	<impl(ts, core(immutable()), containsKey(isRare = true, customComparator = false))>
	<impl(ts, core(immutable()), containsKey(isRare = true, customComparator = true))>	

	<impl(ts, core(immutable()), containsValue())>
	<impl(ts, core(immutable()), containsValue(customComparator = true))>

	<impl(ts, core(immutable()), containsEntry())>
	<impl(ts, core(immutable()), containsEntry(customComparator = true))>

	<impl(ts, core(immutable()), get(isRare = false, customComparator = false))>
	<impl(ts, core(immutable()), get(isRare = false, customComparator = true))>

	<impl(ts, core(immutable()), insertTuple(false, false))>
	<impl(ts, core(immutable()), insertTuple(false, true))>
	<impl(ts, core(immutable()), insertTuple(true, false))>
	<impl(ts, core(immutable()), insertTuple(true, true))>	

	<impl(ts, core(immutable()), insertCollection())>
	<impl(ts, core(immutable()), insertCollection(customComparator = true))>
	
	<impl(ts, core(immutable()), removeTuple(isRare = false, customComparator = false))>
	<impl(ts, core(immutable()), removeTuple(isRare = false, customComparator = true))>
	<impl(ts, core(immutable()), removeTuple(isRare = true, customComparator = false))>
	<impl(ts, core(immutable()), removeTuple(isRare = true, customComparator = true))>	

	<impl(ts, core(immutable()), removeCollection())>
	<impl(ts, core(immutable()), removeCollection(customComparator = true))>

	<impl(ts, core(immutable()), retainCollection())>
	<impl(ts, core(immutable()), retainCollection(customComparator = true))>
	



	<impl(ts, core(immutable()), put())>
	<impl(ts, core(immutable()), putAll())>

	<impl(ts, core(immutable()), add())>
	<impl(ts, core(immutable()), addAll())>

	<impl(ts, core(immutable()), clear())>
	<impl(ts, core(immutable()), remove())>
	
	<impl(ts, core(immutable()), removeAll())>
	<impl(ts, core(immutable()), retainAll())>




	<implOrOverride(getDef(ts, core(immutable()), containsAll()), 	
		"for (Object item : c) {
			if (!contains(item)) {
				return false;
			}
		}
		return true;")>

	<implOrOverride(getDef(ts, core(immutable()), containsAll(customComparator = true)),	
		"for (Object item : c) {
			if (!containsEquivalent(item, cmp)) {
				return false;
			}
		}
		return true;")>

	<impl(ts, core(immutable()), size())>
	
	<impl(ts, core(immutable()), isEmpty())>
	
	<impl(ts, core(immutable()), iterator())>
	
	<impl(ts, core(immutable()), keyIterator())>

	<impl(ts, core(immutable()), valueIterator())>

	<impl(ts, core(immutable()), entryIterator())>
	
	<impl(ts, core(immutable()), tupleIterator())>

	<impl(ts, core(immutable()), valueCollectionsSpliterator())>

	<impl(ts, core(immutable()), valueCollectionsStream())>

	<implOrOverride(getDef(ts, core(immutable()), keySet()), 
		generate_bodyOf_jul_Map_keySet(ts, ts.coreClassName))>
		
	<implOrOverride(getDef(ts, core(immutable()), values()), 
		generate_bodyOf_jul_Map_values(ts, ts.coreClassName))>

	<implOrOverride(getDef(ts, core(immutable()), entrySet()), 
		generate_bodyOf_jul_Map_entrySet(ts, ts.coreClassName))>

	<implOrOverride(getDef(ts, core(immutable()), toObjectArray()), 
		generate_bodyOf_jul_Collection_toObjectArray(ts))>

	<implOrOverride(getDef(ts, core(immutable()), toGenericArray()), 
		generate_bodyOf_jul_Collection_toGenericArray(ts))>

	<implOrOverride(getDef(ts, core(immutable()), PredefOp::equals()), 
		generate_bodyOf_CoreCommon_equals(ts, ts.coreClassName))>

	<implOrOverride(getDef(ts, core(immutable()), PredefOp::hashCode()), 
		"return hashCode;")>

	@Override
	public boolean isTransientSupported() {
		return <isOptionEnabled(ts, useStagedMutability())>; 
	}

	@Override
	public Transient<toString(ts.ds)><CollectionGenericsExpandedStr(ts)> asTransient() {
		<if (isOptionEnabled(ts, useStagedMutability())) {> return new Transient<ts.coreClassName><GenericsStr(ts.tupleTypes)>(this); <} else {> <toString(UNSUPPORTED_OPERATION_EXCEPTION)> <}>		
	}

	<if (false) {>
	// <!isOptionEnabled(setup, useStructuralEquality()) && \map() := ts.ds>

	@Override
	public boolean equals(Object other) {
		if (other == this)
			return true;
		if (other == null)
			return false;

		if (other instanceof TrieMap_BleedingEdge) {
			TrieMap_BleedingEdge that = (TrieMap_BleedingEdge) other;

			if (this.size() != that.size())
				return false;

			SupplierIterator it1 = this.keyIterator();
			SupplierIterator it2 = that.keyIterator();

			boolean result = true;
			while (it1.hasNext() && it2.hasNext() && (result = it1.next().equals(it2.next()))
							&& (result = it1.get().equals(it2.get())))
				;

			assert !result || !it1.hasNext(); // result =\> !it1.hasNext
			assert !result || !it2.hasNext(); // result =\> !it2.hasNext

			return result;
		} else if (other instanceof Map) {
			throw new UnsupportedOperationException(\"TODO\");
		}		

		return false;
	}	
	<}>

	<if (false) {>
	// <!isOptionEnabled(setup, useStructuralEquality()) && ts.ds == \set()>
		
	@Override
	public boolean equals(Object other) {
		if (other == this)
			return true;
		if (other == null)
			return false;

		if (other instanceof TrieSet_BleedingEdge) {
			TrieSet_BleedingEdge that = (TrieSet_BleedingEdge) other;

			if (this.size() != that.size())
				return false;

			Iterator it1 = this.iterator();
			Iterator it2 = that.iterator();

			boolean result = true;
			while (it1.hasNext() && it2.hasNext() && (result = it1.next().equals(it2.next())))
				;

			assert !result || !it1.hasNext(); // result =\> !it1.hasNext
			assert !result || !it2.hasNext(); // result =\> !it2.hasNext

			return result;
		} else if (other instanceof Set) {
			throw new UnsupportedOperationException(\"TODO\");
		}

		return false;
	}
	<}>
	
	/*
	 * For analysis purposes only.
	 */
	protected <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> getRootNode() {
		return rootNode;
	}

	/*
	 * For analysis purposes only.
	 */
	protected Iterator\<<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)>\> nodeIterator() {
		return new <ts.nodeIteratorClassName><InferredGenerics(ts.ds, ts.tupleTypes)>(rootNode);
	}

	/*
	 * For analysis purposes only.
	 */
	protected int getNodeCount() {
		final Iterator\<<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)>\> it = nodeIterator();
		int sumNodes = 0;

		for (; it.hasNext(); it.next()) {
			sumNodes += 1;
		}

		return sumNodes;
	}

	/*
	 * For analysis purposes only.
	 */
	protected int[][] arityCombinationsHistogram() {
		final Iterator\<<AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)>\> it = nodeIterator();
		final int[][] sumArityCombinations = new int[<ts.nMax+1>][<ts.nMax+1>];

		while (it.hasNext()) {
			final <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> node = it.next();
			sumArityCombinations[node.payloadArity()][node.nodeArity()] += 1;
		}

		return sumArityCombinations;
	}

	/*
	 * For analysis purposes only.
	 */
	protected int[] arityHistogram() {
		final int[][] sumArityCombinations = arityCombinationsHistogram();
		final int[] sumArity = new int[<ts.nMax+1>];

		final int maxArity = <ts.nMax>; // TODO: factor out constant

		for (int j = 0; j \<= maxArity; j++) {
			for (int maxRestArity = maxArity - j, k = 0; k \<= maxRestArity - j; k++) {
				sumArity[j + k] += sumArityCombinations[j][k];
			}
		}

		return sumArity;
	}

	/*
	 * For analysis purposes only.
	 */
	public void printStatistics() {
		final int[][] sumArityCombinations = arityCombinationsHistogram();
		final int[] sumArity = arityHistogram();
		final int sumNodes = getNodeCount();

		final int[] cumsumArity = new int[<ts.nMax+1>];
		for (int cumsum = 0, i = 0; i \< <ts.nMax+1>; i++) {
			cumsum += sumArity[i];
			cumsumArity[i] = cumsum;
		}

		final float threshhold = 0.01f; // for printing results
		for (int i = 0; i \< <ts.nMax+1>; i++) {
			float arityPercentage = (float) (sumArity[i]) / sumNodes;
			float cumsumArityPercentage = (float) (cumsumArity[i]) / sumNodes;

			if (arityPercentage != 0 && arityPercentage \>= threshhold) {
				// details per level
				StringBuilder bldr = new StringBuilder();
				int max = i;
				for (int j = 0; j \<= max; j++) {
					for (int k = max - j; k \<= max - j; k++) {
						float arityCombinationsPercentage = (float) (sumArityCombinations[j][k])
										/ sumNodes;

						if (arityCombinationsPercentage != 0
										&& arityCombinationsPercentage \>= threshhold) {
							bldr.append(String.format(\"%d/%d: %s, \", j, k, new DecimalFormat(
											\"0.00%\").format(arityCombinationsPercentage)));
						}
					}
				}
				final String detailPercentages = bldr.toString();

				// overview
				System.out.println(String.format(\"%2d: %s\\t[cumsum = %s]\\t%s\", i,
								new DecimalFormat(\"0.00%\").format(arityPercentage),
								new DecimalFormat(\"0.00%\").format(cumsumArityPercentage),
								detailPercentages));
			}
		}
	}
	
	<innerClassesString>
		
	<if (isOptionEnabled(ts, useSunMiscUnsafe())) {>
	private abstract static class DataLayoutHelper<GenericsStr(ts.tupleTypes)> <extendsStr(compactNode(ts, compactNode(specializeByBitmap(true, true))))> {

		private static final long[] arrayOffsets = arrayOffsets(DataLayoutHelper.class,
						new String[] { \"slot0\", \"slot1\" });

		public final Object slot0 = null;

		public final Object slot1 = null;

		private DataLayoutHelper() {
			super(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0);
		}

	}
	<}>
		
}";
}

// TODO: extend JDT generation from model to other than 'trieNode' types
str __generateCoreClassString(ts, str innerClassesString) = generateCoreClassString(ts);

default str generateCoreClassString(TrieSpecifics ts) 
	= generateJdtString(ts, jdt, core(immutable()))
when jdt := coreJdt(ts, immutable(), modifierList = [ "private", "abstract", "static" ]);

lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, Artifact artifact:core(immutable())) 
	= [ <nodeType,method> | method <- declaredMethodsByCollection(ts)];

list[PredefOp] declaredMethodsByCollection(TrieSpecifics ts) = [
	
	emptyTrieNodeConstant(),
	
	containsKey(isRare = false, customComparator = false),
	containsKey(isRare = false, customComparator = true),
	containsKey(isRare = true, customComparator = false),
	containsKey(isRare = true, customComparator = true),	

	containsValue(),
	containsValue(customComparator = true),

	containsEntry(),
	containsEntry(customComparator = true),

	get(),
	get(customComparator = true),

	insertTuple(false, false),
	insertTuple(false, true),
	insertTuple(true, false),
	insertTuple(true, true),	

	insertCollection(),
	insertCollection(customComparator = true),
	
	removeTuple(),
	removeTuple(customComparator = true),

	removeCollection(),
	removeCollection(customComparator = true),

	retainCollection(),
	retainCollection(customComparator = true),

	put(),
	putAll(),

	add(),
	addAll(),

	clear(),
	remove(),
	
	removeAll(),
	retainAll(),

	size(),
	isEmpty(),	
	iterator(),
	keyIterator(),
	valueIterator(),
	entryIterator(),	
	tupleIterator(),
	valueCollectionsSpliterator(),
	valueCollectionsStream()	
	
];

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:removeTuple(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true; 
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:removeTuple(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"<dec(ts.keyHash)> = <hashCode(content(ts, ctPayloadArg(0, isRare = op.isRare)))>;
	'<dec(ts.details)> = <ts.ResultStr>.unchanged();
	
	<dec(\inode(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), removeTuple(customComparator = op.customComparator)), 
					argsOverride = (ts.mutator: NULL(), 
						ts.keyHash: call(getDef(ts, artifact, PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ts.keyHash))), 
						ts.shift: constant(ts.shift.\type, "0"))))>;
		
	if (<use(ts.details)>.isModified()) {
		<if (\map() := ts.ds) {>assert <use(ts.details)>.hasReplacedValue(); <dec(ts.valHash)> = <hashCode(content(ts, ctPayloadArg(1, isRare = op.isRare), "<use(ts.details)>.getReplacedValue()"))>;<}>return 
			new <ts.coreClassName><GenericsStr(ts.tupleTypes)>(newRootNode, 
				<eval(updateProperty(ts, op, hashCodeProperty(), onRemove(), tupleHashesNew = cutToTupleSize(ts, [ useExpr(ts.keyHash), useExpr(ts.valHash) ])))>, 
				<eval(updateProperty(ts, op, sizeProperty(), onRemove()))>);
	}

	return this;"
when rootNode := jdtToVal(abstractNode(ts), "rootNode");