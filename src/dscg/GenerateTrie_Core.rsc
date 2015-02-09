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
import dscg::GenerateTrie_Core_Common;

str generateCoreClassString(tsSuper, rel[Option,bool] setup, str innerClassesString) {
	
	TrieSpecifics ts = setArtifact(tsSuper, core(immutable()));
	
	str emptyCollectionConstantName = "EMPTY_<toUpperCase(toString(ts.ds))>";
	str emptyTrieNodeConstantName   = "EMPTY_NODE";
	
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

import java.text.DecimalFormat;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.AbstractCollection;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.AbstractCollection;
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
import java.util.ArrayList;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;


@SuppressWarnings(\"rawtypes\")
public class <ts.coreClassName><GenericsStr(ts.tupleTypes)> implements Immutable<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> {

	<toString(UNCHECKED_ANNOTATION())>
	private static final <ts.coreClassName> <emptyCollectionConstantName> = new <ts.coreClassName>(Compact<toString(ts.ds)>Node.<emptyTrieNodeConstantName>, 0, 0);

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
	public static final <GenericsStr(ts.tupleTypes)> Immutable<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> of() {
		return <ts.coreClassName>.<emptyCollectionConstantName>;
	}

	<if (\map() := ts.ds) {>
	<toString(UNCHECKED_ANNOTATION())>
	public static final <GenericsStr(ts.tupleTypes)> Immutable<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> of(Object... keyValuePairs) {
		if (keyValuePairs.length % 2 != 0) {
			throw new IllegalArgumentException(\"Length of argument list is uneven: no key/value pairs.\");
		}

		Immutable<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> result = <ts.coreClassName>.<emptyCollectionConstantName>;

		for (int i = 0; i \< keyValuePairs.length; i += 2) {
			<dec(key(ts.keyType))> = (<typeToString(ts.keyType)>) keyValuePairs[i];
			<dec(val(ts.valType))> = (<typeToString(ts.valType)>) keyValuePairs[i + 1];

			result = result.<toString(call(getDef(ts, insertTuple())))>;		
		}

		return result;
	}
	<}>
	
	<if (ts.ds == \set()) {>
	<toString(UNCHECKED_ANNOTATION())>
	public static final <GenericsStr(ts.tupleTypes)> Immutable<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> of(<typeToString(ts.keyType)>... keys) {
		Immutable<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> result = <ts.coreClassName>.<emptyCollectionConstantName>;

		for (<dec(key(ts.keyType))> : keys) {
			result = result.<insertTupleMethodName(ts.ds, ts.artifact)>(<use(key(ts.keyType))>);
		}

		return result;
	}
	<}>
	
	<toString(UNCHECKED_ANNOTATION())>
	public static final <GenericsStr(ts.tupleTypes)> Transient<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> transientOf() {
		return <ts.coreClassName>.<emptyCollectionConstantName>.asTransient();
	}

	<if (\map() := ts.ds) {>
	<toString(UNCHECKED_ANNOTATION())>
	public static final <GenericsStr(ts.tupleTypes)> Transient<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> transientOf(Object... keyValuePairs) {
		if (keyValuePairs.length % 2 != 0) {
			throw new IllegalArgumentException(
							\"Length of argument list is uneven: no key/value pairs.\");
		}

		final Transient<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> result = <ts.coreClassName>.<emptyCollectionConstantName>.asTransient();

		for (int i = 0; i \< keyValuePairs.length; i += 2) {
			<dec(key(ts.keyType))> = (<typeToString(ts.keyType)>) keyValuePairs[i];
			<dec(val(ts.valType))> = (<typeToString(ts.valType)>) keyValuePairs[i + 1];

			result.<toString(call(getDef(ts, insertTuple())))>;		
		}

		return result;
	}
	<}>
	
	<if (ts.ds == \set()) {>
	<toString(UNCHECKED_ANNOTATION())>
	public static final <GenericsStr(ts.tupleTypes)> Transient<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> transientOf(<typeToString(ts.keyType)>... keys) {
		final Transient<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> result = <ts.coreClassName>.<emptyCollectionConstantName>.asTransient();

		for (<dec(key(ts.keyType))> : keys) {
			result.<insertTupleMethodName(ts.ds, ts.artifact)>(<use(key(ts.keyType))>);
		}

		return result;
	}
	<}>

	<generate_checkHashCodeAndSize(ts, setup)>
	
	private static int improve(final int hash) {
		return hash; // return idendity
	}

	<impl(ts, containsKey())>
	<impl(ts, containsKey(customComparator = true))>

	<impl(ts, containsValue())>
	<impl(ts, containsValue(customComparator = true))>

	<impl(ts, containsEntry())>
	<impl(ts, containsEntry(customComparator = true))>

	<impl(ts, get())>
	<impl(ts, get(customComparator = true))>

	<impl(ts, insertTuple())>
	<impl(ts, insertTuple(customComparator = true))>

	<impl(ts, insertCollection())>
	<impl(ts, insertCollection(customComparator = true))>
	
	<impl(ts, removeTuple())>
	<impl(ts, removeTuple(customComparator = true))>

	<impl(ts, removeCollection())>
	<impl(ts, removeCollection(customComparator = true))>

	<impl(ts, retainCollection())>
	<impl(ts, retainCollection(customComparator = true))>
	




	<implOrOverride(ts.jul_Map_put, UNSUPPORTED_OPERATION_EXCEPTION)>	
	<implOrOverride(ts.jul_Map_clear, UNSUPPORTED_OPERATION_EXCEPTION)>
	<implOrOverride(ts.jul_Map_remove, UNSUPPORTED_OPERATION_EXCEPTION)>
	<implOrOverride(ts.jul_Map_putAll, UNSUPPORTED_OPERATION_EXCEPTION)>
	
	<implOrOverride(ts.Multimap_remove, UNSUPPORTED_OPERATION_EXCEPTION)>
	
	<implOrOverride(ts.jul_Set_add, UNSUPPORTED_OPERATION_EXCEPTION)>	
	<implOrOverride(ts.jul_Set_clear, UNSUPPORTED_OPERATION_EXCEPTION)>
	<implOrOverride(ts.jul_Set_remove, UNSUPPORTED_OPERATION_EXCEPTION)>
	<implOrOverride(ts.jul_Set_addAll, UNSUPPORTED_OPERATION_EXCEPTION)>
	<implOrOverride(ts.jul_Set_removeAll, UNSUPPORTED_OPERATION_EXCEPTION)>
	<implOrOverride(ts.jul_Set_retainAll, UNSUPPORTED_OPERATION_EXCEPTION)>

	<implOrOverride(ts.jul_Set_containsAll, 	
		"for (Object item : c) {
			if (!contains(item)) {
				return false;
			}
		}
		return true;")>

	<implOrOverride(ts.jul_Set_containsAllEquivalent,	
		"for (Object item : c) {
			if (!containsEquivalent(item, cmp)) {
				return false;
			}
		}
		return true;")>


	<implOrOverride(ts.CoreCommon_size,
		"return cachedSize;")>

	<implOrOverride(ts.CoreCommon_isEmpty,
		"return cachedSize == 0;")>
	
	<impl(ts, iterator())>
	
	<impl(ts, keyIterator())>

	<impl(ts, valueIterator())>

	<impl(ts, entryIterator())>
	
	<impl(ts, tupleIterator())>

	<impl(ts, valueCollectionsSpliterator())>

	<impl(ts, valueCollectionsStream())>

	<implOrOverride(ts.jul_Map_keySet, generate_bodyOf_jul_Map_keySet(ts, ts.coreClassName))>
		
	<implOrOverride(ts.jul_Map_values, generate_bodyOf_jul_Map_values(ts, ts.coreClassName))>

	<implOrOverride(ts.jul_Map_entrySet, generate_bodyOf_jul_Map_entrySet(ts, ts.coreClassName))>

	<implOrOverride(ts.jul_Collection_toObjectArray, generate_bodyOf_jul_Collection_toObjectArray(ts))>

	<implOrOverride(ts.jul_Collection_toGenericArray, generate_bodyOf_jul_Collection_toGenericArray(ts))>

	<implOrOverride(ts.CoreCommon_equals, generate_bodyOf_CoreCommon_equals(ts, ts.coreTransientClassName))>

	@Override
	public int hashCode() {
		return hashCode;
	}

	@Override
	public boolean isTransientSupported() {
		return <isOptionEnabled(ts.setup, useStagedMutability())>; 
	}

	@Override
	public Transient<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> asTransient() {
		<if (isOptionEnabled(ts.setup, useStagedMutability())) {> return new Transient<ts.coreClassName><GenericsStr(ts.tupleTypes)>(this); <} else {> <toString(UNSUPPORTED_OPERATION_EXCEPTION)> <}>		
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
	 * For analysis purposes only. Payload X Node
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
		
}";
}

default str generate_bodyOf_Core_get(TrieSpecifics ts, rel[Option,bool] setup, str(Argument, Argument) eq,
				str optionalComparatorArgument = "<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>") =
	"	try {
			<toString(UNCHECKED_ANNOTATION())>
			<dec(key(ts.keyType))> = (<typeToString(ts.keyType)>) o;
			final Optional<MapsToGenerics(ts.ds, ts.tupleTypes)> result = rootNode.findByKey(<use(key(ts.keyType))>, improve(<hashCode(key(ts.keyType))>), 0<optionalComparatorArgument>);
	
			if (result.isPresent()) {
				return result.get();
			} else {
				return null;
			}			
		} catch (ClassCastException unused) {
			return null;
		}"
	;		

default str generate_bodyOf_Core_retainAll(TrieSpecifics ts, rel[Option,bool] setup, str(Argument, Argument) eq,
				str optionalComparatorArgument = "<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>",
				str optionalEquivalentPostfix = "<if (!(eq == equalityDefaultForArguments)) {>Equivalent<}>") =

	"	Transient<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> tmp = asTransient();
		tmp.__retainAll<optionalEquivalentPostfix>(<uncapitalize(toString(ts.ds))><optionalComparatorArgument>);
		return tmp.freeze();"
	;		
