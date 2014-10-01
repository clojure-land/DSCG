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
import dscg::Common;
import dscg::GenerateTrie_Core_Common;

str generateCoreClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str innerClassesString) {
	
	str emptyCollectionConstantName = "EMPTY_<toUpperCase(toString(ds))>";
	str emptyTrieNodeConstantName   = "EMPTY_NODE";
	
return 
"/*******************************************************************************
 * Copyright (c) 2013-2014 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 *******************************************************************************/
package org.eclipse.imp.pdb.facts.util;

import static org.eclipse.imp.pdb.facts.util.AbstractSpecialisedImmutableMap.entryOf;

import java.text.DecimalFormat;
import java.util.AbstractMap;
import java.util.AbstractSet;
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

@SuppressWarnings(\"rawtypes\")
public class <ts.coreClassName><Generics(ds)> extends AbstractImmutable<toString(ds)><GenericsExpanded(ds)> {

	@SuppressWarnings(\"unchecked\")
	private static final <ts.coreClassName> <emptyCollectionConstantName> = new <ts.coreClassName>(Compact<toString(ds)>Node.<emptyTrieNodeConstantName>, 0, 0);

	private static final boolean DEBUG = false;

	private final <AbstractNode(ds)><Generics(ds)> rootNode;
	private final int hashCode;
	private final int cachedSize;

	<ts.coreClassName>(<AbstractNode(ds)><Generics(ds)> rootNode, int hashCode, int cachedSize) {
		this.rootNode = rootNode;
		this.hashCode = hashCode;
		this.cachedSize = cachedSize;
		if (DEBUG) {
			assert checkHashCodeAndSize(hashCode, cachedSize);
		}
	}

	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> Immutable<toString(ds)><GenericsExpanded(ds)> of() {
		return <ts.coreClassName>.<emptyCollectionConstantName>;
	}

	<if (ds == \map()) {>
	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> Immutable<toString(ds)><GenericsExpanded(ds)> of(Object... keyValuePairs) {
		if (keyValuePairs.length % 2 != 0) {
			throw new IllegalArgumentException(\"Length of argument list is uneven: no key/value pairs.\");
		}

		Immutable<toString(ds)><GenericsExpanded(ds)> result = <ts.coreClassName>.<emptyCollectionConstantName>;

		for (int i = 0; i \< keyValuePairs.length; i += 2) {
			<dec(key())> = (<toString(key().\type)>) keyValuePairs[i];
			<dec(val())> = (<toString(val().\type)>) keyValuePairs[i + 1];

			result = result.<insertOrPutMethodName(ds)>(<use(ts.payloadTuple)>);
		}

		return result;
	}
	<}>
	
	<if (ds == \set()) {>
	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> Immutable<toString(ds)><GenericsExpanded(ds)> of(<toString(key().\type)>... keys) {
		Immutable<toString(ds)><GenericsExpanded(ds)> result = <ts.coreClassName>.<emptyCollectionConstantName>;

		for (<dec(key())> : keys) {
			result = result.<insertOrPutMethodName(ds)>(<use(key())>);
		}

		return result;
	}
	<}>
	
	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> Transient<toString(ds)><GenericsExpanded(ds)> transientOf() {
		return <ts.coreClassName>.<emptyCollectionConstantName>.asTransient();
	}

	<if (ds == \map()) {>
	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> Transient<toString(ds)><GenericsExpanded(ds)> transientOf(Object... keyValuePairs) {
		if (keyValuePairs.length % 2 != 0) {
			throw new IllegalArgumentException(
							\"Length of argument list is uneven: no key/value pairs.\");
		}

		final Transient<toString(ds)><GenericsExpanded(ds)> result = <ts.coreClassName>.<emptyCollectionConstantName>.asTransient();

		for (int i = 0; i \< keyValuePairs.length; i += 2) {
			<dec(key())> = (<toString(key().\type)>) keyValuePairs[i];
			<dec(val())> = (<toString(val().\type)>) keyValuePairs[i + 1];

			result.<insertOrPutMethodName(ds)>(<use(ts.payloadTuple)>);
		}

		return result;
	}
	<}>
	
	<if (ds == \set()) {>
	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> Transient<toString(ds)><GenericsExpanded(ds)> transientOf(<toString(key().\type)>... keys) {
		final Transient<toString(ds)><GenericsExpanded(ds)> result = <ts.coreClassName>.<emptyCollectionConstantName>.asTransient();

		for (<dec(key())> : keys) {
			result.<insertOrPutMethodName(ds)>(<use(key())>);
		}

		return result;
	}
	<}>

	<generate_checkHashCodeAndSize(ts, setup)>
	
	<implOrOverride(ts.Core_updated, 		generate_bodyOf_Core_updated(ts, setup, equalityDefaultForArguments		))>
	<implOrOverride(ts.Core_updatedEquiv,	generate_bodyOf_Core_updated(ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.Core_removed, 		generate_bodyOf_Core_removed(ts, setup, equalityDefaultForArguments		))>
	<implOrOverride(ts.Core_removedEquiv,	generate_bodyOf_Core_removed(ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.Core_containsKey, 		generate_bodyOf_Core_containsKey(ts, setup, equalityDefaultForArguments		))>
	<implOrOverride(ts.Core_containsKeyEquiv,	generate_bodyOf_Core_containsKey(ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.Core_containsValue, 		generate_bodyOf_Core_containsValue(ts, setup, equalityDefaultForArguments		))>
	<implOrOverride(ts.Core_containsValueEquiv,	generate_bodyOf_Core_containsValue(ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.Core_get, 		generate_bodyOf_Core_get(ts, setup, equalityDefaultForArguments		))>
	<implOrOverride(ts.Core_getEquiv,	generate_bodyOf_Core_get(ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.Core_insertOrPutAll, 		generate_bodyOf_Core_insertOrPutAll(ts, setup, equalityDefaultForArguments		))>
	<implOrOverride(ts.Core_insertOrPutAllEquiv,	generate_bodyOf_Core_insertOrPutAll(ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.Core_retainAll, 		generate_bodyOf_Core_retainAll(ts, setup, equalityDefaultForArguments		))>
	<implOrOverride(ts.Core_retainAllEquiv,	generate_bodyOf_Core_retainAll(ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.Core_removeAll, 		generate_bodyOf_Core_removeAll(ts, setup, equalityDefaultForArguments		))>
	<implOrOverride(ts.Core_removeAllEquiv,	generate_bodyOf_Core_removeAll(ts, setup, equalityComparatorForArguments	))>

	@Override
	public int size() {
		return cachedSize;
	}

	<if (ds == \set()) {>
	@Override
	public Iterator<GenericsExpanded(ds)> iterator() {
		return keyIterator();
	}
	<}>

	@Override
	public SupplierIterator<SupplierIteratorGenerics(ds)> keyIterator() {
		<generate_bodyOf_keyIterator(ds, setup)>
	}

	<if (ds == \map()) {>
	@Override
	public Iterator\<<toString(primitiveToClass(val().\type))>\> valueIterator() {
		return new <toString(ds)>ValueIterator<InferredGenerics()>(rootNode);
	}

	@Override
	public Iterator\<Map.Entry<GenericsExpanded(ds)>\> entryIterator() {
		return new <toString(ds)>EntryIterator<InferredGenerics()>(rootNode);
	}
	<}>

	<if (ds == \map()) {>
	@Override
	public Set\<java.util.Map.Entry<GenericsExpanded(ds)>\> entrySet() {
		Set\<java.util.Map.Entry<GenericsExpanded(ds)>\> entrySet = null;

		if (entrySet == null) {
			entrySet = new AbstractSet\<java.util.Map.Entry<GenericsExpanded(ds)>\>() {
				@Override
				public Iterator\<java.util.Map.Entry<GenericsExpanded(ds)>\> iterator() {
					return new Iterator\<Entry<GenericsExpanded(ds)>\>() {
						private final Iterator\<Entry<GenericsExpanded(ds)>\> i = entryIterator();

						@Override
						public boolean hasNext() {
							return i.hasNext();
						}

						@Override
						public Entry<GenericsExpanded(ds)> next() {
							return i.next();
						}

						@Override
						public void remove() {
							i.remove();
						}
					};
				}

				@Override
				public int size() {
					return <ts.coreClassName>.this.size();
				}

				@Override
				public boolean isEmpty() {
					return <ts.coreClassName>.this.isEmpty();
				}

				@SuppressWarnings(\"deprecation\")
				@Override
				public void clear() {
					<ts.coreClassName>.this.clear();
				}

				@Override
				public boolean contains(Object k) {
					return <ts.coreClassName>.this.containsKey(k);
				}
			};
		}
		return entrySet;
	}
	<}>

	@Override
	public boolean isTransientSupported() {
		return true;
	}

	@Override
	public Transient<toString(ds)><GenericsExpanded(ds)> asTransient() {
		return new Transient<ts.coreClassName><Generics(ds)>(this);
	}

	@Override
	public int hashCode() {
		return hashCode;
	}

	<if (isOptionEnabled(setup, useStructuralEquality())) {>
	@Override
	public boolean equals(Object other) {
		if (other == this) {
			return true;
		}
		if (other == null) {
			return false;
		}

		if (other instanceof <ts.coreClassName>) {
			<ts.coreClassName><QuestionMarkGenerics(ds)> that = (<ts.coreClassName><QuestionMarkGenerics(ds)>) other;

			if (this.size() != that.size()) {
				return false;
			}

			return rootNode.equals(that.rootNode);
		}

		return super.equals(other);
	}
	<}>

	/*
	 * For analysis purposes only.
	 */
	protected <AbstractNode(ds)><Generics(ds)> getRootNode() {
		return rootNode;
	}

	/*
	 * For analysis purposes only.
	 */
	protected Iterator\<<AbstractNode(ds)><Generics(ds)>\> nodeIterator() {
		return new <ts.nodeIteratorClassName><InferredGenerics()>(rootNode);
	}

	/*
	 * For analysis purposes only.
	 */
	protected int getNodeCount() {
		final Iterator\<<AbstractNode(ds)><Generics(ds)>\> it = nodeIterator();
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
		final Iterator\<<AbstractNode(ds)><Generics(ds)>\> it = nodeIterator();
		final int[][] sumArityCombinations = new int[<nMax+1>][<nMax+1>];

		while (it.hasNext()) {
			final <AbstractNode(ds)><Generics(ds)> node = it.next();
			sumArityCombinations[node.payloadArity()][node.nodeArity()] += 1;
		}

		return sumArityCombinations;
	}

	/*
	 * For analysis purposes only.
	 */
	protected int[] arityHistogram() {
		final int[][] sumArityCombinations = arityCombinationsHistogram();
		final int[] sumArity = new int[<nMax+1>];

		final int maxArity = <nMax>; // TODO: factor out constant

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

		final int[] cumsumArity = new int[<nMax+1>];
		for (int cumsum = 0, i = 0; i \< <nMax+1>; i++) {
			cumsum += sumArity[i];
			cumsumArity[i] = cumsum;
		}

		final float threshhold = 0.01f; // for printing results
		for (int i = 0; i \< <nMax+1>; i++) {
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

str generate_bodyOf_keyIterator(DataStructure ds, rel[Option,bool] setup:{_*, <useFixedStackIterator(),true>}) = 
	"return new <toString(ds)>KeyIterator<InferredGenerics()>(rootNode);"
	;
	
default str generate_bodyOf_keyIterator(DataStructure ds, rel[Option,bool] setup) =
	"return new <ts.coreClassName>Iterator<InferredGenerics()>((Compact<toString(ds)>Node<Generics(ds)>) rootNode);"
	;
	
	
default str generate_bodyOf_Core_updated(TrieSpecifics ts, rel[Option,bool] setup, str(Argument, Argument) eq) {

	str optionalComparatorArgument = "<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>";

	return  
	"	final int keyHash = key.hashCode();
		<dec(ts.details)> = Result.unchanged();
		
		<dec(\node(ts.ds, "newRootNode"))> = rootNode.updated(null, <use(ts.payloadTuple)>, keyHash, 0, <use(ts.details)><optionalComparatorArgument>);

		if (<use(ts.details)>.isModified()) {
			<if (ts.ds == \map()) {>
				if (<use(ts.details)>.hasReplacedValue()) {
					final int valHashOld = <use(ts.details)>.getReplacedValue().hashCode();
					final int valHashNew = <hashCode(val())>;
	
					return new <ts.coreClassName><Generics(ts.ds)>(newRootNode, hashCode + (keyHash ^ valHashNew)
									- (keyHash ^ valHashOld), cachedSize);
				}
			<}>
				
			<if (ts.ds == \map()) {>
				final int valHash = <hashCode(val())>;
				return new <ts.coreClassName><Generics(ts.ds)>(newRootNode, hashCode + (keyHash ^ valHash), cachedSize + 1);
			<} else {>
				return new <ts.coreClassName><Generics(ts.ds)>(newRootNode, hashCode + keyHash, cachedSize + 1);			
			<}>
		}

		return this;"
	;
}

default str generate_bodyOf_Core_removed(TrieSpecifics ts, rel[Option,bool] setup, str(Argument, Argument) eq, 
				str optionalComparatorArgument = "<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>") =
	"
	final int keyHash = key.hashCode();
	<dec(ts.details)> = Result.unchanged();
	
	<dec(\node(ts.ds, "newRootNode"))> = rootNode.removed(null, key, keyHash, 0, <use(ts.details)><optionalComparatorArgument>);

	if (<use(ts.details)>.isModified()) {
		<if (ts.ds == \set()) {>
			return new <ts.coreClassName><Generics(ts.ds)>(newRootNode, hashCode - keyHash, cachedSize - 1);
		<} else {>
			assert <use(ts.details)>.hasReplacedValue();
			final int valHash = <use(ts.details)>.getReplacedValue().hashCode();

			return new <ts.coreClassName><Generics(ts.ds)>(newRootNode, hashCode - (keyHash ^ valHash), cachedSize - 1);
		<}>
	}

	return this;
	"
	;

default str generate_bodyOf_Core_containsKey(TrieSpecifics ts, rel[Option,bool] setup, str(Argument, Argument) eq,
				str optionalComparatorArgument = "<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>") =
	"
		try {
			<dec(key())> = (<toString(key().\type)>) o;
			return rootNode.containsKey(<use(key())>, <hashCode(key())>, 0<optionalComparatorArgument>);			
		} catch (ClassCastException unused) {
			return false;
		}

	"
	;

default str generate_bodyOf_Core_containsValue(TrieSpecifics ts, rel[Option,bool] setup, str(Argument, Argument) eq,
				str optionalComparatorArgument = "<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>") =
	"
		for (Iterator\<<toString(primitiveToClass(val().\type))>\> iterator = valueIterator(); iterator.hasNext();) {
			if (iterator.next().equals(o)) {
				return true;
			}
		}
		return false;
	"
	;

default str generate_bodyOf_Core_get(TrieSpecifics ts, rel[Option,bool] setup, str(Argument, Argument) eq,
				str optionalComparatorArgument = "<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>") =
	"
		try {
			<dec(key())> = (<toString(key().\type)>) o;
			final Optional<MapsToGenerics(ts.ds)> result = rootNode.findByKey(<use(key())>, <hashCode(key())>, 0<optionalComparatorArgument>);
	
			if (result.isPresent()) {
				return result.get();
			} else {
				return null;
			}			
		} catch (ClassCastException unused) {
			return null;
		}
	"
	;	
	
default str generate_bodyOf_Core_insertOrPutAll(TrieSpecifics ts, rel[Option,bool] setup, str(Argument, Argument) eq,
				str optionalComparatorArgument = "<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>",
				str optionalEquivalentPostfix = "<if (!(eq == equalityDefaultForArguments)) {>Equivalent<}>") =

	"	Transient<toString(ts.ds)><GenericsExpanded(ts.ds)> tmp = asTransient();
		tmp.<insertOrPutMethodName(ts.ds)>All<optionalEquivalentPostfix>(<uncapitalize(toString(ts.ds))><optionalComparatorArgument>);
		return tmp.freeze();"
	;		

default str generate_bodyOf_Core_retainAll(TrieSpecifics ts, rel[Option,bool] setup, str(Argument, Argument) eq,
				str optionalComparatorArgument = "<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>",
				str optionalEquivalentPostfix = "<if (!(eq == equalityDefaultForArguments)) {>Equivalent<}>") =

	"	Transient<toString(ts.ds)><GenericsExpanded(ts.ds)> tmp = asTransient();
		tmp.__retainAll<optionalEquivalentPostfix>(<uncapitalize(toString(ts.ds))><optionalComparatorArgument>);
		return tmp.freeze();"
	;		

default str generate_bodyOf_Core_removeAll(TrieSpecifics ts, rel[Option,bool] setup, str(Argument, Argument) eq,
				str optionalComparatorArgument = "<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>",
				str optionalEquivalentPostfix = "<if (!(eq == equalityDefaultForArguments)) {>Equivalent<}>") =

	"	Transient<toString(ts.ds)><GenericsExpanded(ts.ds)> tmp = asTransient();
		tmp.__removeAll<optionalEquivalentPostfix>(<uncapitalize(toString(ts.ds))><optionalComparatorArgument>);
		return tmp.freeze();"
	;		
