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

import dscg::Common;

str generateCoreClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str innerClassesString, str classNamePostfix) { 

	str coreClassName = "Trie<toString(ds)><classNamePostfix>";
	str nodeIteratorClassName = "Trie<toString(ds)><classNamePostfix>NodeIterator";

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
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

@SuppressWarnings(\"rawtypes\")
public class <coreClassName><Generics(ds)> extends AbstractImmutableMap<GenericsExpanded(ds)> {

	@SuppressWarnings(\"unchecked\")
	private static final <coreClassName> EMPTY_INPLACE_INDEX_MAP = new <coreClassName>(CompactMapNode.EMPTY_INPLACE_INDEX_NODE, 0, 0);

	private static final boolean DEBUG = false;

	private final <AbstractNode(ds)><Generics(ds)> rootNode;
	private final int hashCode;
	private final int cachedSize;

	<coreClassName>(<AbstractNode(ds)><Generics(ds)> rootNode, int hashCode, int cachedSize) {
		this.rootNode = rootNode;
		this.hashCode = hashCode;
		this.cachedSize = cachedSize;
		if (DEBUG) {
			assert invariant();
		}
	}

	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> ImmutableMap<GenericsExpanded(ds)> of() {
		return <coreClassName>.EMPTY_INPLACE_INDEX_MAP;
	}

	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> ImmutableMap<GenericsExpanded(ds)> of(Object... keyValuePairs) {
		if (keyValuePairs.length % 2 != 0) {
			throw new IllegalArgumentException(\"Length of argument list is uneven: no key/value pairs.\");
		}

		ImmutableMap<GenericsExpanded(ds)> result = <coreClassName>.EMPTY_INPLACE_INDEX_MAP;

		for (int i = 0; i \< keyValuePairs.length; i += 2) {
			<dec(key())> = (<key().\type>) keyValuePairs[i];
			<dec(val())> = (<val().\type>) keyValuePairs[i + 1];

			result = result.__put(key, val);
		}

		return result;
	}

	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> TransientMap<GenericsExpanded(ds)> transientOf() {
		return <coreClassName>.EMPTY_INPLACE_INDEX_MAP.asTransient();
	}

	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> TransientMap<GenericsExpanded(ds)> transientOf(Object... keyValuePairs) {
		if (keyValuePairs.length % 2 != 0) {
			throw new IllegalArgumentException(
							\"Length of argument list is uneven: no key/value pairs.\");
		}

		final TransientMap<GenericsExpanded(ds)> result = <coreClassName>.EMPTY_INPLACE_INDEX_MAP.asTransient();

		for (int i = 0; i \< keyValuePairs.length; i += 2) {
			<dec(key())> = (<key().\type>) keyValuePairs[i];
			<dec(val())> = (<val().\type>) keyValuePairs[i + 1];

			result.__put(key, val);
		}

		return result;
	}

	private boolean invariant() {
		int _hash = 0;
		int _count = 0;

		for (Iterator\<Map.Entry<GenericsExpanded(ds)>\> it = entryIterator(); it.hasNext();) {
			final Map.Entry<GenericsExpanded(ds)> entry = it.next();

			_hash += entry.getKey().hashCode() ^ entry.getValue().hashCode();
			_count += 1;
		}

		return this.hashCode == _hash && this.cachedSize == _count;
	}

	// returns 0 \<= mask \<= <nMax-1>
	static byte recoverMask(<chunkSizeToPrimitive(bitPartitionSize)> map, byte i_th) {
		assert 1 \<= i_th && i_th \<= <nMax>;

		byte cnt1 = 0;
		byte mask = 0;

		while (mask \< <nMax>) {
			if ((map & 0x01) == 0x01) {
				cnt1 += 1;

				if (cnt1 == i_th) {
					return mask;
				}
			}

			map = (<chunkSizeToPrimitive(bitPartitionSize)>) (map \>\> 1);
			mask += 1;
		}

		assert cnt1 != i_th;
		throw new RuntimeException(\"Called with invalid arguments.\");
	}

	@Override
	public <coreClassName><Generics(ds)> __put(<dec(primitiveToClass(key()))>, <dec(primitiveToClass(val()))>) {
		final int keyHash = key.hashCode();
		final Result<ResultGenerics(ds)> result = rootNode.updated(null, key,
						keyHash, val, 0);

		if (result.isModified()) {
			if (result.hasReplacedValue()) {
				final int valHashOld = result.getReplacedValue().hashCode();
				final int valHashNew = <hashCode(val())>;

				return new <coreClassName><Generics(ds)>(result.getNode(), hashCode + (keyHash ^ valHashNew)
								- (keyHash ^ valHashOld), cachedSize);
			}

			final int valHash = <hashCode(val())>;
			return new <coreClassName><Generics(ds)>(result.getNode(), hashCode + (keyHash ^ valHash),
							cachedSize + 1);
		}

		return this;
	}

	@Override
	public <coreClassName><Generics(ds)> __putEquivalent(<dec(primitiveToClass(key()))>, <dec(primitiveToClass(val()))>, Comparator\<Object\> cmp) {
		final int keyHash = key.hashCode();
		final Result<ResultGenerics(ds)> result = rootNode.updated(null, key,
						keyHash, val, 0, cmp);

		if (result.isModified()) {
			if (result.hasReplacedValue()) {
				final int valHashOld = result.getReplacedValue().hashCode();
				final int valHashNew = <hashCode(val())>;

				return new <coreClassName><Generics(ds)>(result.getNode(), hashCode + (keyHash ^ valHashNew)
								- (keyHash ^ valHashOld), cachedSize);
			}

			final int valHash = <hashCode(val())>;
			return new <coreClassName><Generics(ds)>(result.getNode(), hashCode + (keyHash ^ valHash),
							cachedSize + 1);
		}

		return this;
	}

	@Override
	public ImmutableMap<GenericsExpanded(ds)> __putAll(Map<GenericsExpandedUpperBounded(ds)> map) {
		TransientMap<GenericsExpanded(ds)> tmp = asTransient();
		tmp.__putAll(map);
		return tmp.freeze();
	}

	@Override
	public ImmutableMap<GenericsExpanded(ds)> __putAllEquivalent(Map<GenericsExpandedUpperBounded(ds)> map,
					Comparator\<Object\> cmp) {
		TransientMap<GenericsExpanded(ds)> tmp = asTransient();
		tmp.__putAllEquivalent(map, cmp);
		return tmp.freeze();
	}

	@Override
	public <coreClassName><Generics(ds)> __remove(<dec(primitiveToClass(key()))>) {
		final int keyHash = key.hashCode();
		final Result<ResultGenerics(ds)> result = rootNode.removed(null, key,
						keyHash, 0);

		if (result.isModified()) {
			// TODO: carry deleted value in result
			// assert result.hasReplacedValue();
			// final int valHash = result.getReplacedValue().hashCode();

			final int valHash = rootNode.findByKey(key, keyHash, 0).get().getValue().hashCode();

			return new <coreClassName><Generics(ds)>(result.getNode(), hashCode - (keyHash ^ valHash),
							cachedSize - 1);
		}

		return this;
	}

	@Override
	public <coreClassName><Generics(ds)> __removeEquivalent(<dec(primitiveToClass(key()))>, Comparator\<Object\> cmp) {
		final int keyHash = key.hashCode();
		final Result<ResultGenerics(ds)> result = rootNode.removed(null, key,
						keyHash, 0, cmp);

		if (result.isModified()) {
			// TODO: carry deleted value in result
			// assert result.hasReplacedValue();
			// final int valHash = result.getReplacedValue().hashCode();

			final int valHash = rootNode.findByKey(key, keyHash, 0, cmp).get().getValue()
							.hashCode();

			return new <coreClassName><Generics(ds)>(result.getNode(), hashCode - (keyHash ^ valHash),
							cachedSize - 1);
		}

		return this;
	}

	@Override
	public boolean containsKey(Object o) {
		try {
			<dec(key())> = (<key().\type>) o;
			return rootNode.containsKey(<use(key())>, <hashCode(key())>, 0);			
		} catch (ClassCastException unused) {
			return false;
		}
	}

	@Override
	public boolean containsKeyEquivalent(Object o, Comparator\<Object\> cmp) {
		try {
			<dec(key())> = (<key().\type>) o;
			return rootNode.containsKey(<use(key())>, <hashCode(key())>, 0, cmp);			
		} catch (ClassCastException unused) {
			return false;
		}
	}

	@Override
	public boolean containsValue(Object o) {
		for (Iterator\<<primitiveToClass(val()).\type>\> iterator = valueIterator(); iterator.hasNext();) {
			if (iterator.next().equals(o)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean containsValueEquivalent(Object o, Comparator\<Object\> cmp) {
		for (Iterator\<<primitiveToClass(val()).\type>\> iterator = valueIterator(); iterator.hasNext();) {
			if (cmp.compare(iterator.next(), o) == 0) {
				return true;
			}
		}
		return false;
	}

	@Override
	public <primitiveToClass(val()).\type> get(Object o) {
		try {
			<dec(key())> = (<key().\type>) o;
			final Optional\<Map.Entry<GenericsExpanded(ds)>\> result = rootNode.findByKey(<use(key())>, <hashCode(key())>, 0);
	
			if (result.isPresent()) {
				return result.get().getValue();
			} else {
				return null;
			}			
		} catch (ClassCastException unused) {
			return null;
		}
	}

	@Override
	public <primitiveToClass(val()).\type> getEquivalent(Object o, Comparator\<Object\> cmp) {
		try {
			<dec(key())> = (<key().\type>) o;
			final Optional\<Map.Entry<GenericsExpanded(ds)>\> result = rootNode.findByKey(<use(key())>, <hashCode(key())>, 0, cmp);
	
			if (result.isPresent()) {
				return result.get().getValue();
			} else {
				return null;
			}			
		} catch (ClassCastException unused) {
			return null;
		}
	}

	@Override
	public int size() {
		return cachedSize;
	}

	@Override
	public SupplierIterator<SupplierIteratorGenerics(ds)> keyIterator() {
		<generate_bodyOf_keyIterator(ds, setup)>
	}

	@Override
	public Iterator\<<primitiveToClass(val()).\type>\> valueIterator() {
		return new MapValueIterator<InferredGenerics()>(rootNode);
	}

	@Override
	public Iterator\<Map.Entry<GenericsExpanded(ds)>\> entryIterator() {
		return new MapEntryIterator<InferredGenerics()>(rootNode);
	}

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
					return <coreClassName>.this.size();
				}

				@Override
				public boolean isEmpty() {
					return <coreClassName>.this.isEmpty();
				}

				@SuppressWarnings(\"deprecation\")
				@Override
				public void clear() {
					<coreClassName>.this.clear();
				}

				@Override
				public boolean contains(Object k) {
					return <coreClassName>.this.containsKey(k);
				}
			};
		}
		return entrySet;
	}

	@Override
	public boolean isTransientSupported() {
		return true;
	}

	@Override
	public TransientMap<GenericsExpanded(ds)> asTransient() {
		return new Transient<coreClassName><Generics(ds)>(this);
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

		if (other instanceof <coreClassName>) {
			<coreClassName><QuestionMarkGenerics(ds)> that = (<coreClassName><QuestionMarkGenerics(ds)>) other;

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
		return new <nodeIteratorClassName><InferredGenerics()>(rootNode);
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
	"return new MapKeyIterator<InferredGenerics()>(rootNode);"
	;
	
default str generate_bodyOf_keyIterator(DataStructure ds, rel[Option,bool] setup) =
	"return new <coreClassName>Iterator<InferredGenerics()>((CompactMapNode<Generics(ds)>) rootNode);"
	;
	