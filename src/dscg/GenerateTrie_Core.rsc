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

str generateCoreClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str innerClassesString) = 
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
import static org.eclipse.imp.pdb.facts.util.ArrayUtils.copyAndInsert;
import static org.eclipse.imp.pdb.facts.util.ArrayUtils.copyAndInsertPair;
import static org.eclipse.imp.pdb.facts.util.ArrayUtils.copyAndMoveToBackPair;
import static org.eclipse.imp.pdb.facts.util.ArrayUtils.copyAndMoveToFrontPair;
import static org.eclipse.imp.pdb.facts.util.ArrayUtils.copyAndRemove;
import static org.eclipse.imp.pdb.facts.util.ArrayUtils.copyAndRemovePair;
import static org.eclipse.imp.pdb.facts.util.ArrayUtils.copyAndSet;

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
public class TrieMap<Generics(ds)> extends AbstractImmutableMap<Generics(ds)> {

	@SuppressWarnings(\"unchecked\")
	private static final TrieMap EMPTY_INPLACE_INDEX_MAP = new TrieMap(CompactMapNode.EMPTY_INPLACE_INDEX_NODE, 0, 0);

	private static final boolean DEBUG = false;

	private final <AbstractNode(ds)><Generics(ds)> rootNode;
	private final int hashCode;
	private final int cachedSize;

	TrieMap(<AbstractNode(ds)><Generics(ds)> rootNode, int hashCode, int cachedSize) {
		this.rootNode = rootNode;
		this.hashCode = hashCode;
		this.cachedSize = cachedSize;
		if (DEBUG) {
			assert invariant();
		}
	}

	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> ImmutableMap<Generics(ds)> of() {
		return TrieMap.EMPTY_INPLACE_INDEX_MAP;
	}

	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> ImmutableMap<Generics(ds)> of(Object... keyValuePairs) {
		if (keyValuePairs.length % 2 != 0) {
			throw new IllegalArgumentException(\"Length of argument list is uneven: no key/value pairs.\");
		}

		ImmutableMap<Generics(ds)> result = TrieMap.EMPTY_INPLACE_INDEX_MAP;

		for (int i = 0; i \< keyValuePairs.length; i += 2) {
			final K key = (K) keyValuePairs[i];
			final V val = (V) keyValuePairs[i + 1];

			result = result.__put(key, val);
		}

		return result;
	}

	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> TransientMap<Generics(ds)> transientOf() {
		return TrieMap.EMPTY_INPLACE_INDEX_MAP.asTransient();
	}

	@SuppressWarnings(\"unchecked\")
	public static final <Generics(ds)> TransientMap<Generics(ds)> transientOf(Object... keyValuePairs) {
		if (keyValuePairs.length % 2 != 0) {
			throw new IllegalArgumentException(
							\"Length of argument list is uneven: no key/value pairs.\");
		}

		final TransientMap<Generics(ds)> result = TrieMap.EMPTY_INPLACE_INDEX_MAP.asTransient();

		for (int i = 0; i \< keyValuePairs.length; i += 2) {
			final K key = (K) keyValuePairs[i];
			final V val = (V) keyValuePairs[i + 1];

			result.__put(key, val);
		}

		return result;
	}

	private boolean invariant() {
		int _hash = 0;
		int _count = 0;

		for (Iterator\<Map.Entry<Generics(ds)>\> it = entryIterator(); it.hasNext();) {
			final Map.Entry<Generics(ds)> entry = it.next();

			_hash += entry.getKey().hashCode() ^ entry.getValue().hashCode();
			_count += 1;
		}

		return this.hashCode == _hash && this.cachedSize == _count;
	}

	// returns 0 \<= mask \<= <nMax-1>
	static byte recoverMask(int map, byte i_th) {
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

			map = map \>\> 1;
			mask += 1;
		}

		assert cnt1 != i_th;
		throw new RuntimeException(\"Called with invalid arguments.\");
	}

	@Override
	public TrieMap<Generics(ds)> __put(K key, V val) {
		final int keyHash = key.hashCode();
		final Result<ResultGenerics(ds)> result = rootNode.updated(null, key,
						keyHash, val, 0);

		if (result.isModified()) {
			if (result.hasReplacedValue()) {
				final int valHashOld = result.getReplacedValue().hashCode();
				final int valHashNew = val.hashCode();

				return new TrieMap<Generics(ds)>(result.getNode(), hashCode + (keyHash ^ valHashNew)
								- (keyHash ^ valHashOld), cachedSize);
			}

			final int valHash = val.hashCode();
			return new TrieMap<Generics(ds)>(result.getNode(), hashCode + (keyHash ^ valHash),
							cachedSize + 1);
		}

		return this;
	}

	@Override
	public TrieMap<Generics(ds)> __putEquivalent(K key, V val, Comparator\<Object\> cmp) {
		final int keyHash = key.hashCode();
		final Result<ResultGenerics(ds)> result = rootNode.updated(null, key,
						keyHash, val, 0, cmp);

		if (result.isModified()) {
			if (result.hasReplacedValue()) {
				final int valHashOld = result.getReplacedValue().hashCode();
				final int valHashNew = val.hashCode();

				return new TrieMap<Generics(ds)>(result.getNode(), hashCode + (keyHash ^ valHashNew)
								- (keyHash ^ valHashOld), cachedSize);
			}

			final int valHash = val.hashCode();
			return new TrieMap<Generics(ds)>(result.getNode(), hashCode + (keyHash ^ valHash),
							cachedSize + 1);
		}

		return this;
	}

	@Override
	public ImmutableMap<Generics(ds)> __putAll(Map\<? extends K, ? extends V\> map) {
		TransientMap<Generics(ds)> tmp = asTransient();
		tmp.__putAll(map);
		return tmp.freeze();
	}

	@Override
	public ImmutableMap<Generics(ds)> __putAllEquivalent(Map\<? extends K, ? extends V\> map,
					Comparator\<Object\> cmp) {
		TransientMap<Generics(ds)> tmp = asTransient();
		tmp.__putAllEquivalent(map, cmp);
		return tmp.freeze();
	}

	@Override
	public TrieMap<Generics(ds)> __remove(K key) {
		final int keyHash = key.hashCode();
		final Result<ResultGenerics(ds)> result = rootNode.removed(null, key,
						keyHash, 0);

		if (result.isModified()) {
			// TODO: carry deleted value in result
			// assert result.hasReplacedValue();
			// final int valHash = result.getReplacedValue().hashCode();

			final int valHash = rootNode.findByKey(key, keyHash, 0).get().getValue().hashCode();

			return new TrieMap<Generics(ds)>(result.getNode(), hashCode - (keyHash ^ valHash),
							cachedSize - 1);
		}

		return this;
	}

	@Override
	public TrieMap<Generics(ds)> __removeEquivalent(K key, Comparator\<Object\> cmp) {
		final int keyHash = key.hashCode();
		final Result<ResultGenerics(ds)> result = rootNode.removed(null, key,
						keyHash, 0, cmp);

		if (result.isModified()) {
			// TODO: carry deleted value in result
			// assert result.hasReplacedValue();
			// final int valHash = result.getReplacedValue().hashCode();

			final int valHash = rootNode.findByKey(key, keyHash, 0, cmp).get().getValue()
							.hashCode();

			return new TrieMap<Generics(ds)>(result.getNode(), hashCode - (keyHash ^ valHash),
							cachedSize - 1);
		}

		return this;
	}

	@Override
	public boolean containsKey(Object o) {
		return rootNode.containsKey(o, o.hashCode(), 0);
	}

	@Override
	public boolean containsKeyEquivalent(Object o, Comparator\<Object\> cmp) {
		return rootNode.containsKey(o, o.hashCode(), 0, cmp);
	}

	@Override
	public boolean containsValue(Object o) {
		for (Iterator\<V\> iterator = valueIterator(); iterator.hasNext();) {
			if (iterator.next().equals(o)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean containsValueEquivalent(Object o, Comparator\<Object\> cmp) {
		for (Iterator\<V\> iterator = valueIterator(); iterator.hasNext();) {
			if (cmp.compare(iterator.next(), o) == 0) {
				return true;
			}
		}
		return false;
	}

	@Override
	public V get(Object key) {
		final Optional\<Map.Entry<Generics(ds)>\> result = rootNode.findByKey(key, key.hashCode(), 0);

		if (result.isPresent()) {
			return result.get().getValue();
		} else {
			return null;
		}
	}

	@Override
	public V getEquivalent(Object key, Comparator\<Object\> cmp) {
		final Optional\<Map.Entry<Generics(ds)>\> result = rootNode.findByKey(key, key.hashCode(), 0, cmp);

		if (result.isPresent()) {
			return result.get().getValue();
		} else {
			return null;
		}
	}

	@Override
	public int size() {
		return cachedSize;
	}

	@Override
	public SupplierIterator<Generics(ds)> keyIterator() {
		<generate_bodyOf_keyIterator(ds, setup)>
	}

	@Override
	public Iterator\<V\> valueIterator() {
		return new MapValueIterator\<\>(rootNode);
	}

	@Override
	public Iterator\<Map.Entry<Generics(ds)>\> entryIterator() {
		return new MapEntryIterator\<\>(rootNode);
	}

	@Override
	public Set\<java.util.Map.Entry<Generics(ds)>\> entrySet() {
		Set\<java.util.Map.Entry<Generics(ds)>\> entrySet = null;

		if (entrySet == null) {
			entrySet = new AbstractSet\<java.util.Map.Entry<Generics(ds)>\>() {
				@Override
				public Iterator\<java.util.Map.Entry<Generics(ds)>\> iterator() {
					return new Iterator\<Entry<Generics(ds)>\>() {
						private final Iterator\<Entry<Generics(ds)>\> i = entryIterator();

						@Override
						public boolean hasNext() {
							return i.hasNext();
						}

						@Override
						public Entry<Generics(ds)> next() {
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
					return TrieMap.this.size();
				}

				@Override
				public boolean isEmpty() {
					return TrieMap.this.isEmpty();
				}

				@SuppressWarnings(\"deprecation\")
				@Override
				public void clear() {
					TrieMap.this.clear();
				}

				@Override
				public boolean contains(Object k) {
					return TrieMap.this.containsKey(k);
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
	public TransientMap<Generics(ds)> asTransient() {
		return new TransientTrieMap<Generics(ds)>(this);
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

		if (other instanceof TrieMap) {
			TrieMap\<?, ?\> that = (TrieMap\<?, ?\>) other;

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
		return new TrieMapNodeIterator\<\>(rootNode);
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
	protected void printStatistics() {
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


str generate_bodyOf_keyIterator(DataStructure ds, rel[Option,bool] setup:{_*, <useFixedStackIterator(),true>}) = 
	"return new MapKeyIterator\<\>(rootNode);"
	;
	
default str generate_bodyOf_keyIterator(DataStructure ds, rel[Option,bool] setup) =
	"return new TrieMapIterator\<\>((CompactMapNode<Generics(ds)>) rootNode);"
	;
	