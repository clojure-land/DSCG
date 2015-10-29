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
module dscg::GenerateTrie

import IO;
import List;
import String;
import util::Math;

import dscg::GenerateTrie_Master;

data TrieConfig 
	= hashTrieConfig(DataStructure ds, int bitPartitionSize, list[Type] tupleTypes, SpecializationConfig specializationConfig);

data SpecializationConfig 
	= withoutSpecialization()
	| specializationConfig(int specializeTo, bool flagUntypedVariables)
	| heterogeneousSpecializationConfig(int specializeTo);

void main() {
	dataStructures = { \set(), \map() };
	bitPartitionSizes = { 5 };
	typeCombGenInt = { generic("K"), primitive("int") } * { primitive("int"), generic("V") };
	
	specializeToBounds = { 8 };
	booleanOptions = { true, false };
				
	set[SpecializationConfig] specializationConfigs 
		= { specializationConfig(specializeTo, flagUntypedVariables) | 
				<int specializeTo, bool flagUntypedVariables> <- specializeToBounds * booleanOptions } 
		+ withoutSpecialization();
			
	set[TrieConfig] trieConfigs 
		= { hashTrieConfig(d,b,[keyType,valType],s) | 
				d <- dataStructures, b <- bitPartitionSizes, t:<keyType,valType> <- typeCombGenInt, s <- specializationConfigs,
					(specializationConfig(_,true) := s) ==> (isGeneric(keyType) && isGeneric(valType)),
					(withoutSpecialization() := s) ==> (isGeneric(keyType) && isGeneric(valType))};					
				
	for (TrieConfig cfg <- trieConfigs) {
		doGenerate(cfg);
	}
	
	doGenerateInterfaces();	
}

void doGenerateInterfaces() {
	TrieSpecifics genericTsSet = expandConfigurationAndCreateModel(hashTrieConfig(\set(), 5, [generic("K"), generic("V")], withoutSpecialization()), "");
	TrieSpecifics genericTsMap = expandConfigurationAndCreateModel(hashTrieConfig(\map(), 5, [generic("K"), generic("V")], withoutSpecialization()), "");
	
	writeFile(|project://<targetProject>/<targetFolder>/<immutableInterfaceName(\set())>.java|, generateImmutableInterface(genericTsSet));
	writeFile(|project://<targetProject>/<targetFolder>/<transientInterfaceName(\set())>.java|, generateTransientInterface(genericTsSet));

	writeFile(|project://<targetProject>/<targetFolder>/<immutableInterfaceName(\map())>.java|, generateImmutableInterface(genericTsMap));
	writeFile(|project://<targetProject>/<targetFolder>/<transientInterfaceName(\map())>.java|, generateTransientInterface(genericTsMap));	
}

void doGenerateCurrent() {
	doGenerate(hashTrieConfig(\map(), 5, [generic("K"), generic("V")], withoutSpecialization()));
	doGenerate(hashTrieConfig(\set(), 5, [generic("K"), generic("V")], withoutSpecialization()));	
}

void doGenerateBleedingEdge() {
//	doGenerate(hashTrieConfig(\map(), 5, [generic("K"), generic("V")], specializationConfig(8, true)), overideClassNamePostfixWith = "BleedingEdge_Untyped");
//	doGenerate(hashTrieConfig(\set(), 5, [generic("K"), generic("V")], specializationConfig(8, true)), overideClassNamePostfixWith = "BleedingEdge_Untyped");	
//
//	doGenerate(hashTrieConfig(\map(), 5, [generic("K"), generic("V")], specializationConfig(8, false)), overideClassNamePostfixWith = "BleedingEdge_Typed");
//	doGenerate(hashTrieConfig(\set(), 5, [generic("K"), generic("V")], specializationConfig(8, false)), overideClassNamePostfixWith = "BleedingEdge_Typed");	

	//genericTsSet = expandConfigurationAndCreateModel(hashTrieConfig(\set(), 5, [generic("K"), generic("V")], specializationConfig(8, false)), "BleedingEdge");
	//genericTsMap = expandConfigurationAndCreateModel(hashTrieConfig(\map(), 5, [generic("K"), generic("V")], specializationConfig(8, false)), "BleedingEdge");
	//ts = genericTsMap;

	//doGenerate(hashTrieConfig(\map(), 5, [object(), object()], heterogeneousSpecializationConfig(4)), overideClassNamePostfixWith = "BleedingEdge");
	//doGenerate(hashTrieConfig(\set(), 5, [object(), object()], heterogeneousSpecializationConfig(8)), overideClassNamePostfixWith = "BleedingEdge");
}

void doGenerateHeterogeneous() {
	
	// ts = expandConfigurationAndCreateModel(hashTrieConfig(\map(), 3, [primitive("int"), primitive("int")], heterogeneousSpecializationConfig(8)), "Heterogeneous_BleedingEdge");
	
	doGenerate(hashTrieConfig(\map(), 3, [primitive("int"), primitive("int")], heterogeneousSpecializationConfig(8)), overideClassNamePostfixWith = "Heterogeneous_BleedingEdge");

}

void doGenerateMapAsSetOfTuples() {	
	doGenerate(hashTrieConfig(\map(), 5, [ object(), object() ], withoutSpecialization()), overideClassNamePostfixWith = "MapAsSetOfTuples");
}

void doGenerateBleedingEdgeExpanded() {
	// generate map and set interfaces
	doGenerateInterfaces();

	TrieConfig tcMultimap = hashTrieConfig(\map(multi = true), 5, [generic("K"), generic("V")], withoutSpecialization());

	writeFile(|project://<targetProject>/<targetFolder>/<immutableInterfaceName(\map(multi = true))>.java|, generateImmutableInterface(expandConfigurationAndCreateModel(tcMultimap, "")));
	writeFile(|project://<targetProject>/<targetFolder>/<transientInterfaceName(\map(multi = true))>.java|, generateTransientInterface(expandConfigurationAndCreateModel(tcMultimap, "")));

	doGenerate(tcMultimap, overideClassNamePostfixWith = "BleedingEdge");
	
	doGenerate(hashTrieConfig(\map(), 5, [generic("K"), generic("V")], withoutSpecialization()), overideClassNamePostfixWith = "BleedingEdge");
	doGenerate(hashTrieConfig(\set(), 5, [generic("K"), generic("V")], withoutSpecialization()), overideClassNamePostfixWith = "BleedingEdge");	

	//doGenerate(hashTrieConfig(\map(), 5, [primitive("int"), primitive("int")], withoutSpecialization()), overideClassNamePostfixWith = "BleedingEdge");
	//doGenerate(hashTrieConfig(\set(), 5, [primitive("int"), primitive("int")], withoutSpecialization()), overideClassNamePostfixWith = "BleedingEdge");	

	//doGenerate(hashTrieConfig(\map(), 5, [generic("K"), generic("V")], specializationConfig(1, false)), overideClassNamePostfixWith = "BleedingEdge");
	//doGenerate(hashTrieConfig(\set(), 5, [generic("K"), generic("V")], specializationConfig(1, false)), overideClassNamePostfixWith = "BleedingEdge");	
}

void doGenerateBleedingEdgeMultimap() {
	doGenerate(hashTrieConfig(\map(multi = true), 5, [generic("K"), generic("V")], withoutSpecialization()), overideClassNamePostfixWith = "BleedingEdge");
	//doGenerate(hashTrieConfig(\set(), 5, [generic("K"), generic("V")], withoutSpecialization()), overideClassNamePostfixWith = "BleedingEdge");	
}

void doGenerateSpecializedUntyped() {
	doGenerate(hashTrieConfig(\map(), 5, [generic("K"), generic("V")], specializationConfig(8, true)));
	doGenerate(hashTrieConfig(\set(), 5, [generic("K"), generic("V")], specializationConfig(8, true)));	
}

TrieSpecifics expandConfigurationAndCreateModel(TrieConfig cfg, str overideClassNamePostfixWith) {
	TrieSpecifics ts = expandConfiguration(cfg, overideClassNamePostfixWith);
	
	// *** STAGE: CREATE MODEL *** //
	return ts[model = buildLanguageAgnosticModel(ts)];	
}

TrieSpecifics expandConfiguration(TrieConfig cfg:hashTrieConfig(DataStructure ds, int bitPartitionSize, list[Type] tupleTypes:[keyType, valType, *_], SpecializationConfig specializationConfig), str overideClassNamePostfixWith) {
	bool flagSpecialization = false;
	bool flagHeterogeneousSpecialization = false;
	int specializeTo = 0;
	bool flagUntypedVariables = false;	

	if (specializationConfig(__specializeTo, __flagUntypedVariables) := specializationConfig) {
		flagSpecialization = true;
		specializeTo = __specializeTo;
		flagUntypedVariables = __flagUntypedVariables;
	}

	if (heterogeneousSpecializationConfig(__specializeTo) := specializationConfig) {
		flagSpecialization = true;
		//flagHeterogeneousSpecialization = true;
		specializeTo = __specializeTo;
		flagUntypedVariables = false; // heterogeneous config implies untyped, hower the option are distinct for easier pattern matching.
	}
		
	str classNamePostfix = "_<bitPartitionSize>Bits";
	
	if (flagUntypedVariables) {
		classNamePostfix = classNamePostfix + "_Untyped";
	}
	
	if (flagSpecialization) {
		classNamePostfix = classNamePostfix + "_Spec0To<specializeTo>";
	}	
	
	if (!isGeneric(keyType)) {
		classNamePostfix = classNamePostfix + "_<capitalize(typeToString(keyType))>Key";
	}	
	if (!isGeneric(valType) && \map() := ds) {
		classNamePostfix = classNamePostfix + "_<capitalize(typeToString(valType))>Value";
	}

	if (overideClassNamePostfixWith != "") {
		classNamePostfix = "_<overideClassNamePostfixWith>";
	}

	rel[Option,bool] setup = { 
		<useSpecialization(),flagSpecialization>,
		<useUntypedVariables(),flagUntypedVariables>,
		<useFixedStackIterator(),true>,
		<useSupplierIterator(),false>,
		<toStringOnTrieNodes(),false>,
		<useStructuralEquality(),true>,
		<methodsWithComparator(),true>,
		<useSandwichArrays(),true>,
		<useStagedMutability(),true>,
		<usePrefixInsteadOfPostfixEncoding(),false>,	
		<usePathCompression(),false>,
		<useIncrementalHashCodes(),true>,
		<separateTrieAndLeafNodes(),false>,
		<compareValueAtMapPut(),false>,
		<useHeterogeneousEncoding(),false>,
		<useSunMiscUnsafe(),false>,
		<unsafeCodeAsData(),true>
	}; // { compactionViaFieldToMethod() };

	return trieSpecifics(ds, bitPartitionSize, specializeTo, keyType, valType, classNamePostfix, setup, unknownArtifact());
}

void doGenerate(TrieConfig cfg, str overideClassNamePostfixWith = "") {
	TrieSpecifics ts = expandConfiguration(cfg, overideClassNamePostfixWith);
	
	// *** STAGE: CREATE MODEL *** //
	ts = ts[model = buildLanguageAgnosticModel(ts)];
	
	// *** STAGE: GENERATE CODE *** //
	
	list[str] innerClassStrings = doGenerateInnerClassStrings(ts);
	//if (\map(multi = true) := ts.ds) {
	//	TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts);
	//	
	//	innerClassStrings = innerClassStrings
	//	+ [ generateResultClassString(tsSet, ts.setup) ]
	//	+ [ generateAbstractNodeClassString(tsSet)]		
	//	+ [ generateCompactNodeClassString(tsSet, ts.setup)];
	//}	
		
	list[str] classStrings = [ generateCoreClassString(ts, ts.setup, intercalate("\n", innerClassStrings))];			
		
	// writeFile(|project://DSCG/gen/org/eclipse/imp/pdb/facts/util/AbstractSpecialisedTrieMap.java|, classStrings);

	writeFile(|project://<targetProject>/<targetFolder>/Trie<toString(ts.ds)><ts.classNamePostfix>.java|, classStrings);
}
	
list[str] doGenerateInnerClassStrings(TrieSpecifics ts) {
	bool isLegacy = false;

	list[str] innerClassStrings 
		= [ generateOptionalClassString() ]
		+ [ generateResultClassString(ts, ts.setup) ]
		+ [ generateAbstractAnyNodeClassString(ts, ts.setup)]
		+ [ generateAbstractNodeClassString(ts, isLegacy = isLegacy)]
		+ [ generateCompactNodeClassString(ts, isLegacy)];
		// + [ generateFeatureFlagsClassString(ts, isLegacy = true)];

	//if (isOptionEnabled(ts.setup, useHeterogeneousEncoding())) {
	//	innerClassStrings = innerClassStrings + [ generateCompactHeterogeneousNodeClassString(ts, isLegacy = isLegacy)];
	//}

	if (isOptionEnabled(ts.setup, separateTrieAndLeafNodes())) {
		innerClassStrings = innerClassStrings + [ generateLeafNodeClassString(ts)];
	}

	if (!isOptionEnabled(ts.setup, useSpecialization()) || ts.nBound < ts.nMax) {
		innerClassStrings = innerClassStrings + [ generateBitmapIndexedNodeClassString(ts, isLegacy = isLegacy)];
	}

	innerClassStrings 
		= innerClassStrings
		+ [ generateHashCollisionNodeClassString(ts, isLegacy = isLegacy)]
		+ [ generateIteratorClassString(ts, ts.setup)] // , classNamePostfix
		;
	
	if (!isOptionEnabled(ts.setup, useFixedStackIterator())) {
		innerClassStrings = innerClassStrings + [ generateEasyIteratorClassString(ts, ts.setup)];
	}
	
	innerClassStrings 
		= innerClassStrings
		+ [ generateNodeIteratorClassString(ts, ts.setup, ts.classNamePostfix)]		
		;
		
	if (isOptionEnabled(ts.setup, useStagedMutability())) { 
		innerClassStrings = innerClassStrings + [ generateCoreTransientClassString(ts)];
	}	
		
	if (isOptionEnabled(ts.setup, useSpecialization()) && !isOptionEnabled(ts.setup, useUntypedVariables()) && !isOptionEnabled(ts.setup, useHeterogeneousEncoding())) {
		innerClassStrings = innerClassStrings + 
		// TODO: [ generateSpecializedBitmapIndexedNodeClassString(ts, specializedBitmapIndexedNode(n, m)) | m <- [0..ts.nMax+1], n <- [0..ts.nMax+1], (n + m) <= ts.nBound ];
		[ generateSpecializedNodeWithBitmapPositionsClassString(n, m, ts, ts.classNamePostfix) | m <- [0..ts.nMax+1], n <- [0..ts.nMax+1], (n + m) <= ts.nBound ];
	}

	// TODO: fix correct creation of mn instead of m and n		
	if (isOptionEnabled(ts.setup, useSpecialization()) && isOptionEnabled(ts.setup, useUntypedVariables()) && !isOptionEnabled(ts.setup, useHeterogeneousEncoding())) {
		innerClassStrings = innerClassStrings +
		// TODO: [ generateSpecializedBitmapIndexedNodeClassString(ts, specializedBitmapIndexedNode(n, m)) | mn <- [0.. tupleLength(ts.ds) * ts.nMax + 1], mn <= tupleLength(ts.ds) * ts.nBound ]; 
		[ generateSpecializedNodeWithBitmapPositionsClassString(mn, 0, ts, ts.classNamePostfix) | mn <- [0.. tupleLength(ts.ds) * ts.nMax + 1], mn <= tupleLength(ts.ds) * ts.nBound ];
	}
	
	// TODO: fix correct creation of mn instead of m and n		
	if (isOptionEnabled(ts.setup, useHeterogeneousEncoding())) {
		innerClassStrings = innerClassStrings + 
		[ generateSpecializedBitmapIndexedNodeClassString(ts, specializedBitmapIndexedNode(n, m)) | m <- [0..ts.nMax+1], n <- [0..ts.nMax+1], (n + m) <= ts.nBound ];
		//[ generateSpecializedNodeWithBitmapPositionsClassString(mn, 0, ts, ts.setup, ts.classNamePostfix) | mn <- [0.. tupleLength(ts.ds) * ts.nMax + 1], mn <= tupleLength(ts.ds) * ts.nBound ];
	}	
	
	return innerClassStrings;
}
	
//str generateClassString(int n) =  
//	"class Map<n><GenericsStr(ts.tupleTypes)> extends AbstractSpecialisedImmutableMap<GenericsStr(ts.tupleTypes)> {
//	'	<for (i <- [1..n+1]) {>
//	'	private final K <keyName><i>;
//	'	private final V <valName><i>;
//	'	<}>	
//	'
//	'	Map<n>(<for (i <- [1..n+1]) {>final K <keyName><i>, final V <valName><i><if (i != n) {>, <}><}>) {					
//	'		<checkForDuplicateKeys(n)><intercalate("\n\n", ["this.<keyName><i> = <keyName><i>; this.<valName><i> = <valName><i>;" | i <- [1..n+1]])>
//	'	}
//
//	'	@Override
//	'	public boolean <containsKeyMethodName(ds)>(Object <keyName>) {
//	'		<generate_bodyOf_containsKeyOrVal(n, equalityDefault, keyName)>	
//	'	}
//
//	'	@Override
//	'	public boolean <containsKeyMethodName(ds)>Equivalent(Object <keyName>, Comparator\<Object\> <cmpName>) {
//	'		<generate_bodyOf_containsKeyOrVal(n, equalityComparator, keyName)>	
//	'	}
//	
//	'	@Override
//	'	public boolean containsValue(Object <valName>) { 
//	'		<generate_bodyOf_containsKeyOrVal(n, equalityDefault, valName)>
//	'	}
//	
//	'	@Override
//	'	public boolean containsValueEquivalent(Object <valName>, Comparator\<Object\> <cmpName>) {
//	'		<generate_bodyOf_containsKeyOrVal(n, equalityComparator, valName)>
//	'	}
//		
//	'	@Override
//	'	public V get(Object <keyName>) {
//	'		<generate_bodyOf_get(n, equalityDefault)>
//	'	}
//	
//	'	@Override
//	'	public V getEquivalent(Object <keyName>, Comparator\<Object\> <cmpName>) {
//	'		<generate_bodyOf_get(n, equalityComparator)>
//	'	}	
//
//	'	@Override
//	'	public int size() {
//	'		return <n>;
//	'	}
//
//	'	@Override
//	'	public Set\<Entry<GenericsStr(ts.tupleTypes)>\> entrySet() {
//	'		<generate_bodyOf_entrySet(n)>
//	'	}
//
//	'	@Override
//	'	public Set\<K\> keySet() {
//	'		<generate_bodyOf_keySet(n)>
//	'	}
//
//	'	@Override
//	'	public Collection\<V\> values() {
//	'		<generate_bodyOf_values(n)>
//	'	}
//	
//	'	@Override
//	'	public SupplierIterator<SupplierIteratorGenerics(ds)> keyIterator() {
//	'		<generate_bodyOf_keyIterator(n)>
//	'	}	
//
//	'	@Override
//	'	public ImmutableMap<GenericsStr(ts.tupleTypes)> __put(K <keyName>, V <valName>) {
//	'		<generate_bodyOf_put(n, equalityDefault)>
//	'	}
//	
//	'	@Override
//	'	public ImmutableMap<GenericsStr(ts.tupleTypes)> __putEquivalent(K <keyName>, V <valName>, Comparator\<Object\> <cmpName>) {
//	'		<generate_bodyOf_put(n, equalityComparator)>
//	'	}	
//
//	'	@Override
//	'	public ImmutableMap<GenericsStr(ts.tupleTypes)> __remove(K <keyName>) {
//	'		<generate_bodyOf_remove(n, equalityDefault)>	
//	'	}
//
//	'	@Override
//	'	public ImmutableMap<GenericsStr(ts.tupleTypes)> __removeEquivalent(K <keyName>, Comparator\<Object\> <cmpName>) {
//	'		<generate_bodyOf_remove(n, equalityComparator)>
//	'	}
//	
//	'	@Override
//	'	public TransientMap<GenericsStr(ts.tupleTypes)> asTransient() {
//	'		return TrieMap.transientOf(<for (i <- [1..n+1]) {><keyName><i>, <valName><i><if (i != n) {>, <}><}>);
//	'	}
//	
//	'	@Override
//	'	public int hashCode() {
//	'		<if (n == 0) {>return 0;<} else {>return (<for (i <- [1..n+1]) {>(Objects.hashCode(<keyName><i>) ^ Objects.hashCode(<valName><i>))<if (i != n) {> + <}><}>);<}>
//	'	}		
//	
//	'	@Override
//	'	public String toString() {
//	'		<if (n == 0) {>return \"{}\";<} else {>return String.format(\"{<for (i <- [1..n+1]) {>%s=%s<if (i != n) {>, <}><}>}\", <for (i <- [1..n+1]) {><keyName><i>, <valName><i><if (i != n) {>, <}><}>);<}>
//	'	}
//	
//	'}
//	";
