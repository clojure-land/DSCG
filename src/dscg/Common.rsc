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
module dscg::Common

import List;
import util::Math;


data DataStructure
	= \map()
	| \set()
	| \vector()
	;

data Type 
	= unknown  (bool isArray = false)
	| object   (bool isArray = false)
	| generic  (str \type, bool isArray = false)
	| specific (str \type, bool isArray = false)
	//| primitiveByte()
	//| primitiveShort()
	//| primitiveInt()
	//| primitiveLong()
	| primitive(str \type, bool isArray = false)
	| ___primitive(str \type, bool isArray = false)
	;

str toString(t:object()) = "java.lang.Object<if (t.isArray) {>[]<}>";
str toString(t:generic  (str \type)) = "<\type><if (t.isArray) {>[]<}>";
str toString(t:specific (str \type)) = "<\type><if (t.isArray) {>[]<}>";
str toString(t:primitive(str \type)) = "<\type><if (t.isArray) {>[]<}>";
str toString(t:___primitive(str \type)) = "<\type><if (t.isArray) {>[]<}>";
default str toString(Type _) { throw "Ahhh"; } 

data Argument
	= field (Type \type, str name)
	| getter(Type \type, str name)
	;

data Option // TODO: finish!
	= useSpecialization()
	| useUntypedVariables() // dependent on useSpecialization() 
	| useFixedStackIterator()
	| useStructuralEquality()	
	| methodsWithComparator()
	| compactionViaFieldToMethod()
	;

data TrieSpecifics 
	= ___expandedTrieSpecifics(DataStructure ds, int bitPartitionSize, int nMax, int nBound)
	;
	
TrieSpecifics trieSpecifics(DataStructure ds, int bitPartitionSize, int nBound) {
	if (bitPartitionSize < 1 || bitPartitionSize > 6) {
		throw "Unsupported bit partition size of <bitPartitionSize>.";
	}
	
	int nMax = toInt(pow(2, bitPartitionSize));
	
	if (nBound > nMax) {
		throw "Specialization bound (<nBound>) must be smaller than the number of buckets (<nMax>)";
	}
	
	return ___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound);
}

data Position // TODO: finish!
	= positionField(bool sorted = true)
	| positionBitmap()
	;

bool isOptionEnabled(rel[Option,bool] setup, Option \o) { 
	if ({_*, <\o, b>} := setup) {
		return b;
	} else {
		throw "Option <\o> not present.";
	}
}
	
/*
 * Rewrite Rules
 */ 
Type primitive(str \type:"byte", bool isArray = false)  = ___primitive(\type, isArray=isArray);
Type primitive(str \type:"short", bool isArray = false) = ___primitive(\type, isArray=isArray);
Type primitive(str \type:"int", bool isArray = false)   = ___primitive(\type, isArray=isArray);
Type primitive(str \type:"long", bool isArray = false)  = ___primitive(\type, isArray=isArray);
default Type primitive(str _, bool _) { throw "Ahhh"; }

Type asArray(unknown(isArray = false)) = unknown(isArray = true);
Type asArray(object(isArray = false)) = object(isArray = true);
Type asArray(generic(\type, isArray = false)) = generic(\type, isArray = true);
Type asArray(specific(\type, isArray = false)) = specific(\type, isArray = true);
Type asArray(primitive(\type, isArray = false)) = primitive(\type, isArray = true);
Type asArray(primitive(\type, isArray = false)) = primitive(\type, isArray = true);
Type asArray(___primitive(\type, isArray = false)) = ___primitive(\type, isArray = true);
default Type asArray(Type \type) { throw "Ahhh"; }

Argument getter(str name) = getter(unknown(), name); 
Argument field (str name) = field (unknown(), name);
Argument getter(str name) = getter(unknown(), name);

Argument keyPos(int i) 		= field(primitive("byte"), "<keyPosName><i>");
Argument key()				= key("<keyName>");
Argument key(int i) 		= key("<keyName><i>");
Argument key(str name) 		= field(generic("K"), "<name>");
Argument val()				= val("<valName>");
Argument val(int i) 		= val("<valName><i>");
Argument val(str name) 		= field(generic("V"), "<name>");

Argument slot() 			= slot("<slotName>");
Argument slot(int i) 		= slot("<slotName><i>");
Argument slot(str name)		= field(object(), "<name>");

Argument nodePos(int i) = field("byte", "<nodePosName><i>");
Argument \node(DataStructure ds)			= field(specific("<CompactNode(ds)><Generics(ds)>"), "<nodeName>");
Argument \node(DataStructure ds, int i) 	= field(specific("<CompactNode(ds)><Generics(ds)>"), "<nodeName><i>");
Argument \node(DataStructure ds, str name)	= field(specific("<CompactNode(ds)><Generics(ds)>"), name);

public Argument bitmapField = field("nodeMap");
public Argument valmapField = field("dataMap");
public Argument bitposField = field("bitpos");

Argument ___bitmapField(int bitPartitionSize) = field(chunkSizeToPrimitive(bitPartitionSize), "nodeMap");
Argument ___valmapField(int bitPartitionSize) = field(chunkSizeToPrimitive(bitPartitionSize), "dataMap");
Argument ___bitposField(int bitPartitionSize) = field(chunkSizeToPrimitive(bitPartitionSize), "bitpos");

public Argument bitmapMethod = getter("nodeMap");
public Argument valmapMethod = getter("dataMap");

Argument ___bitmapMethod(int bitPartitionSize) = getter(chunkSizeToPrimitive(bitPartitionSize), "nodeMap");
Argument ___valmapMethod(int bitPartitionSize) = getter(chunkSizeToPrimitive(bitPartitionSize), "dataMap");
Argument ___bitposMethod(int bitPartitionSize) = getter(chunkSizeToPrimitive(bitPartitionSize), "bitpos");

public Argument thisMutator = field(specific("Void"), "null");

Type chunkSizeToPrimitive(int _:3) = primitive("byte");
Type chunkSizeToPrimitive(int _:4) = primitive("short");
Type chunkSizeToPrimitive(int _:5) = primitive("int");
Type chunkSizeToPrimitive(int _:6) = primitive("long");

str chunkSizeToObject(int _:3) = "java.lang.Byte";
str chunkSizeToObject(int _:4) = "java.lang.Short";
str chunkSizeToObject(int _:5) = "java.lang.Integer";
str chunkSizeToObject(int _:6) = "java.lang.Long";

str integerOrLongObject(int _:6) = "java.lang.Long";
str integerOrLongObject(int _:n) = "java.lang.Integer" when n > 0 && n < 6;

// convert either to int or to long and take care of unsigned conversion 
str useSafeUnsigned(Argument a) = "(int)(<use(a)> & 0xFF)"   when a has \type && a.\type == primitive("byte");
str useSafeUnsigned(Argument a) = "(int)(<use(a)> & 0xFFFF)" when a has \type && a.\type == primitive("short");
str useSafeUnsigned(Argument a) = "<use(a)>"                 when a has \type && a.\type == primitive("int");
str useSafeUnsigned(Argument a) = "<use(a)>" when a has \type && a.\type == primitive("long");
default str useSafeUnsigned(Argument a) { throw "ahhh"; }

str hashCode(Argument a) = primitiveHashCode(a) when isPrimitive(a.\type);
default str hashCode(Argument a) = "<use(a)>.hashCode()";

str primitiveHashCode(Argument a) = "(int)(<use(a)> ^ (<use(a)> \>\>\> 32))" when a has \type && a.\type == "long";
default str primitiveHashCode(Argument a) = "(int) <use(a)>";




Argument primitiveToClass(field("byte", name))  = field("java.lang.Byte", name);
Argument primitiveToClass(field("short", name)) = field("java.lang.Short", name);
Argument primitiveToClass(field("int", name))   = field("java.lang.Integer", name);
Argument primitiveToClass(field("long", name))  = field("java.lang.Long", name);
Argument primitiveToClass(field("Object", name))= field("java.lang.Object", name); // HACK!!!
/***/
Argument primitiveToClass(getter("byte", name))  = getter("java.lang.Byte", name);
Argument primitiveToClass(getter("short", name)) = getter("java.lang.Short", name);
Argument primitiveToClass(getter("int", name))   = getter("java.lang.Integer", name);
Argument primitiveToClass(getter("long", name))  = getter("java.lang.Long", name);
Argument primitiveToClass(getter("Object", name))= getter("java.lang.Object", name); // HACK!!!
/***/
default Argument primitiveToClass(Argument nonPrimitive)  = nonPrimitive;

bool isPrimitive(Type \type) = true when ___primitive(_) := \type;
default bool isPrimitive(Type _) = false;
/***/
bool isPrimitive(Argument a) = true when ___primitive(_) := a.\type;
default bool isPrimitive(Argument a) = false;

bool isGeneric(Type \type) = true when generic(_) := \type;
default bool isGeneric(Type _) = false;
/***/
bool isGeneric(Argument a) = true when generic(_) := a.\type;
default bool isGeneric(Argument a) = false;

bool isPrimitiveArray("byte[]")  = true;
bool isPrimitiveArray("short[]") = true;
bool isPrimitiveArray("int[]")   = true;
bool isPrimitiveArray("long[]")  = true;
bool isPrimitiveArray("Object[]")= true; // HACK!!!
default bool isPrimitiveArray(str x) = false;
/***/
bool isPrimitiveArray(Argument a)  = isPrimitiveArray(a.\type);
default bool isPrimitiveArray(_) { throw "aahh"; }
/***/



/*
 * Functions
 */
str use(field(_, name)) = name;
str use(getter(_, name)) = "<name>()";
default str use(Argument a) { throw "You forgot <a>!"; }
/***/
str use(list[Argument] xs) = intercalate(", ", mapper(xs, use));

str dec(field(\type, name)) = "final <toString(\type)> <name>";
str dec(getter(\type, name)) = "abstract <toString(\type)> <name>()";
default str dec(Argument a) { throw "You forgot <a>!"; }
/***/
str dec(list[Argument] xs) = intercalate(", ", mapper(xs, dec));

// convertions
Argument asField(getter(\type, name)) = field(\type, name);
Argument asField(f:field(_, _)) = f;
default Argument asField(Argument a) { throw "You forgot <a>!"; }
/***/
list[Argument] asFieldList(list[Argument] xs) = mapper(xs, asField);

str toString(\map()) = "Map";
str toString(\set()) = "Set";
str toString(\vector()) = "Vector";
default str toString(DataStructure ds) { throw "You forgot <ds>!"; }


/*
 * Convenience Functions [TODO: remove global state dependency!]
 */
list[Argument] payloadTriple(int i) {
	if (ds == \map()) {
		return [ keyPos(i), key(i), val(i) ];
	} else { 
		return [ keyPos(i), key(i) ];
	}
}

list[Argument] payloadTriple(str posName) {
	if (ds == \map()) {
		return [ field("byte", posName), key(), val() ];
	} else { 
		return [ field("byte", posName), key() ];
	}
}

list[Argument] payloadTriple(str posName, int i) {
	if (ds == \map()) {
		return [ field("byte", posName), key(i), val(i) ];
	} else { 
		return [ field("byte", posName), key(i) ];
	}
}

list[Argument] payloadTriple(str posName, str keyName, str valName) {
	if (ds == \map()) {
		return [ field("byte", posName), key(keyName), val(valName) ];
	} else { 
		return [ field("byte", posName), key(keyName) ];
	}
} 

list[Argument] subnodePair(int i) = [ nodePos(i), \node(ds, i) ];

str AbstractNode(DataStructure ds) = "Abstract<toString(ds)>Node";
str CompactNode(DataStructure ds) = "Compact<toString(ds)>Node";


str Generics(DataStructure ds:\map()) = "" when !isGeneric(key()) && !isGeneric(val());
str Generics(DataStructure ds:\map()) = "\<<toString(primitiveToClass(key()).\type)>, <toString(primitiveToClass(val()).\type)>\>" when !!isGeneric(key()) && !!isGeneric(val());
str Generics(DataStructure ds:\map()) = "\<<toString(primitiveToClass(val()).\type)>\>" when  !isGeneric(key()) && !!isGeneric(val());
str Generics(DataStructure ds:\map()) = "\<<toString(primitiveToClass(key()).\type)>\>" when !!isGeneric(key()) &&  !isGeneric(val());
/***/
str Generics(DataStructure ds:\set()) = "" when !isGeneric(key());
str Generics(DataStructure ds:\set()) = "\<<toString(primitiveToClass(key()).\type)>\>" when !!isGeneric(key());
/***/
str Generics(DataStructure ds:\vector()) = "" when !isGeneric(val());
str Generics(DataStructure ds:\vector()) = "\<<toString(primitiveToClass(val()).\type)>\>" when !!isGeneric(val());
/***/
default str Generics(DataStructure _) { throw "Ahhh"; }


str InferredGenerics() = "" when !isGeneric(key()) && !isGeneric(val());
default str InferredGenerics() = "\<\>";

str GenericsExpanded(DataStructure ds:\vector()) = "\<<toString(primitiveToClass(key()).\type)>, <toString(primitiveToClass(val()).\type)>\>";
str GenericsExpanded(DataStructure ds:\map()) = "\<<toString(primitiveToClass(key()).\type)>, <toString(primitiveToClass(val()).\type)>\>";
str GenericsExpanded(DataStructure ds:\set()) = "\<<toString(primitiveToClass(key()).\type)>\>";

str UnifiedGenericsExpanded(DataStructure ds:\map()) = "\<<toString(primitiveToClass(key()).\type)>, <toString(primitiveToClass(val()).\type)>\>";
str UnifiedGenericsExpanded(DataStructure ds:\set()) = "\<<toString(primitiveToClass(key()).\type)>, java.lang.Void\>";
str UnifiedGenericsExpanded(DataStructure ds:\vector()) = "\<<toString(primitiveToClass(key()).\type)>, <toString(primitiveToClass(val()).\type)>\>";
default str UnifiedGenericsExpanded(DataStructure _) { throw "Ahhh"; }


str GenericsExpandedReversed(DataStructure ds:\map()) = "\<<toString(primitiveToClass(val()).\type)>, <toString(primitiveToClass(key()).\type)>\>";
str GenericsExpandedReversed(DataStructure ds:\set()) = GenericsExpanded(ds);

str GenericsExpandedUpperBounded(DataStructure ds:\map()) = "\<? extends <toString(primitiveToClass(key()).\type)>, ? extends <toString(primitiveToClass(val()).\type)>\>";
str GenericsExpandedUpperBounded(DataStructure ds:\set()) = "\<? extends <toString(primitiveToClass(key()).\type)>\>";

str GenericsDec(DataStructure ds:\map()) = "\<K <GenericsDecExtentionForPrimitives(key())>, V <GenericsDecExtentionForPrimitives(val())>\>";
str GenericsDec(DataStructure ds:\set()) = "\<K <GenericsDecExtentionForPrimitives(key())>\>";
str GenericsDecExtentionForPrimitives(Argument a) = "extends <primitiveToClass(a)>" when !isGeneric(a);
default str GenericsDecExtentionForPrimitives(Argument _) = "";

str ResultGenerics(DataStructure ds:\map()) = "\<<toString(primitiveToClass(key()).\type)>, <toString(primitiveToClass(val()).\type)>, ? extends <CompactNode(ds)><Generics(ds)>\>";
str ResultGenerics(DataStructure ds:\vector()) = "\<<toString(primitiveToClass(key()).\type)>, <toString(primitiveToClass(val()).\type)>, ? extends <CompactNode(ds)><Generics(ds)>\>";
str ResultGenerics(DataStructure ds:\set()) = "\<<toString(primitiveToClass(key()).\type)>, Void, ? extends <CompactNode(ds)><Generics(ds)>\>";
default str ResultGenerics(DataStructure _) { throw "Ahhh"; }

str ResultGenericsDec(DataStructure ds:\map()) = "\<K <GenericsDecExtentionForPrimitives(key().\type)>, V <GenericsDecExtentionForPrimitives(val().\type)>, ? extends <CompactNode(ds)><Generics(ds)>\>";
str ResultGenericsDec(DataStructure ds:\set()) = "\<K <GenericsDecExtentionForPrimitives(key().\type)>, Void, ? extends <CompactNode(ds)><Generics(ds)>\>";


str MapsToGenerics(DataStructure ds:\map())    = "\<<toString(primitiveToClass(val()).\type)>\>";
str MapsToGenerics(DataStructure ds:\set())    = "\<<toString(primitiveToClass(key()).\type)>\>";
str MapsToGenerics(DataStructure ds:\vector()) = "\<<toString(primitiveToClass(val()).\type)>\>";
default str MapsToGenerics(DataStructure _) { throw "Ahhh"; }
/***/
str dsAtFunction__domain_type(DataStructure ds:\map())    = toString(key().\type);
str dsAtFunction__domain_type(DataStructure ds:\set())    = toString(key().\type);
str dsAtFunction__domain_type(DataStructure ds:\vector()) = toString(key().\type);
default str dsAtFunction__domain_type(_) { throw "Ahhh"; }
/***/
str dsAtFunction__range_type(DataStructure ds:\map())    = toString(val().\type);
str dsAtFunction__range_type(DataStructure ds:\set())    = toString(key().\type);
str dsAtFunction__range_type(DataStructure ds:\vector()) = toString(val().\type);
default str dsAtFunction__range_type(_) { throw "Ahhh"; }



str SupplierIteratorGenerics(DataStructure ds:\vector())= GenericsExpanded(ds);
str SupplierIteratorGenerics(DataStructure ds:\map())	= GenericsExpanded(ds);
str SupplierIteratorGenerics(DataStructure ds:\set())	= "\<K, K\>";

str SupplierIteratorGenericsReversed(DataStructure ds) = GenericsExpandedReversed(ds);

str QuestionMarkGenerics(DataStructure ds:\map()) = "" when !isGeneric(key().\type) && !isGeneric(val().\type);
str QuestionMarkGenerics(DataStructure ds:\map()) = "\<?, ?\>" when !!isGeneric(key().\type) && !!isGeneric(val().\type);
str QuestionMarkGenerics(DataStructure ds:\map()) = "\<?\>" when !!isGeneric(key().\type) && !isGeneric(val().\type);
str QuestionMarkGenerics(DataStructure ds:\map()) = "\<?\>" when !isGeneric(key().\type) && !!isGeneric(val().\type);
default str QuestionMarkGenerics(DataStructure ds:\map()) = "\<\>";

str QuestionMarkGenerics(DataStructure ds:\set()) = "\<?\>";


/* 
 * Configuration 
 */
public str keyName = "key";
public str valName = "val";
public str cmpName = "cmp"; 

public str slotName = "slot";

public str nodeName = "node";
public str nodePosName = "npos";

public str nestedResult = "nestedResult";

public str keyPosName = "pos";

str equalityDefault(str x, str y) = "<x>.equals(<y>)"; // TODO: remove

str equalityDefaultForArguments(Argument x, Argument y) = "<use(x)> == <use(y)>"
	when x.\type == y.\type && isPrimitive(x.\type) && isPrimitive(y.\type);
str equalityDefaultForArguments(Argument x, Argument y) = "<use(x)>.equals(<use(y)>)"
	when x.\type == y.\type && !isPrimitive(x.\type) && !isPrimitive(y.\type);
default str equalityDefaultForArguments(Argument x, Argument y) { throw "Ahhh"; }
	

str equalityComparator(str x, str y) = "<cmpName>.compare(<x>, <y>) == 0"; // TODO: remove

str equalityComparatorForArguments(Argument x, Argument y) = "<use(x)> == <use(y)>"
	when x.\type == y.\type && isPrimitive(x.\type) && isPrimitive(y.\type);
str equalityComparatorForArguments(Argument x, Argument y) = "<cmpName>.compare(<use(x)>, <use(y)>) == 0"
	when x.\type == y.\type && !isPrimitive(x.\type) && !isPrimitive(y.\type);
default str equalityComparatorForArguments(Argument x, Argument y) { throw "Ahhh"; }	

/*
 * Mainly CompactNode specifics
 */
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:true, bool values:true) = "CompactMixed<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:true, bool values:false) = "CompactNodesOnly<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:false, bool values:true) = "CompactValuesOnly<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:false, bool values:false) = "CompactEmpty<toString(ds)>Node";
default str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes, bool values) { throw "Ahhh"; }

list[Argument] metadataArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ ___bitmapField(bitPartitionSize), ___valmapField(bitPartitionSize) ]
	;

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ key(i), val(i) | i <- [1..m+1]] 
	+ [ \node(ds, i)   | i <- [1..n+1]]
when (ds == \map() || ds == \vector()) 
		&& !isOptionEnabled(setup,useUntypedVariables());	

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ slot(i) | i <- [0..2*m + n]]
when (ds == \map() || ds == \vector()) 
		&& isOptionEnabled(setup,useUntypedVariables());

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ key(i)         | i <- [1..m+1]] 
	+ [ \node(ds, i)   | i <- [1..n+1]]
when (ds == \set()) 
		&& !isOptionEnabled(setup,useUntypedVariables());	

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ slot(i) | i <- [0..1*m + n]]
when (ds == \set()) 
		&& isOptionEnabled(setup,useUntypedVariables());

list[Argument] payloadTuple(ts:___expandedTrieSpecifics(ds:\vector(),	bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str name) = [ key(name), val(name) ];
list[Argument] payloadTuple(ts:___expandedTrieSpecifics(ds:\map(),		bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str name) = [ key(name), val(name) ];
list[Argument] payloadTuple(ts:___expandedTrieSpecifics(ds:\set(),		bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str name) = [ key(name) ];
/***/
list[Argument] payloadTuple(ts:___expandedTrieSpecifics(ds:\vector(), 	bitPartitionSize, nMax, nBound), rel[Option,bool] setup, int i) = [ key(i), val(i) ];
list[Argument] payloadTuple(ts:___expandedTrieSpecifics(ds:\map(), 		bitPartitionSize, nMax, nBound), rel[Option,bool] setup, int i) = [ key(i), val(i) ];
list[Argument] payloadTuple(ts:___expandedTrieSpecifics(ds:\set(), 		bitPartitionSize, nMax, nBound), rel[Option,bool] setup, int i) = [ key(i) ];
/***/
list[Argument] payloadTuple(ts:___expandedTrieSpecifics(ds:\vector(), 	bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = [ key(), val() ];
list[Argument] payloadTuple(ts:___expandedTrieSpecifics(ds:\map(),		bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = [ key(), val() ];
list[Argument] payloadTuple(ts:___expandedTrieSpecifics(ds:\set(),		bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = [ key() ];
/***/
default list[Argument] payloadTuple(_, _) { throw "Ahhh"; }

//list[Argument] untypedPayloadTuple(ts:___expandedTrieSpecifics(ds:\vector(),	bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str name) = [ key(name), val(name) ];
//list[Argument] untypedPayloadTuple(ts:___expandedTrieSpecifics(ds:\map(),		bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str name) = [ key(name), val(name) ];
//list[Argument] untypedPayloadTuple(ts:___expandedTrieSpecifics(ds:\set(),		bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str name) = [ key(name) ];
/***/
list[Argument] untypedPayloadTuple(ts:___expandedTrieSpecifics(ds:\vector(), 	bitPartitionSize, nMax, nBound), rel[Option,bool] setup, int i) = [ slot(i), slot(i+1) ];
list[Argument] untypedPayloadTuple(ts:___expandedTrieSpecifics(ds:\map(), 		bitPartitionSize, nMax, nBound), rel[Option,bool] setup, int i) = [ slot(i), slot(i+1) ];
list[Argument] untypedPayloadTuple(ts:___expandedTrieSpecifics(ds:\set(), 		bitPartitionSize, nMax, nBound), rel[Option,bool] setup, int i) = [ slot(i) ];
/***/
//list[Argument] untypedPayloadTuple(ts:___expandedTrieSpecifics(ds:\vector(), 	bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = [ key(), val() ];
//list[Argument] untypedPayloadTuple(ts:___expandedTrieSpecifics(ds:\map(),		bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = [ key(), val() ];
//list[Argument] untypedPayloadTuple(ts:___expandedTrieSpecifics(ds:\set(),		bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = [ key() ];
/***/
default list[Argument] untypedPayloadTuple(_, _) { throw "Ahhh"; }

str containsKeyMethodName(DataStructure ds:\map()) = "containsKey";
str containsKeyMethodName(DataStructure ds:\set()) = "contains";
str containsKeyMethodName(DataStructure ds:\vector()) = "containsKey";
default str containsKeyMethodName(_) { throw "Ahhh"; }

str insertOrPutMethodName(DataStructure ds:\map()) = "__put";
str insertOrPutMethodName(DataStructure ds:\set()) = "__insert";
str insertOrPutMethodName(DataStructure ds:\vector()) = "__put";
default str insertOrPutMethodName(_) { throw "Ahhh"; }

int tupleLength(DataStructure ds:\map()) = 2;
int tupleLength(DataStructure ds:\set()) = 1;
int tupleLength(DataStructure ds:\vector()) = 2;
default int tupleLength(_) { throw "Ahhh"; }

public Argument tupleLengthConstant = field(primitive("int"), "TUPLE_LENGTH"); // TODO: get rid of public state

// TODO: move to List.rsc?
list[&T] times(&T template, int count) 
	= [ template | i <- [1..count]];