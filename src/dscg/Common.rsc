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

data Argument
	= field (str \type, str name)
	| getter(str \type, str name)
	;

data Option // TODO: finish!
	= useSpecialization()
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
Argument field (str name) = field ("???", name);
Argument getter(str name) = getter("???", name);

Argument keyPos(int i) = field("byte", "<keyPosName><i>");
Argument key()				= key("<keyName>");
Argument key(int i) 		= key("<keyName><i>");
Argument key(str name) 		= field("K", "<name>");
Argument val()				= val("<valName>");
Argument val(int i) 		= val("<valName><i>");
Argument val(str name) 		= field("V", "<name>");

Argument nodePos(int i) = field("byte", "<nodePosName><i>");
Argument \node(DataStructure ds)			= field("<CompactNode(ds)><Generics(ds)>", "<nodeName>");
Argument \node(DataStructure ds, int i) 	= field("<CompactNode(ds)><Generics(ds)>", "<nodeName><i>");
Argument \node(DataStructure ds, str name)	= field("<CompactNode(ds)><Generics(ds)>", name);

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

public Argument thisMutator = field("Void", "null");

str chunkSizeToPrimitive(int _:3) = "byte";
str chunkSizeToPrimitive(int _:4) = "short";
str chunkSizeToPrimitive(int _:5) = "int";
str chunkSizeToPrimitive(int _:6) = "long";

str chunkSizeToObject(int _:3) = "java.lang.Byte";
str chunkSizeToObject(int _:4) = "java.lang.Short";
str chunkSizeToObject(int _:5) = "java.lang.Integer";
str chunkSizeToObject(int _:6) = "java.lang.Long";

str integerOrLongObject(int _:6) = "java.lang.Long";
str integerOrLongObject(int _:n) = "java.lang.Integer" when n > 0 && n < 6;

// convert either to int or to long and take care of unsigned conversion 
str useSafeUnsigned(Argument a) = "(int)(<use(a)> & 0xFF)"   when a has \type && a.\type == "byte";
str useSafeUnsigned(Argument a) = "(int)(<use(a)> & 0xFFFF)" when a has \type && a.\type == "short";
str useSafeUnsigned(Argument a) = "<use(a)>"                 when a has \type && a.\type == "int";
str useSafeUnsigned(Argument a) = "<use(a)>" when a has \type && a.\type == "long";
default str useSafeUnsigned(Argument a) { throw "ahhh"; }

str hashCode(Argument a) = primitiveHashCode(a) when isPrimitive(a.\type);
default str hashCode(Argument a) = "<use(a)>.hashCode()";

str primitiveHashCode(Argument a) = "(int)(<use(a)> ^ (<use(a)> \>\>\> 32))" when a has \type && a.\type == "long";
default str primitiveHashCode(Argument a) = "(int) <use(a)>";




Argument primitiveToClass(field("byte", name))  = field("java.lang.Byte", name);
Argument primitiveToClass(field("short", name)) = field("java.lang.Short", name);
Argument primitiveToClass(field("int", name))   = field("java.lang.Integer", name);
Argument primitiveToClass(field("long", name))  = field("java.lang.Long", name);
/***/
Argument primitiveToClass(getter("byte", name))  = getter("java.lang.Byte", name);
Argument primitiveToClass(getter("short", name)) = getter("java.lang.Short", name);
Argument primitiveToClass(getter("int", name))   = getter("java.lang.Integer", name);
Argument primitiveToClass(getter("long", name))  = getter("java.lang.Long", name);
/***/
default Argument primitiveToClass(Argument nonPrimitive)  = nonPrimitive;



bool isPrimitive("byte")  = true;
bool isPrimitive("short") = true;
bool isPrimitive("int")   = true;
bool isPrimitive("long")  = true;
/***/
bool isPrimitive(Argument a)  = isPrimitive(a.\type);
/***/
default bool isPrimitive(str _)  = false;



bool isPrimitiveArray("byte[]")  = true;
bool isPrimitiveArray("short[]") = true;
bool isPrimitiveArray("int[]")   = true;
bool isPrimitiveArray("long[]")  = true;
/***/
bool isPrimitiveArray(Argument a)  = isPrimitiveArray(a.\type);
/***/
default bool isPrimitiveArray(str _)  = false;



/*
 * Functions
 */
str use(field(_, name)) = name;
str use(getter(_, name)) = "<name>()";
default str use(Argument a) { throw "You forgot <a>!"; }
/***/
str use(list[Argument] xs) = intercalate(", ", mapper(xs, use));

str dec(field(\type, name)) = "final <\type> <name>";
str dec(getter(\type, name)) = "abstract <\type> <name>()";
default str dec(Argument a) { throw "You forgot <a>!"; }
/***/
str dec(list[Argument] xs) = intercalate(", ", mapper(xs, dec));

// convertions
Argument asField(getter(\type, name)) = field(\type, name);
Argument asField(f:field(_, _)) = f;
default Argument asField(Argument a) { throw "You forgot <a>!"; }
/***/
list[Argument] asFieldList(list[Argument] xs) = mapper(xs, asField);

default str toString(\map()) = "Map";
default str toString(\set()) = "Set";
default str toString(\vector()) = "Vector";
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

str Generics(DataStructure ds:\map) = "" when isPrimitive(key()) && isPrimitive(val());
str Generics(DataStructure ds:\map) = "\<<primitiveToClass(key()).\type>, <primitiveToClass(val()).\type>\>" when !isPrimitive(key()) && !isPrimitive(val());
str Generics(DataStructure ds:\map) = "\<<primitiveToClass(val()).\type>\>" when  isPrimitive(key()) && !isPrimitive(val());
str Generics(DataStructure ds:\map) = "\<<primitiveToClass(key()).\type>\>" when !isPrimitive(key()) &&  isPrimitive(val());
//default str Generics(DataStructure ds:\map) = "\<<primitiveToClass(key())>, <primitiveToClass(val())>\>";

//str Generics(DataStructure ds:\map) = "\<<primitiveToClass(key())>, <primitiveToClass(val())>\>" when isPrimitive(key()) && isPrimitive(val());
//str Generics(DataStructure ds:\map) = "\<<primitiveToClass(val())>\>" when isPrimitive(key()) && !isPrimitive(val());
//str Generics(DataStructure ds:\map) = "\<<primitiveToClass(key())>\>" when !isPrimitive(key()) && isPrimitive(val());

//str Generics(DataStructure ds:\set) = "\<<primitiveToClass(key())>\>"; // TODO


str InferredGenerics() = "" when isPrimitive(key().\type) && isPrimitive(val().\type);
default str InferredGenerics() = "\<\>";

str GenericsExpanded(DataStructure ds:\map) = "\<<primitiveToClass(key()).\type>, <primitiveToClass(val()).\type>\>";
str GenericsExpandedReversed(DataStructure ds:\map) = "\<<primitiveToClass(val()).\type>, <primitiveToClass(key()).\type>\>";

str GenericsExpandedUpperBounded(DataStructure ds:\map) = "\<? extends <primitiveToClass(key()).\type>, ? extends <primitiveToClass(val()).\type>\>";

str GenericsDec(DataStructure ds:\map) = "\<K <GenericsDecExtentionForPrimitives(key())>, V <GenericsDecExtentionForPrimitives(val())>\>";
str GenericsDec(DataStructure ds:\set) = "\<K <GenericsDecExtentionForPrimitives(key())>\>";
str GenericsDecExtentionForPrimitives(Argument a) = "extends <primitiveToClass(a)>" when isPrimitive(a);
default str GenericsDecExtentionForPrimitives(Argument _) = "";

str ResultGenerics(DataStructure ds:\map) = "\<<primitiveToClass(key()).\type>, <primitiveToClass(val()).\type>, ? extends <CompactNode(ds)><Generics(ds)>\>";
str ResultGenerics(DataStructure ds:\set) = "\<<primitiveToClass(key()).\type>, Void, ? extends <CompactNode(ds)><Generics(ds)>\>";

str ResultGenericsDec(DataStructure ds:\map) = "\<K <GenericsDecExtentionForPrimitives(key().\type)>, V <GenericsDecExtentionForPrimitives(val().\type)>, ? extends <CompactNode(ds)><Generics(ds)>\>";
str ResultGenericsDec(DataStructure ds:\set) = "\<K <GenericsDecExtentionForPrimitives(key().\type)>, Void, ? extends <CompactNode(ds)><Generics(ds)>\>";

str KeyOrMapEntryGenerics(DataStructure ds:\map) = "\<java.util.Map.Entry<GenericsExpanded(ds)>\>";
str KeyOrMapEntryGenerics(DataStructure ds:\set) = "\<K\>";

str SupplierIteratorGenerics(DataStructure ds:\map) = GenericsExpanded(ds);
str SupplierIteratorGenerics(DataStructure ds:\set) = "\<K, K\>";

str SupplierIteratorGenericsReversed(DataStructure ds:\map) = GenericsExpandedReversed(ds);

str QuestionMarkGenerics(DataStructure ds:\map) = "" when isPrimitive(key().\type) && isPrimitive(val().\type);
str QuestionMarkGenerics(DataStructure ds:\map) = "\<?, ?\>" when !isPrimitive(key().\type) && !isPrimitive(val().\type);
str QuestionMarkGenerics(DataStructure ds:\map) = "\<?\>" when !isPrimitive(key().\type) && isPrimitive(val().\type);
str QuestionMarkGenerics(DataStructure ds:\map) = "\<?\>" when isPrimitive(key().\type) && !isPrimitive(val().\type);
default str QuestionMarkGenerics(DataStructure ds:\map) = "\<\>";

str QuestionMarkGenerics(DataStructure ds:\set) = "\<?\>";


/* 
 * Configuration 
 */
public str keyName = "key";
public str valName = "val";
public str cmpName = "cmp"; 

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

str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:true, bool values:true) = "CompactMixed<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:true, bool values:false) = "CompactNodesOnly<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:false, bool values:true) = "CompactValuesOnly<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:false, bool values:false) = "CompactEmpty<toString(ds)>Node";
default str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes, bool values) { throw "Ahhh"; }
