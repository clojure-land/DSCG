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
Argument key()		= field("K", "<keyName>");
Argument key(int i) = field("K", "<keyName><i>");
Argument val()		= field("V", "<valName>");
Argument val(int i) = field("V", "<valName><i>");

Argument nodePos(int i) = field("byte", "<nodePosName><i>");
Argument \node(DataStructure ds)		= field("<CompactNode(ds)><Generics(ds)>", "<nodeName>");
Argument \node(DataStructure ds, int i) = field("<CompactNode(ds)><Generics(ds)>", "<nodeName><i>");

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

str chunkSizeToObject(int _:3) = "Byte";
str chunkSizeToObject(int _:4) = "Short";
str chunkSizeToObject(int _:5) = "Integer";
str chunkSizeToObject(int _:6) = "Long";

str integerOrLongObject(int _:6) = "Long";
str integerOrLongObject(int _:n) = "Integer" when n > 0 && n < 6;

// convert either to int or to long and take care of unsigned conversion 
str useSafeUnsigned(Argument a) = "(int)(<use(a)> & 0xFF)"   when a has \type && a.\type == "byte";
str useSafeUnsigned(Argument a) = "(int)(<use(a)> & 0xFFFF)" when a has \type && a.\type == "short";
str useSafeUnsigned(Argument a) = "<use(a)>"                 when a has \type && a.\type == "int";
str useSafeUnsigned(Argument a) = "<use(a)>" when a has \type && a.\type == "long";
default str useSafeUnsigned(Argument a) { throw "ahhh"; }

str primitiveHashCode(Argument a) = "(int)(<use(a)> ^ (<use(a)> \>\>\> 32))" when a has \type && a.\type == "long";
default str primitiveHashCode(Argument a) = "(int) <use(a)>";

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
		return [ field("byte", posName), field("K", keyName), field("V", valName) ];
	} else { 
		return [ field("byte", posName), field("K", keyName) ];
	}
} 

list[Argument] subnodePair(int i) = [ nodePos(i), \node(ds, i) ];

str AbstractNode(DataStructure ds) = "Abstract<toString(ds)>Node";
str CompactNode(DataStructure ds) = "Compact<toString(ds)>Node";

str Generics(DataStructure ds:\map) = "\<K, V\>";
str Generics(DataStructure ds:\set) = "\<K\>";

str ResultGenerics(DataStructure ds:\map) = "\<K, V, ? extends <CompactNode(ds)><Generics(ds)>\>";
str ResultGenerics(DataStructure ds:\set) = "\<K, Void, ? extends <CompactNode(ds)><Generics(ds)>\>";

str KeyOrMapEntryGenerics(DataStructure ds:\map) = "\<java.util.Map.Entry<Generics(ds)>\>";
str KeyOrMapEntryGenerics(DataStructure ds:\set) = "\<K\>";

str SupplierIteratorGenerics(DataStructure ds:\map) = "\<K, V\>";
str SupplierIteratorGenerics(DataStructure ds:\set) = "\<K, K\>";

str QuestionMarkGenerics(DataStructure ds:\map) = "\<?, ?\>";
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

str equalityDefault(str x, str y) = "<x>.equals(<y>)";

str equalityComparator(str x, str y) = "<cmpName>.compare(<x>, <y>) == 0";



str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:true, bool values:true) = "CompactMixed<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:true, bool values:false) = "CompactNodesOnly<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:false, bool values:true) = "CompactValuesOnly<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:false, bool values:false) = "CompactEmpty<toString(ds)>Node";
default str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes, bool values) { throw "Ahhh"; }
