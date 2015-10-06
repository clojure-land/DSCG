module dscg::Common_ContentType

import List;
import String;

import dscg::Common;
	
Expression ctToConstant(ContentType ct) = constant(specific("ContentType"), prettyPrint(ct));
str prettyPrint(ContentType ct) = "ContentType.<prettyPrintContentType(ct)>";

str prettyPrintContentType(ct:ctPayloadArg(int index)) = camelCaseToUpperCaseWithUnderscores(contentArgIdPrefix(index, isRare = ct.isRare));
//str prettyPrintContentType(ct:ctKey()) = ct.isRare ? "RARE_KEY" : "KEY";
//str prettyPrintContentType(ct:ctVal()) = ct.isRare ? "RARE_VAL" : "VAL";
str prettyPrintContentType(ct:ctNode()) = "NODE";
str prettyPrintContentType(ct:ctSlot()) = "SLOT"; 

str contentAccessorMethodName(ct:ctPayloadArg(int index)) = "get<capitalize(contentArgIdPrefix(index, isRare = ct.isRare))>";
//str contentAccessorMethodName(ct:ctKey()) = ct.isRare ? "getRareKey" : "getKey";
//str contentAccessorMethodName(ct:ctVal()) = ct.isRare ? "getRareVal" : "getVal";
str contentAccessorMethodName(ct:ctNode()) = "getNode";
str contentAccessorMethodName(ct:ctSlot()) = "getSlot";

list[str] splitByUpperCaseCharacters(str input) {
	list[str] strings = [];	
		
	str current = "";
	for (i <- chars(input), c := stringChars([i])) {
		if (toUpperCase(c) != c) {
			current = "<current><c>";
		} else {
			// flush
			if (current != "") {
				strings += current;
			}
						
			current = "<c>";
		}
	}

	// flush	
	if (current != "") {
		strings += current;
	}
	
	return strings;
}

str camelCaseToUpperCaseWithUnderscores(str input) = intercalate("_", [ toUpperCase(charSeq) | charSeq <- splitByUpperCaseCharacters(input) ]);