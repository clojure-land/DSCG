module QuickFixer

import Syntax;

import ParseTree;
import IO;

str introduceFeature(Tree input, loc origin) {
	str toBeAddedFeature = getReferenceAt(input, origin);

	input = visit(input) {
		case FeatureDefinitions definitions => [FeatureDefinitions]"<definitions> 
			'	<toBeAddedFeature>: x"
	}
	
	return "<input>";
}

str getReferenceAt(&T<:Tree t, loc l) { 
	visit(t) {
		case FeatureName reference : if (reference@\loc == l) { return "<reference>"; }
	}
  
 	return "";
}

