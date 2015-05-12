module Load

import Parse;
import AST;
import ParseTree;

FeatureDiagram load(loc l) = implodeFDL(parseFeatureDiagram(l));
FeatureDiagram loadExample(str file) = load(|project://feature-diagram-language/input/<file>|);

FeatureDiagram implodeFDL(Tree fd) = implode(#FeatureDiagram, fd); 