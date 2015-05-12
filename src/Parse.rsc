module Parse

import Syntax;
import ParseTree;

public start[FeatureDiagram] parseFeatureDiagram(str src, loc file) = 
  parse(#start[FeatureDiagram], src, file);
  
public start[FeatureDiagram] parseFeatureDiagram(loc file) = 
  parse(#start[FeatureDiagram], file);
  
public start[FeatureDiagram] parseExample(str name) =
	parseFeatureDiagram(|project://feature-diagram-language/input/<name>.fdl|);