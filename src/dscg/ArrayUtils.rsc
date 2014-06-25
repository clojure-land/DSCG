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
module dscg::ArrayUtils

import List;
import dscg::Common;

/*
 * usage: arraycopyAndInsertPair(field("Object[]", "src"), field("Object[]", "dst"), [field("K", "key"), field("V", "val")], field("int", "idx"));
 */
str arraycopyAndInsertPair(Argument src, Argument dst, list[Argument] new, Argument newAtIndex) {
	int tupleSize = size(new);	
	
	dst = field("Object[]", dst.name); 
	
	return
		"<dec(dst)> = new Object[<use(src)>.length + <tupleSize>];

		// copy \'<use(src)>\' and insert tuple at position \'<use(newAtIndex)>\'
		System.arraycopy(<use(src)>, 0, <use(dst)>, 0, <use(newAtIndex)>);
		<for (i <- [0..tupleSize]) {><use(dst)>[<use(newAtIndex)> + <i>] = <use(new[i])>;<}>
		System.arraycopy(<use(src)>, <use(newAtIndex)>, <use(dst)>, <use(newAtIndex)> + <tupleSize>, <use(src)>.length - <use(newAtIndex)>);
		";
}
