/**
 * Copyright (c) 2015 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 */
package dscg;

import org.rascalmpl.value.IInteger;
import org.rascalmpl.value.IString;
import org.rascalmpl.value.IValueFactory;

public class Common {

	private final IValueFactory vf;

	public Common(IValueFactory vf) {
		this.vf = vf;
	}

	public IString toBinaryString(IInteger i) {
		return vf.string(java.lang.Integer.toBinaryString(i.intValue()));
	}

	public IInteger parseInt(IString s, IInteger radix) {
		return vf.integer(java.lang.Integer.parseInt(s.getValue(),
				radix.intValue()));
	}

}
