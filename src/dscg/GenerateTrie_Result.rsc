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
module dscg::GenerateTrie_Result

import dscg::Common;
import dscg::GenerateTrie;

str generateResultClassString(TrieSpecifics ts, rel[Option,bool] setup) {

	className = "Result";

	return
	"static final class <className><Generics(ts.ds)> {
		private <toString(dsAtFunction__range_type(ts.ds))> replacedValue;
		private boolean isModified;

		// update: inserted/removed single element, element count changed
		public void modified() {
			this.isModified = true;
		}

		public void updated(<toString(dsAtFunction__range_type(ts.ds))> replacedValue) {
			this.replacedValue = replacedValue;
			this.isModified = true;
		}

		// update: neither element, nor element count changed
		public static <Generics(ts.ds)> Result<Generics(ts.ds)> unchanged() {
			return new Result\<\>(null, false);
		}

		private Result(<toString(dsAtFunction__range_type(ts.ds))> replacedValue, boolean isMutated) {
			this.replacedValue = replacedValue;
			this.isModified = isMutated;
		}

		public boolean isModified() {
			return isModified;
		}

		public boolean hasReplacedValue() {
			return replacedValue != null;
		}

		public <toString(dsAtFunction__range_type(ts.ds))> getReplacedValue() {
			return replacedValue;
		}
	}";
}
