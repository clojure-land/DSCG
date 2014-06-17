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
module dscg::GenerateTrie_Optional

import dscg::GenerateTrie;

str generateOptionalClassString() {

	className = "Optional";

	return
	"abstract static class <className>\<T\> {
		private static final Optional EMPTY = new Optional() {
			@Override
			boolean isPresent() {
				return false;
			}

			@Override
			Object get() {
				return null;
			}
		};

		@SuppressWarnings(\"unchecked\")
		static \<T\> Optional\<T\> empty() {
			return EMPTY;
		}

		static \<T\> Optional\<T\> of(T value) {
			return new Value\<T\>(value);
		}

		abstract boolean isPresent();

		abstract T get();

		private static final class Value\<T\> extends Optional\<T\> {
			private final T value;

			private Value(T value) {
				this.value = value;
			}

			@Override
			boolean isPresent() {
				return true;
			}

			@Override
			T get() {
				return value;
			}
		}
	}";
}
