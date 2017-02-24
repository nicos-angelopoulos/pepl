0. Licence
---
This software is distributed under the MIT licence.


1. Introduction
---

These programs (Pepl) implement and demonstrate parameter estimation
(PE) in SLPs, using the FAM algorithm. For more information on how
to use these programs see, doc/user_guide.ps.gz. In doc/pfam.ps.gz
are some reading notes on the implementation of the FAM algorithm.

2. Installation
---

You most likely already done that. Pe-pl is distributed as Prolog
source code that can be run on the following Prolog systems:

	Yap (tested on 6.3)
			http://www.dcc.fc.up.pt/~vsc/Yap
	Swi tested on 7.1.4.
			http://www.swi-prolog.org/

   Older versions are known to run on: 

	SICStus (last tested on 3.10, Pepl sources are now well out of date)
			http://www.sics.se/isl/sicstus.html

	On Swi you can install with 
		?- pack_install( pepl ).

	On Yap simply download the latest sources from
		http://stoics.org.uk/~nicos/sware/pepl
	then gunzip and untar the downloaded file.

3. Quick start.
---

On SWI, start swipl, and

[library(pepl)].

[pack('pepl/examples/main')]

main.
main_store.
main_sample.
main_exact. (alias for main)

On Yap, cd to the Pepl's examples directory start yap, and then

[main].
main.
main_store.
main_sample.
main_exact. (alias for main)

See main_* files in examples/ for more examples and doc/ for documentation.

4. Contact
---

Nicos Angelopoulos, http://stoics.org.uk/~nicos

London, February 2017
