= xlr8r

* http://github.com/evanphx/xlr8r

== DESCRIPTION:

Ruby 1.8 is done from the perspective of ruby-core. But there are a lot of
people and businesses still using it.

So what ruby 1.8 needs is a turbocharger. A new fuel injection system.
An engine rebuild. And thats what xlr8r is.

xlr8r is a bytecode engine that hot-patches itself into MRI Ruby 1.8 to
speed up running ruby code.

== FEATURES/PROBLEMS:

* Bytecode engine
* Inline method caches
* Fixnum shortcut optimizations
* Installable as a gem into any ruby 1.8 build with debug symbols
* On any ruby not supported, automatically disables itself
  * Allows xlr8r to be put into any Gemfile

== SYNOPSIS:

  require 'xlr8r'

== REQUIREMENTS:

* MRI Ruby 1.8 with debug symbols

== INSTALL:

* gem install xlr8r

== DEVELOPERS:

After checking out the source, run:

  $ rake newb

This task will install any missing dependencies, run the tests/specs,
and generate the RDoc.

== LICENSE:

(The MIT License)

Copyright (c) 2012 FIX

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
'Software'), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
