require 'benchmark'

total = 10000000

public

def m; nil; end
def t; true; end
def f; false; end
def s; self; end
def sym; :a; end

def add(x); x + 1; end

def cached(x); x.m; end

Benchmark.bm do |x|
  x.report("nil") do
    total.times { m }
  end

  x.report("true") do
    total.times { t }
  end

  x.report("false") do
    total.times { f }
  end

  x.report("self") do
    total.times { s }
  end

  x.report("symbol") do
    total.times { sym }
  end
end

Benchmark.bm do |x|
  x.report("add") do
    total.times { add(1) }
  end

  x.report("cached") do
    total.times { cached(self) }
  end
end
