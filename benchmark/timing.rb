require 'benchmark'

total = 10000000

class Test
  def add(a,b)
    a + b
  end

  def []=(*a)
    a.size
  end

  Ary = [1,2]

  def splat
    add *Ary
  end

  def push
    self[*Ary] = 3
  end
end

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

t = Test.new

Benchmark.bm do |x|
  x.report("add") do
    total.times { add(1) }
  end

  x.report("cached") do
    total.times { cached(self) }
  end

  x.report("splat") do
    total.times { t.splat }
  end

  x.report("push") do
    total.times { t.push }
  end
end
