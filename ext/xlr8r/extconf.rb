require 'mkmf'
require 'rbconfig'

C = RbConfig::CONFIG

ruby = File.join(C['bindir'], "#{C['ruby_install_name']}#{C['EXEEXT']}")

def add(f, name, const=nil)
  diff = @sym_apply - @symbols["_#{name}"]
  const ||= name.upcase

  f.puts "#define OFFSET_#{const} #{diff}"
end

File.open "config.h", "w" do |f|

  file = ruby

  @symbols = symbols = {}

  begin
    IO.popen "nm -P #{file}", "r" do |io|
      io.each_line do |l|
        name, type, addr = l.split(" ")
        symbols[name] = addr.to_i(16)
      end
    end

    sym_apply = symbols["_rb_apply"]
    @sym_apply = sym_apply

    add f, "rb_call0", "CALL0"
    add f, "ruby_iter"
    add f, "rb_eval"
    add f, "frame_unique"
    add f, "prot_tag"
    add f, "top_scope"
    add f, "event_hooks"
    add f, "scope_vmode"
    add f, "assign"
    add f, "rb_call"
    add f, "ruby_block"
    add f, "block_unique"
    add f, "blk_free"
    add f, "return_jump"
    add f, "proc_invoke"
    add f, "umethod_bind"
    add f, "method_call"
  rescue Exception
    f.puts "#define DISABLE_XLR8R 1"
  end
end

$CFLAGS.gsub!(/-O\d/,"-O0") if ENV['DEBUG']

$CFLAGS << " -Wall"

create_makefile "xlr8r_ext"
