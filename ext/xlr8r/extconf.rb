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

  IO.popen "nm -P #{file}", "r" do |io|
    io.each_line do |l|
      name, type, addr = l.split(" ")
      symbols[name] = addr.to_i(16)
    end
  end

  sym_apply = symbols["_rb_apply"]
  @sym_apply = sym_apply

  c0 = sym_apply - symbols["_rb_call0"]

  f.puts "#define OFFSET_CALL0 #{c0}"

  ri = sym_apply - symbols["_ruby_iter"]

  f.puts "#define OFFSET_RUBY_ITER #{ri}"

  re = sym_apply - symbols["_rb_eval"]

  f.puts "#define OFFSET_RB_EVAL #{re}"

  fu = sym_apply - symbols["_frame_unique"]

  f.puts "#define OFFSET_FRAME_UNIQUE #{fu}"

  pt = sym_apply - symbols["_prot_tag"]

  f.puts "#define OFFSET_PROT_TAG #{pt}"

  pt = sym_apply - symbols["_top_scope"]

  f.puts "#define OFFSET_TOP_SCOPE #{pt}"

  eh = sym_apply - symbols["_event_hooks"]

  f.puts "#define OFFSET_EVENT_HOOKS #{eh}"

  sv = sym_apply - symbols["_scope_vmode"]

  f.puts "#define OFFSET_SCOPE_VMODE #{sv}"

  as = sym_apply - symbols["_assign"]

  f.puts "#define OFFSET_ASSIGN #{as}"

  cl = sym_apply - symbols["_rb_call"]

  f.puts "#define OFFSET_RB_CALL #{cl}"

  rb = sym_apply - symbols["_ruby_block"]

  f.puts "#define OFFSET_RUBY_BLOCK #{rb}"

  bu = sym_apply - symbols["_block_unique"]

  f.puts "#define OFFSET_BLOCK_UNIQUE #{bu}"

  bf = sym_apply - symbols["_blk_free"]

  f.puts "#define OFFSET_BLK_FREE #{bf}"

  rj = sym_apply - symbols["_return_jump"]

  f.puts "#define OFFSET_RETURN_JUMP #{rj}"

  add f, "proc_invoke"
  add f, "umethod_bind"
  add f, "method_call"
end

$CFLAGS.gsub!(/-O\d/,"-O0") if ENV['DEBUG']

$CFLAGS << " -Wall"

create_makefile "xlr8r_ext"
