#ifndef __MML_TARGET_FRAME_SIZE_CALCULATOR_H__
#define __MML_TARGET_FRAME_SIZE_CALCULATOR_H__

#include "targets/basic_ast_visitor.h"

#include <sstream>
#include <stack>

namespace mml {

class frame_size_calculator : public basic_ast_visitor {
  cdk::symbol_table<mml::symbol> &_symtab;
  std::shared_ptr<mml::symbol> _function;

  size_t _localsize;
  size_t _retsize;
  bool _ret;

public:
  frame_size_calculator(std::shared_ptr<cdk::compiler> compiler,
                        cdk::symbol_table<mml::symbol> &symtab,
                        std::shared_ptr<mml::symbol> func)
      : basic_ast_visitor(compiler), _symtab(symtab), _function(func),
        _localsize(0), _retsize(0), _ret(false) {}

public:
  ~frame_size_calculator();

public:
  size_t localsize() const { return _localsize; }

  size_t retsize() const { return _retsize; }

public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end
};

} // namespace mml

#endif
