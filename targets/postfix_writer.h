#ifndef __MML_TARGETS_POSTFIX_WRITER_H__
#define __MML_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <cdk/emitters/basic_postfix_emitter.h>
#include <set>
#include <sstream>

typedef int lbl;

namespace mml {

//!
//! Traverse syntax tree and generate the corresponding assembly code.
//!
class postfix_writer : public basic_ast_visitor {
  cdk::symbol_table<mml::symbol> &_symtab;

  std::set<std::string> _functionsToDeclare;
  std::set<std::string> _symbolsToDeclare;

  // code generation
  cdk::basic_postfix_emitter &_pf;
  int _lbl;

  // semantic analysis
  bool _inFunctionBody = false;
  bool _inFunctionArgs = false;
  bool _mainReturnSeen = false;
  bool _returnSeen = false;
  // while labels -- for break/continue; work like stacks
  std::vector<lbl> _whileCond, _whileEnd;

  // if we want to return to a function's segment, we need to know the label
  std::vector<std::string> _functionLabels;
  // a given forwarded function's label, as we want to call it, not branch to it
  std::string _currentForwardLabel;
  // for keeping track of functions and their arguments
  // works like a stack
  std::vector<std::shared_ptr<mml::symbol>> _functions;

  int _offset = 0; // current frame pointer offset -- 0 means no vars defined

public:
  postfix_writer(std::shared_ptr<cdk::compiler> compiler,
                 cdk::symbol_table<mml::symbol> &symtab,
                 cdk::basic_postfix_emitter &pf)
      : basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0) {}

public:
  ~postfix_writer() { os().flush(); }

private:
  /** Method used to generate sequential labels. */
  inline std::string mklbl(int lbl) {
    std::ostringstream oss;
    if (lbl < 0)
      oss << ".L" << -lbl;
    else
      oss << "_L" << lbl;
    return oss.str();
  }

  /** Method use to print error messages. */
  void error(int lineno, std::string e) {
    std::cerr << "[ERROR @ " << lineno << "]: " << e << std::endl;
  }

protected:
  void processIDPBinaryExpression(cdk::binary_operation_node *const node,
                                  int lvl);
  void processIDBinaryExpression(cdk::binary_operation_node *const node,
                                 int lvl);
  void
  processGeneralLogicalBinaryExpression(cdk::binary_operation_node *const node,
                                        int lvl);

  void
  processLocalVariableInitialization(std::shared_ptr<mml::symbol> symbol,
                                     cdk::expression_node *const initializer,
                                     int lvl);
  void
  processGlobalVariableInitialization(std::shared_ptr<mml::symbol> symbol,
                                      cdk::expression_node *const initializer,
                                      int lvl);

  void processMainFunction(mml::function_definition_node *const node, int lvl);
  void processNonMainFunction(mml::function_definition_node *const node,
                              int lvl);

public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end
};

} // namespace mml

#endif
