#ifndef __MML_TARGETS_TYPE_CHECKER_H__
#define __MML_TARGETS_TYPE_CHECKER_H__

#include "targets/basic_ast_visitor.h"
#include ".auto/all_nodes.h" // automatically generated

namespace mml {

/**
 * Print nodes as XML elements to the output stream.
 */
class type_checker : public basic_ast_visitor {
  cdk::symbol_table<mml::symbol> &_symtab;

  basic_ast_visitor *_parent;

public:
  type_checker(std::shared_ptr<cdk::compiler> compiler,
               cdk::symbol_table<mml::symbol> &symtab,
               basic_ast_visitor *parent)
      : basic_ast_visitor(compiler), _symtab(symtab), _parent(parent) {}

public:
  ~type_checker() { os().flush(); }

protected:
  bool compatible_ptr_types(std::shared_ptr<cdk::basic_type> t1,
                                   std::shared_ptr<cdk::basic_type> t2);
  bool compatible_fun_types(std::shared_ptr<cdk::functional_type> t1,
                                   std::shared_ptr<cdk::functional_type> t2);
  bool compatible_types(std::shared_ptr<cdk::basic_type> t1,
                               std::shared_ptr<cdk::basic_type> t2);
  void compatible_node_types(std::shared_ptr<cdk::basic_type> t_node,
                             std::shared_ptr<cdk::basic_type> t_field,
                             cdk::typename_type tname_node,
                             cdk::typename_type tname_field,
                             std::string field_name);
  bool processBinaryExpression(cdk::binary_operation_node *const node, int lvl);
  void processIBinaryExpression(cdk::binary_operation_node *const node,
                                int lvl);
  void processIDBinaryExpression(cdk::binary_operation_node *const node,
                                 int lvl);
  void processIDPBinaryExpression(cdk::binary_operation_node *const node,
                                  int lvl, bool isSub);
  void
  processScalarLogicalBinaryExpression(cdk::binary_operation_node *const node,
                                       int lvl);
  void
  processBooleanLogicalBinaryExpression(cdk::binary_operation_node *const node,
                                        int lvl);
  void
  processGeneralLogicalBinaryExpression(cdk::binary_operation_node *const node,
                                        int lvl);

  template <typename T>
  void process_literal(cdk::literal_node<T> *const node, int lvl) {}

public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end
};

} // namespace mml

//---------------------------------------------------------------------------
//     HELPER MACRO FOR TYPE CHECKING
//---------------------------------------------------------------------------

#define CHECK_TYPES(compiler, symtab, node)                                    \
  {                                                                            \
    try {                                                                      \
      mml::type_checker checker(compiler, symtab, this);                       \
      (node)->accept(&checker, 0);                                             \
    } catch (const std::string &problem) {                                     \
      std::cerr << (node)->lineno() << ": " << problem << std::endl;           \
      return;                                                                  \
    }                                                                          \
  }

#define ASSERT_SAFE_EXPRESSIONS CHECK_TYPES(_compiler, _symtab, node)

#endif
