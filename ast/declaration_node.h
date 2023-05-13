#ifndef __MML_AST_DECLARATION_NODE_H__
#define __MML_AST_DECLARATION_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/typed_node.h>
#include <string>

namespace mml {

/**
 * Class for describing declaration nodes.
 */
class declaration_node : public cdk::typed_node {
  int _qualifier;
  std::string _identifier;
  cdk::expression_node *_init;

public:
  inline declaration_node(int lineno, int qualifier,
                          std::shared_ptr<cdk::basic_type> type,
                          const std::string &identifier,
                          cdk::expression_node *init)
      : cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier),
        _init(init) {
    cdk::typed_node::type(type);
  }

public:
  inline int qualifier() { return _qualifier; }

  inline std::string identifier() { return _identifier; }

  inline cdk::expression_node *init() { return _init; }

  void accept(basic_ast_visitor *sp, int level) {
    sp->do_declaration_node(this, level);
  }
};

} // namespace mml

#endif
