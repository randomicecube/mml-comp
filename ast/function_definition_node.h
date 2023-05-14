#ifndef __MML_AST_FUNCTION_H__
#define __MML_AST_FUNCTION_H__

#include <cdk/ast/expression_node.h>

namespace mml {

class function_definition_node : public cdk::expression_node {
  cdk::sequence_node *_arguments;
  mml::block_node *_block;
  bool _main;

public:
  inline function_definition_node(int lineno,
                                  std::shared_ptr<cdk::basic_type> outputType,
                                  cdk::sequence_node *arguments,
                                  mml::block_node *block)
      : cdk::expression_node(lineno), _arguments(arguments), _block(block),
        _main(false) {
    std::vector<std::shared_ptr<cdk::basic_type>> inputTypes;
    for (auto *node : arguments->nodes())
      inputTypes.push_back(dynamic_cast<cdk::typed_node *>(node)->type());
    type(cdk::functional_type::create(inputTypes, outputType));
  }

  /**
   * Constructor for the main function.
   */
  inline function_definition_node(int lineno, mml::block_node *block)
      : cdk::expression_node(lineno), _arguments(nullptr), _block(block),
        _main(true) {
    type(cdk::functional_type::create(
        cdk::primitive_type::create(4, cdk::TYPE_INT)));
  }

public:
  inline cdk::sequence_node *arguments() { return _arguments; }

  inline mml::block_node *block() { return _block; }

  inline bool main() { return _main; }

  void accept(basic_ast_visitor *sp, int level) {
    sp->do_function_definition_node(this, level);
  }
};

} // namespace mml

#endif