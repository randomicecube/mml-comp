#include "targets/type_checker.h"
#include ".auto/all_nodes.h" // automatically generated
#include <cdk/types/primitive_type.h>
#include <string>

#include <mml_parser.tab.h>

#define ASSERT_UNSPEC                                                          \
  {                                                                            \
    if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC))          \
      return;                                                                  \
  }

static bool compatible_ptr_types(std::shared_ptr<cdk::basic_type> t1,
                                 std::shared_ptr<cdk::basic_type> t2) {
  auto t1_ptr = t1;
  auto t2_ptr = t2;
  while (t1_ptr->name() == cdk::TYPE_POINTER && t2_ptr != nullptr && t2_ptr->name() == cdk::TYPE_POINTER) {
    t1_ptr = cdk::reference_type::cast(t1_ptr)->referenced();
    t2_ptr = cdk::reference_type::cast(t2_ptr)->referenced();
  }
  return t2_ptr == nullptr || t1_ptr->name() == t2_ptr->name();
}

static bool compatible_fun_types(std::shared_ptr<cdk::functional_type> t1,
                                 std::shared_ptr<cdk::functional_type> t2) {
  // TODO: do this
  return true;
}

//---------------------------------------------------------------------------
// NOTE: these methods were adapted from the provided type_checker.cpp, in OG

void mml::type_checker::processIBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in right argument of binary expression");

  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

// FIXME: this is ugly, stolen from OG
void mml::type_checker::processIDBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) &&
      node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) &&
             node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) &&
             node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) &&
             node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) &&
             node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong types in binary expression");
  }
}

// FIXME: this is ugly, stolen from OG
void mml::type_checker::processIDPBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) &&
      node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) &&
             node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) &&
             node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_POINTER) &&
             node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(node->left()->type());
  } else if (node->left()->is_typed(cdk::TYPE_INT) &&
             node->right()->is_typed(cdk::TYPE_POINTER)) {
    node->type(node->right()->type());
  } else if (node->left()->is_typed(cdk::TYPE_INT) &&
             node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) &&
             node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong types in binary expression");
  }
}

void mml::type_checker::processScalarLogicalBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  processIBinaryExpression(node, lvl);
}

void mml::type_checker::processBooleanLogicalBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  processIBinaryExpression(node, lvl);
}

void mml::type_checker::processGeneralLogicalBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  if (node->left()->type() != node->right()->type()) {
    throw std::string("same type expected on both sides of equality operator");
  }
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_sequence_node(cdk::sequence_node *const node,
                                         int lvl) {
  for (auto n : node->nodes())
    n->accept(this, lvl);
}

//--------------------------PURPOSEFULLY EMPTY-------------------------------

void mml::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void mml::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_stop_node(mml::stop_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_next_node(mml::next_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_block_node(mml::block_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::type_checker::do_integer_node(cdk::integer_node *const node,
                                        int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void mml::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!(node->argument()->is_typed(cdk::TYPE_INT) ||
        node->argument()->is_typed(cdk::TYPE_DOUBLE))) {
    throw std::string("wrong type in argument of negation expression");
  }
  node->type(node->argument()->type());
}
void mml::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in argument of not expression");
  }
  node->type(node->argument()->type());
}

//---------------------------------------------------------------------------

void mml::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processIDPBinaryExpression(node, lvl);
}
void mml::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processIDPBinaryExpression(node, lvl);
}
void mml::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processIDBinaryExpression(node, lvl);
}
void mml::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processIDBinaryExpression(node, lvl);
}
void mml::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processIBinaryExpression(node, lvl);
}
void mml::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processScalarLogicalBinaryExpression(node, lvl);
}
void mml::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processScalarLogicalBinaryExpression(node, lvl);
}
void mml::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processScalarLogicalBinaryExpression(node, lvl);
}
void mml::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processScalarLogicalBinaryExpression(node, lvl);
}
void mml::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processGeneralLogicalBinaryExpression(node, lvl);
}
void mml::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processGeneralLogicalBinaryExpression(node, lvl);
}
void mml::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processBooleanLogicalBinaryExpression(node, lvl);
}
void mml::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processBooleanLogicalBinaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_variable_node(cdk::variable_node *const node,
                                         int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<mml::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void mml::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void mml::type_checker::do_assignment_node(cdk::assignment_node *const node,
                                           int lvl) {
  ASSERT_UNSPEC;

  try {
    node->lvalue()->accept(this, lvl);
  } catch (const std::string &id) {
    auto symbol = std::make_shared<mml::symbol>(
        cdk::primitive_type::create(4, cdk::TYPE_INT), id, 0);
    _symtab.insert(id, symbol);
    _parent->set_new_symbol(
        symbol); // advise parent that a symbol has been inserted
    node->lvalue()->accept(this, lvl); // DAVID: bah!
  }

  if (!node->lvalue()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in left argument of assignment expression");

  node->rvalue()->accept(this, lvl + 2);
  if (!node->rvalue()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in right argument of assignment expression");

  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_evaluation_node(mml::evaluation_node *const node,
                                           int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void mml::type_checker::do_print_node(mml::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_while_node(mml::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_if_node(mml::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

void mml::type_checker::do_if_else_node(mml::if_else_node *const node,
                                        int lvl) {
  node->condition()->accept(this, lvl + 4);
}

void mml::type_checker::do_return_node(mml::return_node *const node, int lvl) {
  auto function = _symtab.find("@", 1);
  if (!function) { // a return function may only be inside a function
    throw std::string("return statement found outside function");
  }

  auto main = _symtab.find("_main", 0);
  if (main) {
    if (!node->retval())
      throw std::string("wrong type of return value in main (integer expected)");
    node->retval()->accept(this, lvl + 2);
    if (!node->retval()->is_typed(cdk::TYPE_INT))
      throw std::string("wrong type of return value in main (integer expected)");
    return;
  }

  if (!node->retval()) {
    if (!function->is_typed(cdk::TYPE_VOID))
      throw std::string("missing return value in non-void function");
    return;
  }

  const auto &fun_type = cdk::functional_type::cast(function->type());
  const auto output = fun_type->output();
  const bool has_output = fun_type->output() != nullptr;
  if (has_output && fun_type->output(0)->name() == cdk::TYPE_VOID)
    throw std::string("return with a value in void function");

  node->retval()->accept(this, lvl + 2);
  if (!has_output)
    throw std::string("unknown return type in function");

  const auto type_name = fun_type->output(0)->name();
  switch (type_name) {
  case cdk::TYPE_INT:
    if (!node->retval()->is_typed(cdk::TYPE_INT))
      throw std::string("wrong type of return value in function (integer expected)");
    break;
  case cdk::TYPE_DOUBLE:
    if (!(node->retval()->is_typed(cdk::TYPE_DOUBLE) || node->retval()->is_typed(cdk::TYPE_INT)))
      throw std::string("wrong type of return value in function (integer or double expected)");
    break;
  case cdk::TYPE_STRING:
    if (!node->retval()->is_typed(cdk::TYPE_STRING))
      throw std::string("wrong type of return value in function (string expected)");
    break;
  case cdk::TYPE_POINTER:
    if (node->retval()->is_typed(cdk::TYPE_POINTER) && !compatible_ptr_types(output, node->retval()->type()))
      throw std::string("wrong type of return value in function (pointer expected)");
    break;
  case cdk::TYPE_FUNCTIONAL:
    if (
      (
        node->retval()->is_typed(cdk::TYPE_FUNCTIONAL) &&
        !compatible_fun_types(
          cdk::functional_type::cast(output),
          cdk::functional_type::cast(node->retval()->type())
        )
      ) ||
      (
        node->retval()->is_typed(cdk::TYPE_POINTER) &&
        cdk::reference_type::cast(node->retval()->type())->referenced() == nullptr
      )
    )
      throw std::string("wrong type of return value in function (functional expected)");
    break;
  default:
    throw std::string("unknown return type in function");
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_nullptr_node(mml::nullptr_node *const node,
                                        int lvl) {
  ASSERT_UNSPEC;
  // TODO: check if this is correct;; in MML, expressions are always int
  node->type(cdk::reference_type::create(
      4, cdk::primitive_type::create(4, cdk::TYPE_INT)));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_declaration_node(mml::declaration_node *const node,
                                            int lvl) {
  // FIXME: currently empty in order to compile, isn't required for the first
  // delivery
}

//---------------------------------------------------------------------------

void mml::type_checker::do_input_node(mml::input_node *const node, int lvl) {
  ASSERT_UNSPEC;
  // TODO: check if this is correct;; in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

// note that it can't be processed as a "regular" unary expression,
// as it may not only be an integer, but also a real
void mml::type_checker::do_identity_node(mml::identity_node *const node,
                                         int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  const auto &type = node->argument()->type();
  if (!(type->name() == cdk::TYPE_INT || type->name() == cdk::TYPE_DOUBLE))
    throw std::string("wrong type in argument of identity expression");
  node->type(type);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_index_node(mml::index_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->base()->accept(this, lvl + 2);
  if (!node->base()->is_typed(cdk::TYPE_POINTER))
    throw std::string("wrong type in base of index expression");
  node->index()->accept(this, lvl + 2);
  if (!node->index()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in index of index expression");
  const std::shared_ptr<cdk::basic_type> base_ref =
      cdk::reference_type::cast(node->base()->type())->referenced();
  node->type(base_ref);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_stack_alloc_node(mml::stack_alloc_node *const node,
                                            int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in argument of stack_alloc expression");
  node->type(
      // TODO: check if this is correct;; in MML, expressions are always int
      cdk::reference_type::create(
          4, cdk::primitive_type::create(4, cdk::TYPE_INT)));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_address_of_node(mml::address_of_node *const node,
                                           int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_function_call_node(
    mml::function_call_node *const node, int lvl) {
  ASSERT_UNSPEC;
  std::vector<std::shared_ptr<cdk::basic_type>> args_types;
  
  if (node->function()) { // regular call
    node->function()->accept(this, lvl + 2);
    if (!(node->function()->is_typed(cdk::TYPE_FUNCTIONAL)))
      throw std::string("wrong type in function call expression");

    const auto &type = node->function()->type();
    args_types = cdk::functional_type::cast(type)->input()->components();
    node->type(cdk::functional_type::cast(type)->output(0));
  } else { // recursive call (@)
    auto symbol = _symtab.find("@", 1); // looks at one level above for the symbol
    if (!symbol)
      throw std::string("recursive call to undeclared function");
    else if (symbol->is_main())
      throw std::string("recursive call in main function");
    
    const auto &type = symbol->type();
    args_types = cdk::functional_type::cast(type)->input()->components();
    node->type(cdk::functional_type::cast(type)->output(0));
  }

  if (args_types.size() != node->arguments()->size())
    throw std::string("wrong number of arguments in function call expression");

  node->arguments()->accept(this, lvl + 2);
  for (size_t i = 0; i < args_types.size(); i++) {
    const auto &param_type = dynamic_cast<cdk::expression_node *>(node->arguments()->node(i))->type();
    // note that the second condition is to allow passing an int as a double
    if (
      (args_types[i] == param_type) ||
      (args_types[i]->name() == cdk::TYPE_DOUBLE && param_type->name() == cdk::TYPE_INT)
    )
      continue;
    throw std::string("wrong type in argument of function call expression");
  }
}

//---------------------------------------------------------------------------

// FIXME: this doesn't look right
void mml::type_checker::do_function_definition_node(
    mml::function_definition_node *const node, int lvl) {
  if (node->main()) {
    const auto fun_int_type = cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT));
    const auto main = mml::make_symbol(fun_int_type, "_main", 0, tPRIVATE);
    main->set_main();
    const auto main_at = mml::make_symbol(fun_int_type, "@", 0, tPRIVATE);
    main_at->set_main();
    // FIXME: supposedly this doesn't work well, needs to be looked at
    _symtab.replace_local(main_at->name(), main);
    _parent->set_new_symbol(main);
    return;
  }

  const auto function = mml::make_symbol(node->type(), "@", 0, tPRIVATE);
  // FIXME: supposedly this doesn't work well, needs to be looked at
  _symtab.replace_local(function->name(), function);
  _parent->set_new_symbol(function);
}
