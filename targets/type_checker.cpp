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

bool mml::type_checker::check_compatible_ptr_types(std::shared_ptr<cdk::basic_type> t1,
                          std::shared_ptr<cdk::basic_type> t2) {
  auto t1_ptr = t1;
  auto t2_ptr = t2;
  while (t1_ptr->name() == cdk::TYPE_POINTER && t2_ptr != nullptr &&
         t2_ptr->name() == cdk::TYPE_POINTER) {
    t1_ptr = cdk::reference_type::cast(t1_ptr)->referenced();
    t2_ptr = cdk::reference_type::cast(t2_ptr)->referenced();
  }
  return t2_ptr == nullptr || t1_ptr->name() == t2_ptr->name();
}

bool mml::type_checker::check_compatible_fun_types(std::shared_ptr<cdk::functional_type> t1,
                          std::shared_ptr<cdk::functional_type> t2) {
  // the return type must be compatible
  if ((t1->output_length() > 0 && t2->output_length() > 0) && !check_compatible_types(t1->output(0), t2->output(0)))
    return false;

  // the number of arguments must be the same
  if (t1->input_length() != t2->input_length())
    return false;

  // the types of the arguments must be compatible
  for (size_t i = 0; i < t1->input_length(); i++)
    if (!check_compatible_types(t1->input(i), t2->input(i)))
      return false;
  return true;
}

bool mml::type_checker::check_compatible_types(std::shared_ptr<cdk::basic_type> t1,
                      std::shared_ptr<cdk::basic_type> t2) {
  const auto t1_name = t1->name();
  const auto t2_name = t2->name();


  switch (t1_name) {
  case cdk::TYPE_INT:
    if (t2_name != cdk::TYPE_INT)
      return false;
    break;
  case cdk::TYPE_DOUBLE:
    if (!(t2_name == cdk::TYPE_DOUBLE || t2_name == cdk::TYPE_INT))
      return false;
    break;
  case cdk::TYPE_STRING:
    if (t2_name != cdk::TYPE_STRING)
      return false;
    break;
  case cdk::TYPE_POINTER:
    if (!(t2_name == cdk::TYPE_POINTER && check_compatible_ptr_types(t1, t2)))
      return false;
    break;
  case cdk::TYPE_FUNCTIONAL:
    if (!(t2_name == cdk::TYPE_FUNCTIONAL && check_compatible_fun_types(
      cdk::functional_type::cast(t1), cdk::functional_type::cast(t2)
    )))
      return false;
    break;
  default:
    if (t1_name != t2_name)
      return false;
  }
  return true;
}

void mml::type_checker::throw_incompatible_types(std::shared_ptr<cdk::basic_type> t_node,
                           std::shared_ptr<cdk::basic_type> t_field,
                           cdk::typename_type tname_node,
                           cdk::typename_type tname_field,
                           bool is_return) {
  std::shared_ptr<cdk::functional_type> fun_t_node;
  std::shared_ptr<cdk::functional_type> fun_t_field;
  std::shared_ptr<cdk::basic_type> ref_t_field;
  const std::string field_name = is_return ? "return" : "initialization"; // hacky

  switch (tname_node) {
  case cdk::TYPE_INT:
    if (tname_field != cdk::TYPE_INT)
      throw std::string("wrong type in " + field_name + " (expected int)");
    break;
  case cdk::TYPE_DOUBLE:
    if (!(tname_field == cdk::TYPE_INT || tname_field == cdk::TYPE_DOUBLE))
      throw std::string("wrong type in " + field_name +
                        " (expected double or int)");
    break;
  case cdk::TYPE_STRING:
    if (tname_field != cdk::TYPE_STRING)
      throw std::string("wrong type in " + field_name + " (expected string)");
    break;
  case cdk::TYPE_POINTER:
    if (is_return == (tname_field == cdk::TYPE_POINTER) &&
        !check_compatible_ptr_types(t_node, t_field))
      throw std::string("wrong type in " + field_name + " (expected pointer)");
    break;
  case cdk::TYPE_FUNCTIONAL:
    fun_t_node = cdk::functional_type::cast(t_node);
    fun_t_field = cdk::functional_type::cast(t_field);
    if (
      (tname_field == cdk::TYPE_FUNCTIONAL && !check_compatible_fun_types(fun_t_node, fun_t_field)) ||
      (tname_field == cdk::TYPE_POINTER && cdk::reference_type::cast(t_field)->referenced() == nullptr) // f = null
    )
      throw std::string("wrong type in " + field_name + " (expected function)");
    break;
  default:
    throw std::string("unknown type in " + field_name);
  }
}

//---------------------------------------------------------------------------
// NOTE: these methods were adapted from the provided type_checker.cpp, in OG

// returns false if not correctly processed
bool mml::type_checker::processBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (
    (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) ||
    (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) ||
    (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE))
  ) {
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
    return false;
  }
  return true;
}

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

void mml::type_checker::processIDBinaryExpression(
    cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  if (!processBinaryExpression(node, lvl))
    throw std::string("wrong types in binary expression");
}

void mml::type_checker::processIDPBinaryExpression(
    cdk::binary_operation_node *const node, int lvl, bool isSub) {
  ASSERT_UNSPEC;
  if (processBinaryExpression(node, lvl))
    return;
  
  if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(node->left()->type());
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    node->type(node->right()->type());
  } else {
		if (isSub) {
      if (
        (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) &&
        (check_compatible_ptr_types(node->left()->type(), node->right()->type()))
      ) {
        node->type(node->left()->type());
        return;
      }
		}
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

// FIXME: add and sub may be wrong, as I think only sub_node should allow pointer subtraction
void mml::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processIDPBinaryExpression(node, lvl, false);
}
void mml::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processIDPBinaryExpression(node, lvl, true);
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
  node->lvalue()->accept(this, lvl + 2);
  node->rvalue()->accept(this, lvl + 2);

  const auto lval_type = node->lvalue()->type();
  const auto rval_type = node->rvalue()->type();
  const auto lval_type_name = lval_type->name();
  const auto rval_type_name = rval_type->name();

  const auto fun_lval_type = cdk::functional_type::cast(lval_type);
  const auto fun_rval_type = cdk::functional_type::cast(rval_type);

  const auto int_type = cdk::primitive_type::create(4, cdk::TYPE_INT);
  const auto double_type = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE);
  const auto string_type = cdk::primitive_type::create(4, cdk::TYPE_STRING);
  const auto error_type = cdk::primitive_type::create(0, cdk::TYPE_ERROR);

  switch (lval_type_name) {
  case cdk::TYPE_INT:
    switch (rval_type_name) {
    case cdk::TYPE_INT:
      node->type(int_type);
      return;
    case cdk::TYPE_UNSPEC: // auto-cast
      node->type(int_type);
      node->rvalue()->type(int_type);
      return;
    default:
      throw std::string("wrong assignment to integer");
    }
    break;
  case cdk::TYPE_DOUBLE:
    switch (rval_type_name) {
    case cdk::TYPE_INT:
    case cdk::TYPE_DOUBLE:
      node->type(double_type);
      return;
    case cdk::TYPE_UNSPEC: // auto-cast
      node->type(double_type);
      node->rvalue()->type(double_type);
      return;
    default:
      throw std::string("wrong assignment to double");
    }
    break;
  case cdk::TYPE_STRING:
    switch (rval_type_name) {
    case cdk::TYPE_STRING:
      node->type(string_type);
      return;
    case cdk::TYPE_UNSPEC: // auto-cast
      node->type(string_type);
      node->rvalue()->type(string_type);
      return;
    default:
      throw std::string("wrong assignment to string");
    }
    break;
  case cdk::TYPE_POINTER:
    switch (rval_type_name) {
    case cdk::TYPE_POINTER:
      if (!check_compatible_ptr_types(lval_type, rval_type))
        throw std::string("wrong assignment to pointer");
      node->type(rval_type);
      return;
    case cdk::TYPE_UNSPEC: // auto-cast
      node->type(error_type);
      node->rvalue()->type(error_type);
      return;
    default:
      throw std::string("wrong assignment to pointer");
    }
  case cdk::TYPE_FUNCTIONAL:
    switch (rval_type_name) {
    case cdk::TYPE_FUNCTIONAL:
      if (!check_compatible_fun_types(fun_lval_type, fun_rval_type))
        throw std::string("wrong assignment to functional");
      node->type(rval_type);
      return;
    case cdk::TYPE_POINTER:
      if (cdk::reference_type::cast(rval_type)->referenced() == nullptr)
        throw std::string("wrong assignment to functional");
      node->type(rval_type);
      return;
    case cdk::TYPE_UNSPEC: // auto-cast
      node->type(error_type);
      node->rvalue()->type(error_type);
      return;
    default:
      throw std::string("wrong assignment to functional");
    }
  default:
    throw std::string("wrong types in assignment");
  }
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
  if (!node->condition()->is_typed(cdk::TYPE_INT))
    throw std::string("condition must evaluate to integer");
  node->block()->accept(this, lvl + 4);
}

void mml::type_checker::do_if_else_node(mml::if_else_node *const node,
                                        int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT))
    throw std::string("condition must evaluate to integer");
  node->thenblock()->accept(this, lvl + 4);
  node->elseblock()->accept(this, lvl + 4);
}

// FIXME: I'm not proud of the variable names here
void mml::type_checker::do_return_node(mml::return_node *const node, int lvl) {
  const auto symbol = _symtab.find("@", 1);
  const auto ret_val = node->retval();
  if (!symbol) { // a return may only be inside a function
    const auto main = _symtab.find("_main", 0);
    if (main) {
      if (!ret_val)
        throw std::string("wrong type of return value in main (int expected)");
      ret_val->accept(this, lvl + 2);
      if (!ret_val->is_typed(cdk::TYPE_INT))
        throw std::string("wrong type of return value in main (int expected)");
      return;
    }
    throw std::string("return statement found outside function");
  } else if (!ret_val) {
    return;
  }

  const auto &fun_sym_type = cdk::functional_type::cast(symbol->type());
  const auto output = fun_sym_type->output();
  const bool has_output = output != nullptr;
  if (has_output && fun_sym_type->output(0)->name() == cdk::TYPE_VOID)
    throw std::string("return with a value in void function");

  ret_val->accept(this, lvl + 2);
  if (!has_output)
    throw std::string("unknown return type in function");

  const auto type_name = fun_sym_type->output(0)->name();
  const auto node_type = ret_val->type();
  const auto node_type_name = node_type->name();

  throw_incompatible_types(output, node_type, type_name, node_type_name);
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
  const auto &init = node->init();
  if (init) {
    init->accept(this, lvl + 2);
    if (node->type())
      throw_incompatible_types(node->type(), init->type(), node->type()->name(), init->type()->name(), false);
    else
      node->type(init->type());
  }

  const auto new_symbol = mml::make_symbol(node->type(), node->identifier(), (bool) node->init(), node->qualifier());

  if (!_symtab.insert(node->identifier(), new_symbol)) {
    // in this case, we are redeclaring a variable
    const auto previous_symbol = _symtab.find_local(node->identifier());
    const auto new_symbol_type = new_symbol->type();
    const auto previous_symbol_type = previous_symbol->type();
    switch (new_symbol_type->name()) {
    case cdk::TYPE_INT:
    case cdk::TYPE_DOUBLE:
    case cdk::TYPE_STRING:
      if (new_symbol_type->name() == previous_symbol_type->name()) {
        _symtab.replace(node->identifier(), new_symbol);
        break;
      }
      [[fallthrough]];
    case cdk::TYPE_POINTER:
      if (previous_symbol_type->name() == cdk::TYPE_POINTER && check_compatible_ptr_types(new_symbol_type, previous_symbol_type)) {
        _symtab.replace(node->identifier(), new_symbol);
        break;
      }
      [[fallthrough]];
    case cdk::TYPE_FUNCTIONAL:
      if (
        previous_symbol_type->name() == cdk::TYPE_FUNCTIONAL &&
        check_compatible_fun_types(
          cdk::functional_type::cast(new_symbol_type),
          cdk::functional_type::cast(previous_symbol_type)
        )
      ) {
        _symtab.replace(node->identifier(), new_symbol);
        break;
      }
      [[fallthrough]];
    default:
      throw std::string("wrong redeclaration of variable " + node->identifier());
    }
  }
  _parent->set_new_symbol(new_symbol);
  if (node->qualifier() == tFOREIGN)
    new_symbol->set_foreign();
}

//---------------------------------------------------------------------------

void mml::type_checker::do_input_node(mml::input_node *const node, int lvl) {
  ASSERT_UNSPEC;
  // TODO: check if this is correct;; in MML, expressions are always int
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

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
    auto symbol =
        _symtab.find("@", 1); // looks at one level above for the symbol
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
    const auto &param_type =
        dynamic_cast<cdk::expression_node *>(node->arguments()->node(i))
            ->type();
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

void mml::type_checker::do_function_definition_node(
    mml::function_definition_node *const node, int lvl) {
  if (node->main()) {
    const auto fun_int_type = cdk::functional_type::create(
        cdk::primitive_type::create(4, cdk::TYPE_INT));
    // Note that it's ok to name the function _main, as no variable may have underscores
    const auto main = mml::make_symbol(fun_int_type, "_main", 0, tPRIVATE);
    main->set_main();
    const auto main_at = mml::make_symbol(fun_int_type, "@", 0, tPRIVATE);
    main_at->set_main();
    // TODO: code does not compile with replace_local :(
    if (_symtab.find_local(main_at->name())) {
      _symtab.replace(main_at->name(), main);
    } else {
      _symtab.insert(main_at->name(), main);
    }
    _parent->set_new_symbol(main);
    return;
  }

  const auto function = mml::make_symbol(node->type(), "@", 0, tPRIVATE);
  // TODO: code does not compile with replace_local :(
  if (_symtab.find_local(function->name())) {
    _symtab.replace(function->name(), function);
  } else {
    _symtab.insert(function->name(), function);
  }
  _parent->set_new_symbol(function);
}
