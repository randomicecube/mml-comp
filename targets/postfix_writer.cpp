#include "targets/postfix_writer.h"
#include ".auto/all_nodes.h" // all_nodes.h is automatically generated
#include "targets/type_checker.h"
#include "targets/frame_size_calculator.h"
#include <sstream>
#include <string>

//-------------------------PURPOSEFULLY EMPTY--------------------------------

void mml::postfix_writer::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void mml::postfix_writer::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_integer_node(cdk::integer_node *const node,
                                          int lvl) {
  if (_inFunctionBody)
    _pf.INT(node->value());
  else
    _pf.SINT(node->value());
}
// TODO
void mml::postfix_writer::do_double_node(cdk::double_node *const node,
                                         int lvl) {
  // EMPTY
}
// TODO
void mml::postfix_writer::do_string_node(cdk::string_node *const node,
                                         int lvl) {
  int lbl1;

  /* generate the string */
  _pf.RODATA();                    // strings are DATA readonly
  _pf.ALIGN();                     // make sure we are aligned
  _pf.LABEL(mklbl(lbl1 = ++_lbl)); // give the string a name
  _pf.SSTRING(node->value());      // output string characters

  /* leave the address on the stack */
  _pf.TEXT();            // return to the TEXT segment
  _pf.ADDR(mklbl(lbl1)); // the string to be printed
}
void mml::postfix_writer::do_nullptr_node(mml::nullptr_node *const node,
                                          int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  // we'll always want to put a 0 in the stack, what matters is whether it's
  // static or not
  if (_inFunctionBody)
    _pf.INT(0);
  else
    _pf.SINT(0);
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_identity_node(mml::identity_node *const node,
                                           int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2);
}
void mml::postfix_writer::do_neg_node(cdk::neg_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2); // determine the value
  _pf.NEG();                               // 2-complement
}
void mml::postfix_writer::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2); // the value we want to compare
  _pf.INT(0); // we want to compare it to false
  _pf.EQ(); // checks whether the last two values on the stack are equal
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_and_node(cdk::and_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  const auto lbl = mklbl(++_lbl);
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(lbl);
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(lbl);
}
void mml::postfix_writer::do_or_node(cdk::or_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  const auto lbl = mklbl(++_lbl);
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(lbl);
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(lbl);
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sequence_node(cdk::sequence_node *const node,
                                           int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::processIDPBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  const auto ref_left = cdk::reference_type::cast(node->left()->type())->referenced();
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT))
    _pf.I2D();
  else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.INT(ref_left->size());
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  const auto ref_right = cdk::reference_type::cast(node->right()->type())->referenced();
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT))
    _pf.I2D();
  else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.INT(ref_right->size());
    _pf.MUL();
  }
}
void mml::postfix_writer::do_add_node(cdk::add_node *const node, int lvl) {
  processIDPBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DADD();
  else
    _pf.ADD();
}
void mml::postfix_writer::do_sub_node(cdk::sub_node *const node, int lvl) {
  processIDPBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DSUB();
  else
    _pf.SUB();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::processIDBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT))
    _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT))
    _pf.I2D();
}
void mml::postfix_writer::do_mul_node(cdk::mul_node *const node, int lvl) {
  processIDBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DMUL();
  else
    _pf.MUL();
}
void mml::postfix_writer::do_div_node(cdk::div_node *const node, int lvl) {
  processIDBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DDIV();
  else
    _pf.DIV();
}
void mml::postfix_writer::do_mod_node(cdk::mod_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::processGeneralLogicalBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();
  
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->left()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();
}
void mml::postfix_writer::do_lt_node(cdk::lt_node *const node, int lvl) {
  processGeneralLogicalBinaryExpression(node, lvl);
  _pf.LT();
}
void mml::postfix_writer::do_le_node(cdk::le_node *const node, int lvl) {
  processGeneralLogicalBinaryExpression(node, lvl);
  _pf.LE();
}
void mml::postfix_writer::do_ge_node(cdk::ge_node *const node, int lvl) {
  processGeneralLogicalBinaryExpression(node, lvl);
  _pf.GE();
}
void mml::postfix_writer::do_gt_node(cdk::gt_node *const node, int lvl) {
  processGeneralLogicalBinaryExpression(node, lvl);
  _pf.GT();
}
void mml::postfix_writer::do_ne_node(cdk::ne_node *const node, int lvl) {
  processGeneralLogicalBinaryExpression(node, lvl);
  _pf.NE();
}
void mml::postfix_writer::do_eq_node(cdk::eq_node *const node, int lvl) {
  processGeneralLogicalBinaryExpression(node, lvl);
  _pf.EQ();
}

//---------------------------------------------------------------------------
// TODO: all below

// TODO
void mml::postfix_writer::do_variable_node(cdk::variable_node *const node,
                                           int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  // simplified generation: all variables are global
  _pf.ADDR(node->name());
}

// TODO
void mml::postfix_writer::do_rvalue_node(cdk::rvalue_node *const node,
                                         int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  _pf.LDINT(); // depends on type size
}

// TODO
void mml::postfix_writer::do_assignment_node(cdk::assignment_node *const node,
                                             int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl); // determine the new value
  _pf.DUP32();
  if (new_symbol() == nullptr) {
    node->lvalue()->accept(this, lvl); // where to store the value
  } else {
    _pf.DATA();  // variables are all global and live in DATA
    _pf.ALIGN(); // make sure we are aligned
    _pf.LABEL(new_symbol()->name()); // name variable location
    reset_new_symbol();
    _pf.SINT(0);                       // initialize it to 0 (zero)
    _pf.TEXT();                        // return to the TEXT segment
    node->lvalue()->accept(this, lvl); // DAVID: bah!
  }
  _pf.STINT(); // store the value at address
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_evaluation_node(mml::evaluation_node *const node,
                                             int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2); // determine the value
  _pf.TRASH(node->argument()->type()->size()); // delete it
}

// TODO
void mml::postfix_writer::do_print_node(mml::print_node *const node, int lvl) {
  // FIXME: only works in the next delivery
  /* ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value to print
  if (node->argument()->is_typed(cdk::TYPE_INT)) {
    _pf.CALL("printi");
    _pf.TRASH(4); // delete the printed value
  } else if (node->argument()->is_typed(cdk::TYPE_STRING)) {
    _pf.CALL("prints");
    _pf.TRASH(4); // delete the printed value's address
  } else {
    std::cerr << "ERROR: CANNOT HAPPEN!" << std::endl;
    exit(1);
  }
  _pf.CALL("println"); // print a newline */
}

//---------------------------------------------------------------------------

// TODO
void mml::postfix_writer::do_while_node(mml::while_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  _pf.LABEL(mklbl(lbl1 = ++_lbl));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl2 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl1));
  _pf.LABEL(mklbl(lbl2));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_node(mml::if_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_else_node(mml::if_else_node *const node,
                                          int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1 = lbl2));
}

//---------------------------------------------------------------------------

// TODO
void mml::postfix_writer::do_stop_node(mml::stop_node *const node, int lvl) {
  // FIXME: currently empty in order to compile, isn't required for the first
  // delivery
}

//---------------------------------------------------------------------------

// TODO
void mml::postfix_writer::do_next_node(mml::next_node *const node, int lvl) {
  // FIXME: currently empty in order to compile, isn't required for the first
  // delivery
}

//---------------------------------------------------------------------------

// TODO
void mml::postfix_writer::do_return_node(mml::return_node *const node,
                                         int lvl) {
  // FIXME: currently empty in order to compile, isn't required for the first
  // delivery
}

//---------------------------------------------------------------------------

// TODO
void mml::postfix_writer::do_declaration_node(mml::declaration_node *const node,
                                              int lvl) {
  // FIXME: currently empty in order to compile, isn't required for the first
  // delivery
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_block_node(mml::block_node *const node, int lvl) {
  _symtab.push(); // entering new context, new symbol table for block-local vars
  if (node->declarations())
    node->declarations()->accept(this, lvl + 2);
  if (node->instructions())
    node->instructions()->accept(this, lvl + 2);
  _symtab.pop(); // leaving current context
}

//---------------------------------------------------------------------------

// TODO
void mml::postfix_writer::do_input_node(mml::input_node *const node, int lvl) {
  // FIXME: currently empty in order to compile, isn't required for the first
  // delivery
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sizeof_node(mml::sizeof_node *const node,
                                         int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody)
    _pf.INT(node->argument()->type()->size());
  else
    _pf.SINT(node->argument()->type()->size());
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_index_node(mml::index_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->base()->accept(this, lvl);
  node->index()->accept(this, lvl);
  _pf.INT(node->type()->size()); // type size
  _pf.MUL();                     // type size * index
  _pf.ADD();                     // base + (type size * index)
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_stack_alloc_node(mml::stack_alloc_node *const node,
                                              int lvl) {
  const auto ref = cdk::reference_type::cast(node->type())->referenced();
  node->argument()->accept(this, lvl);
  _pf.INT(ref->size()); // type size
  _pf.MUL();            // type size * argument
  _pf.ALLOC();          // allocate space for the array
  _pf.SP();             // pushes the array's address
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_address_of_node(mml::address_of_node *const node,
                                             int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

// TODO
void mml::postfix_writer::do_function_call_node(
    mml::function_call_node *const node, int lvl) {
  // FIXME: currently empty in order to compile, isn't required for the first
  // delivery
}

//---------------------------------------------------------------------------

// TODO
void mml::postfix_writer::do_function_definition_node(
    mml::function_definition_node *const node, int lvl) {
  // FIXME: currently empty in order to compile, isn't required for the first
  // delivery
}

