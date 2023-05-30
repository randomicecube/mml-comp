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
void mml::postfix_writer::do_double_node(cdk::double_node *const node,
                                         int lvl) {
  // FIXME: this may need more stuff
  if (_inFunctionBody) {
    _pf.DOUBLE(node->value());     // load number to the stack
  } else {
    _pf.SDOUBLE(node->value());    // double is on the DATA segment
  }
}
void mml::postfix_writer::do_string_node(cdk::string_node *const node,
                                         int lvl) {
  const auto lbl = mklbl(++_lbl);

  /* generate the string */
  _pf.RODATA();                    // strings are DATA readonly
  _pf.ALIGN();                     // make sure we are aligned
  _pf.LABEL(lbl);                  // give the string a name
  _pf.SSTRING(node->value());      // output string characters

  if (_inFunctionBody) {
    // local variable initializer
    _pf.TEXT();                    // return to the TEXT segment
    _pf.ADDR(lbl);                 // the string to be printed
  } else {
    // global variable initializer
    _pf.DATA();                    // return to the DATA segment
    _pf.SADDR(lbl);                // the string to be printed
  }
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

void mml::postfix_writer::do_rvalue_node(cdk::rvalue_node *const node,
                                         int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.LDDOUBLE();
  } else {
    // integers, pointers, and strings
    // FIXME: may need forward check here
    _pf.LDINT();
  }
}

void mml::postfix_writer::do_assignment_node(cdk::assignment_node *const node,
                                             int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    if (node->rvalue()->type()->name() == cdk::TYPE_INT)
      _pf.I2D();
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.STDOUBLE();
  else
    _pf.STINT();

}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_evaluation_node(mml::evaluation_node *const node,
                                             int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2); // determine the value
  _pf.TRASH(node->argument()->type()->size()); // delete it
}

void mml::postfix_writer::do_print_node(mml::print_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
    const auto arg = dynamic_cast<cdk::expression_node *>(node->arguments()->node(ix));
    arg->accept(this, lvl); // determine the value to print
    if (arg->is_typed(cdk::TYPE_INT)) {
      _functionsToDeclare.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4); // trash int
    }
    else if (arg->is_typed(cdk::TYPE_DOUBLE)) {
      _functionsToDeclare.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8); // trash double
    }
    else if (arg->is_typed(cdk::TYPE_STRING)) {
      _functionsToDeclare.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4); // trash char pointer
    }
    else
      error(node->lineno(), "cannot print expression of unknown type");
  }
  if (node->newline()) {
    _functionsToDeclare.insert("println");
    _pf.CALL("println"); // print a newline
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_while_node(mml::while_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int whileCondLbl = ++_lbl;
  int whileEndLbl = ++_lbl;
  _whileCond.push_back(whileCondLbl); // the (currently) deepest while condition label
  _whileEnd.push_back(whileEndLbl);   // the (currently) deepest while end label

  _symtab.push(); // entering new context, new symbol table for block-local vars

  _pf.ALIGN();   // make sure we are aligned
  _pf.LABEL(mklbl(whileCondLbl)); // setting label for the condition
  node->condition()->accept(this, lvl + 2); // condition evaluation
  _pf.JZ(mklbl(whileEndLbl)); // if false, exit the cycle

  node->block()->accept(this, lvl + 2); // block evaluation
  _pf.JMP(mklbl(whileCondLbl)); // repeat
  _pf.ALIGN();   // make sure we are aligned
  _pf.LABEL(mklbl(whileEndLbl)); // setting label for the end of the cycle
  
  _symtab.pop(); // leaving current context
  _whileCond.pop_back(); // leaving current while condition label
  _whileEnd.pop_back(); // leaving current while end label
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

void mml::postfix_writer::do_stop_node(mml::stop_node *const node, int lvl) {
  if (_whileCond.size() == 0) {
    error(node->lineno(), "stop node found outside a while block");
    return;
  }
  const size_t stopLvl = (size_t) node->level();

  // if stopLvl equals 1, we go to the topmost while end label.
  // otherwise, we go to the stopLvl-th while end label
  // we also need to check if the stopLvl-th while end label even exists
  if (stopLvl > _whileEnd.size() || stopLvl < 1) {
    error(node->lineno(), "invalid stop level");
    return;
  }
  const auto whileEndLbl = _whileEnd[stopLvl - 1];
  _pf.JMP(mklbl(whileEndLbl));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_next_node(mml::next_node *const node, int lvl) {
  if (_whileCond.size() == 0) {
    error(node->lineno(), "next node found outside a while block");
    return;
  }
  const size_t nextLvl = (size_t) node->level();

  // if nextLvl equals 1, we go to the topmost while condition label.
  // otherwise, we go to the nextLvl-th while condition label
  // we also need to check if the nextLvl-th while condition label even exists
  if (nextLvl > _whileCond.size() || nextLvl < 1) {
    error(node->lineno(), "invalid next level");
    return;
  }
  const auto whileCondLbl = _whileCond[nextLvl - 1];
  _pf.JMP(mklbl(whileCondLbl));
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

void mml::postfix_writer::do_input_node(mml::input_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  switch (node->type()->name()) {
  case cdk::TYPE_INT:
    _functionsToDeclare.insert("readi");
    _pf.CALL("readi");
    _pf.LDFVAL32();
    break;
  case cdk::TYPE_DOUBLE:
    _functionsToDeclare.insert("readd");
    _pf.CALL("readd");
    _pf.LDFVAL64();
    break;
  default:
    error(node->lineno(), "cannot read expression of unknown type");
  }
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

