#include "targets/postfix_writer.h"
#include ".auto/all_nodes.h" // all_nodes.h is automatically generated
#include "targets/type_checker.h"
#include "targets/frame_size_calculator.h"
#include <sstream>
#include <string>

#include "mml_parser.tab.h"

//-------------------------PURPOSEFULLY EMPTY--------------------------------

void mml::postfix_writer::do_nil_node(cdk::nil_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: NIL_NODE" << std::endl;
  // EMPTY
  std::cout << "[DEBUG] Leaving node: NIL_NODE" << std::endl;
}
void mml::postfix_writer::do_data_node(cdk::data_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: DATA_NODE" << std::endl;
  // EMPTY
  std::cout << "[DEBUG] Leaving node: DATA_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_integer_node(cdk::integer_node *const node,
                                          int lvl) {
  std::cout << "[DEBUG] Entering node: INTEGER_NODE" << std::endl;
  if (_inFunctionBody)
    _pf.INT(node->value());
  else
    _pf.SINT(node->value());
  std::cout << "[DEBUG] Leaving node: INTEGER_NODE" << std::endl;
}
void mml::postfix_writer::do_double_node(cdk::double_node *const node,
                                         int lvl) {
  std::cout << "[DEBUG] Entering node: DOUBLE_NODE" << std::endl;
  // FIXME: this may need more stuff
  if (_inFunctionBody) {
    _pf.DOUBLE(node->value());     // load number to the stack
  } else {
    _pf.SDOUBLE(node->value());    // double is on the DATA segment
  }
  std::cout << "[DEBUG] Leaving node: DOUBLE_NODE" << std::endl;
}
void mml::postfix_writer::do_string_node(cdk::string_node *const node,
                                         int lvl) {
  std::cout << "[DEBUG] Entering node: STRING_NODE" << std::endl;
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
  std::cout << "[DEBUG] Leaving node: STRING_NODE" << std::endl;
}
void mml::postfix_writer::do_nullptr_node(mml::nullptr_node *const node,
                                          int lvl) {
  std::cout << "[DEBUG] Entering node: NULLPTR_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  // we'll always want to put a 0 in the stack, what matters is whether it's
  // static or not
  if (_inFunctionBody)
    _pf.INT(0);
  else
    _pf.SINT(0);
  std::cout << "[DEBUG] Leaving node: NULLPTR_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_identity_node(mml::identity_node *const node,
                                           int lvl) {
  std::cout << "[DEBUG] Entering node: IDENTITY_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2);
  std::cout << "[DEBUG] Leaving node: IDENTITY_NODE" << std::endl;
}
void mml::postfix_writer::do_neg_node(cdk::neg_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: NEG_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2); // determine the value
  _pf.NEG();                               // 2-complement
  std::cout << "[DEBUG] Leaving node: NEG_NODE" << std::endl;
}
void mml::postfix_writer::do_not_node(cdk::not_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: NOT_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2); // the value we want to compare
  _pf.INT(0); // we want to compare it to false
  _pf.EQ(); // checks whether the last two values on the stack are equal
  std::cout << "[DEBUG] Leaving node: NOT_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_and_node(cdk::and_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: AND_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  const auto lbl = mklbl(++_lbl);
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(lbl);
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(lbl);
  std::cout << "[DEBUG] Leaving node: AND_NODE" << std::endl;
}
void mml::postfix_writer::do_or_node(cdk::or_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: OR_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  const auto lbl = mklbl(++_lbl);
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(lbl);
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(lbl);
  std::cout << "[DEBUG] Leaving node: OR_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sequence_node(cdk::sequence_node *const node,
                                           int lvl) {
  std::cout << "[DEBUG] Entering node: SEQUENCE_NODE" << std::endl;
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
  std::cout << "[DEBUG] Leaving node: SEQUENCE_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::processIDPBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT))
    _pf.I2D();
  else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    const auto ref_left = cdk::reference_type::cast(node->left()->type())->referenced();
    _pf.INT(ref_left->size());
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT))
    _pf.I2D();
  else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    const auto ref_right = cdk::reference_type::cast(node->right()->type())->referenced();
    _pf.INT(ref_right->size());
    _pf.MUL();
  }
}
void mml::postfix_writer::do_add_node(cdk::add_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: ADD_NODE" << std::endl;
  processIDPBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DADD();
  else
    _pf.ADD();
  std::cout << "[DEBUG] Leaving node: ADD_NODE" << std::endl;
}
void mml::postfix_writer::do_sub_node(cdk::sub_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: SUB_NODE" << std::endl;
  processIDPBinaryExpression(node, lvl);
	// FIXME: missing pointer-pointer, where the result is the number if objects of the type pointed by them

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DSUB();
  else {
    _pf.SUB();
    // pointer - pointer requires a special treatment
    const auto ref_left = cdk::reference_type::cast(node->left()->type())->referenced();
    if (
      (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) &&
      ref_left->name() != cdk::TYPE_VOID
    ) {
      _pf.INT(ref_left->size());
      _pf.DIV();
    }
  }
  std::cout << "[DEBUG] Leaving node: SUB_NODE" << std::endl;
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
  std::cout << "[DEBUG] Entering node: MUL_NODE" << std::endl;
  processIDBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DMUL();
  else
    _pf.MUL();
  std::cout << "[DEBUG] Leaving node: MUL_NODE" << std::endl;
}
void mml::postfix_writer::do_div_node(cdk::div_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: DIV_NODE" << std::endl;
  processIDBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DDIV();
  else
    _pf.DIV();
  std::cout << "[DEBUG] Leaving node: DIV_NODE" << std::endl;
}
void mml::postfix_writer::do_mod_node(cdk::mod_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: MOD_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
  std::cout << "[DEBUG] Leaving node: MOD_NODE" << std::endl;
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
  std::cout << "[DEBUG] Entering node: LT_NODE" << std::endl;
  processGeneralLogicalBinaryExpression(node, lvl);
  _pf.LT();
  std::cout << "[DEBUG] Leaving node: LT_NODE" << std::endl;
}
void mml::postfix_writer::do_le_node(cdk::le_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: LE_NODE" << std::endl;
  processGeneralLogicalBinaryExpression(node, lvl);
  _pf.LE();
  std::cout << "[DEBUG] Leaving node: LE_NODE" << std::endl;
}
void mml::postfix_writer::do_ge_node(cdk::ge_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: GE_NODE" << std::endl;
  processGeneralLogicalBinaryExpression(node, lvl);
  _pf.GE();
  std::cout << "[DEBUG] Leaving node: GE_NODE" << std::endl;
}
void mml::postfix_writer::do_gt_node(cdk::gt_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: GT_NODE" << std::endl;
  processGeneralLogicalBinaryExpression(node, lvl);
  _pf.GT();
  std::cout << "[DEBUG] Leaving node: GT_NODE" << std::endl;
}
void mml::postfix_writer::do_ne_node(cdk::ne_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: NE_NODE" << std::endl;
  processGeneralLogicalBinaryExpression(node, lvl);
  _pf.NE();
  std::cout << "[DEBUG] Leaving node: NE_NODE" << std::endl;
}
void mml::postfix_writer::do_eq_node(cdk::eq_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: EQ_NODE" << std::endl;
  processGeneralLogicalBinaryExpression(node, lvl);
  _pf.EQ();
  std::cout << "[DEBUG] Leaving node: EQ_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_variable_node(cdk::variable_node *const node,
                                           int lvl) {
  std::cout << "[DEBUG] Entering node: VARIABLE_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  const auto &id = node->name();
  const auto symbol = _symtab.find(id);
  // a symbol may be global, local, or forwarded from another module
  if (symbol->is_global())
    _pf.ADDR(node->name());
  else if (symbol->is_foreign())
    // if it's been forwarded, we won't branch to it, but rather call it;
    // as such, we'll needs its label (note that this'll be useful in
    // function calls)
    _currentForwardLabel = symbol->name();
  else
    _pf.LOCAL(symbol->offset());
  std::cout << "[DEBUG] Leaving node: VARIABLE_NODE" << std::endl;
}

void mml::postfix_writer::do_rvalue_node(cdk::rvalue_node *const node,
                                         int lvl) {
  std::cout << "[DEBUG] Entering node: RVALUE_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.LDDOUBLE();
  } else {
    // integers, pointers, strings, functionals

    // note that if we're dealing with forwarded methods, we don't want to
    // branch to them, and as such, loading its first instruction address
    // is irrelevant (as we'll just call it by its label)
    if (_currentForwardLabel.empty())
      _pf.LDINT();
  }
  std::cout << "[DEBUG] Leaving node: RVALUE_NODE" << std::endl;
}

void mml::postfix_writer::do_assignment_node(cdk::assignment_node *const node,
                                             int lvl) {
  std::cout << "[DEBUG] Entering node: ASSIGNMENT_NODE" << std::endl;
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
  std::cout << "[DEBUG] Leaving node: ASSIGNMENT_NODE" << std::endl;

}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_evaluation_node(mml::evaluation_node *const node,
                                             int lvl) {
  std::cout << "[DEBUG] Entering node: EVALUATION_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2); // determine the value
  _pf.TRASH(node->argument()->type()->size()); // delete it
  std::cout << "[DEBUG] Leaving node: EVALUATION_NODE" << std::endl;
}

void mml::postfix_writer::do_print_node(mml::print_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: PRINT_NODE" << std::endl;
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
  std::cout << "[DEBUG] Leaving node: PRINT_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_while_node(mml::while_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: WHILE_NODE" << std::endl;
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
  std::cout << "[DEBUG] Leaving node: WHILE_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_node(mml::if_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: IF_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
  std::cout << "[DEBUG] Leaving node: IF_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_else_node(mml::if_else_node *const node,
                                          int lvl) {
  std::cout << "[DEBUG] Entering node: IF_ELSE_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1 = lbl2));
  std::cout << "[DEBUG] Leaving node: IF_ELSE_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_stop_node(mml::stop_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: STOP_NODE" << std::endl;
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
  std::cout << "[DEBUG] Leaving node: STOP_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_next_node(mml::next_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: NEXT_NODE" << std::endl;
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
  std::cout << "[DEBUG] Leaving node: NEXT_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_return_node(mml::return_node *const node,
                                         int lvl) {
  std::cout << "[DEBUG] Entering node: RETURN_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;

  // should not reach here without returning a value (if not void)
  _returnSeen = true;
  const auto current_function_type_name = cdk::functional_type::cast(
    _functions.back()->type()
  )->output(0)->name();

  if (current_function_type_name != cdk::TYPE_VOID) {
    node->retval()->accept(this, lvl + 2);
    switch (current_function_type_name) {
    case cdk::TYPE_INT:
      _pf.STFVAL32(); // removes 4 bytes (an int) from the stack
      break;
    case cdk::TYPE_DOUBLE:
      if (node->retval()->is_typed(cdk::TYPE_INT))
        _pf.I2D(); // converts int to double
      _pf.STFVAL64(); // removes 8 bytes (a double) from the stack
      break;
    case cdk::TYPE_STRING:
    case cdk::TYPE_POINTER:
    case cdk::TYPE_FUNCTIONAL:
      _pf.STFVAL32(); // removes 4 bytes from the stack
      break;
    default:
      error(node->lineno(), "invalid return type");
    }
  }

  _pf.LEAVE(); // leaves the function, destroys its local stack data
  _pf.RET(); // returns from a function -- the value being returned has been removed from the stack
  std::cout << "[DEBUG] Leaving node: RETURN_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_declaration_node(mml::declaration_node *const node,
                                              int lvl) {
  std::cout << "[DEBUG] Entering node: DECLARATION_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  const auto id = node->identifier();
  const auto type_size = node->type()->size(); // size in bytes
  int offset = 0;
  
  // the offset represents the frame pointer, which always contains the previous
  // value of the stack pointer
  // if the variable is global, the offset is 0
  // if the variable is local, the offset is negative
  // if the variable is a function argument, the offset is positive
  // note that the FP is somewhere in between the arguments and the local
  // variables, not necessarily exactly in the middle
  // read: wiki + https://people.cs.rutgers.edu/~pxk/419/notes/frames.html
  if (_inFunctionArgs) {
    // the function's arguments are placed in the stack by the caller
    offset = _offset;
    _offset += type_size;
  } else if (_inFunctionBody) {
    // the function's local variables are placed in the stack by the callee
    _offset -= type_size;
    offset = _offset;
  }

  auto symbol = new_symbol();
  if (symbol) {
    symbol->set_offset(offset);
    reset_new_symbol();
  }

  // if it's global, we'll need to declare it whenever we reach main, if it's
  // not initialized until then
  if (!_inFunctionArgs && !_inFunctionBody)
    _symbolsToDeclare.insert(symbol->name());

  // we may still need to initialize the variable
  if (node->init()) {
    if (_inFunctionBody)
      processLocalVariableInitialization(symbol, node->init(), lvl);
    else
      processGlobalVariableInitialization(symbol, node->init(), lvl);
    _symbolsToDeclare.erase(symbol->name());
  }
  std::cout << "[DEBUG] Leaving node: DECLARATION_NODE" << std::endl;
}
void mml::postfix_writer::processLocalVariableInitialization(std::shared_ptr<mml::symbol> symbol, cdk::expression_node *const initializer, int lvl) {
  initializer->accept(this, lvl + 2);
  switch (symbol->type()->name()) {
    case cdk::TYPE_INT:
    case cdk::TYPE_STRING:
    case cdk::TYPE_POINTER:
    case cdk::TYPE_FUNCTIONAL:
      _pf.LOCAL(symbol->offset());
      _pf.STINT();
      break;
    case cdk::TYPE_DOUBLE:
      if (initializer->is_typed(cdk::TYPE_INT))
        _pf.I2D();
      _pf.LOCAL(symbol->offset());
      _pf.STDOUBLE();
      break;
    default:
      error(initializer->lineno(), "invalid type for variable initialization");
  }
}
void mml::postfix_writer::processGlobalVariableInitialization(std::shared_ptr<mml::symbol> symbol, cdk::expression_node *const initializer, int lvl) {
  switch (symbol->type()->name()) {
    case cdk::TYPE_INT:
    case cdk::TYPE_STRING:
    case cdk::TYPE_POINTER:
      _pf.DATA(); // Data segment, for global variables
      _pf.ALIGN();
      _pf.LABEL(symbol->name());
      initializer->accept(this, lvl + 2);
      break;
    case cdk::TYPE_DOUBLE:
      _pf.DATA(); // Data segment, for global variables
      _pf.ALIGN();
      _pf.LABEL(symbol->name());

      // the following initializations need to be done outside of the switch
      const cdk::integer_node *dclini;
      cdk::double_node *ddi;
      switch (initializer->type()->name()) {
        case cdk::TYPE_INT:
          // here, we actually want to initialize the variable with a double
          // thus, we need to convert the expression to a double node
          // NOTE: I don't like these variable names either, taken from DM
          dclini = dynamic_cast<const cdk::integer_node*>(initializer);
          ddi = new cdk::double_node(dclini->lineno(), dclini->value());
          ddi->accept(this, lvl + 2);
          break;
        case cdk::TYPE_DOUBLE:
          initializer->accept(this, lvl + 2);
          break;
        default:
          error(initializer->lineno(), "invalid type for double variable initialization");
      }
      break;
    case cdk::TYPE_FUNCTIONAL:
      // see last example in https://web.tecnico.ulisboa.pt/~david.matos/w/pt/index.php/Code_Generation#Basic_Structures_.28data.29
      _functions.push_back(symbol);
      reset_new_symbol();
      initializer->accept(this, lvl + 2);
      _pf.DATA(); // Data segment, for global variables
      _pf.ALIGN();
      if (symbol->qualifier() == tPUBLIC)
        _pf.GLOBAL(symbol->name(), _pf.OBJ());
      _pf.LABEL(symbol->name());
      _pf.SADDR(symbol->name());
      break;
    default:
      error(initializer->lineno(), "invalid type for variable initialization");
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_block_node(mml::block_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: BLOCK_NODE" << std::endl;
  _symtab.push(); // entering new context, new symbol table for block-local vars
  if (node->declarations())
    node->declarations()->accept(this, lvl + 2);
  if (node->instructions())
    node->instructions()->accept(this, lvl + 2);
  _symtab.pop(); // leaving current context
  std::cout << "[DEBUG] Leaving node: BLOCK_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_input_node(mml::input_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: INPUT_NODE" << std::endl;
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
  std::cout << "[DEBUG] Leaving node: INPUT_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sizeof_node(mml::sizeof_node *const node,
                                         int lvl) {
  std::cout << "[DEBUG] Entering node: SIZEOF_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody)
    _pf.INT(node->argument()->type()->size());
  else
    _pf.SINT(node->argument()->type()->size());
  std::cout << "[DEBUG] Leaving node: SIZEOF_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_index_node(mml::index_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: INDEX_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  node->base()->accept(this, lvl);
  node->index()->accept(this, lvl);
  _pf.INT(node->type()->size()); // type size
  _pf.MUL();                     // type size * index
  _pf.ADD();                     // base + (type size * index)
  std::cout << "[DEBUG] Leaving node: INDEX_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_stack_alloc_node(mml::stack_alloc_node *const node,
                                              int lvl) {
  std::cout << "[DEBUG] Entering node: STACK_ALLOC_NODE" << std::endl;
  const auto ref = cdk::reference_type::cast(node->type())->referenced();
  node->argument()->accept(this, lvl);
  _pf.INT(ref->size()); // type size
  _pf.MUL();            // type size * argument
  _pf.ALLOC();          // allocate space for the array
  _pf.SP();             // pushes the array's address
  std::cout << "[DEBUG] Leaving node: STACK_ALLOC_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_address_of_node(mml::address_of_node *const node,
                                             int lvl) {
  std::cout << "[DEBUG] Entering node: ADDRESS_OF_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl + 2);
  std::cout << "[DEBUG] Leaving node: ADDRESS_OF_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_function_call_node(
    mml::function_call_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: FUNCTION_CALL_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  std::vector<std::shared_ptr<cdk::basic_type>> arg_types;
  if (node->function())
    // in non recursive calls, the arguments are already stored in the node itself
    arg_types = cdk::functional_type::cast(node->function()->type())->input()->components();
  else {
    // in recursive calls, we'll want to fetch the symbol associated with
    // the deepest function we can find, and retrieve its arguments
    auto deepest_function = _functions.back();
    arg_types = cdk::functional_type::cast(deepest_function->type())->input()->components();
  }

  size_t args_size = 0; // size of all the arguments in bytes
  for (int ax = node->arguments()->size() - 1; ax >= 0; ax--) {
    auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ax));
    arg->accept(this, lvl + 2);
    if (arg_types[ax]->name() == cdk::TYPE_DOUBLE && arg->type()->name() == cdk::TYPE_INT) {
      args_size += 4; // if we're passing an integer where a double is expected, we need to allocate 4 additional bytes
      _pf.I2D();      // also need to convert integer to double
    }
    args_size += arg->type()->size();
  }

  // there are 3 cases now: we may want to do a recursive, non-recursive "regular", or forwarded call
  if (node->function()) {
    // non-recursive calls
    _currentForwardLabel.clear();
    // if we accept a forwarded function, the label will once again be set
    node->function()->accept(this, lvl + 2);
    if (_currentForwardLabel.empty()) // it's a "regular" non-recursive call
      _pf.BRANCH();
    else // it's a forwarded call
      _pf.CALL(_currentForwardLabel);
  } else {
    // recursive calls
    // TODO: check if this works
    _pf.CALL(_functions.back()->name());
  }

  if (args_size > 0)
    _pf.TRASH(args_size); // removes no-longer-needed arguments from the stack
  
  switch (node->type()->name()) {
  case cdk::TYPE_INT:
  case cdk::TYPE_STRING:
  case cdk::TYPE_POINTER:
  case cdk::TYPE_VOID:
    _pf.LDFVAL32();
    break;
  case cdk::TYPE_DOUBLE:
    _pf.LDFVAL64();
    break;
  default: // can't happen!
    error(node->lineno(), "cannot call expression of unknown type");
  }

  _currentForwardLabel.clear();
  std::cout << "[DEBUG] Leaving node: FUNCTION_CALL_NODE" << std::endl;
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_function_definition_node(
    mml::function_definition_node *const node, int lvl) {
  std::cout << "[DEBUG] Entering node: FUNCTION_DEFINITION_NODE" << std::endl;
  ASSERT_SAFE_EXPRESSIONS;
  node->main() ? processMainFunction(node, lvl) : processNonMainFunction(node, lvl);
  std::cout << "[DEBUG] Leaving node: FUNCTION_DEFINITION_NODE" << std::endl;
}
void mml::postfix_writer::processMainFunction(
    mml::function_definition_node *const node, int lvl) {
  std::shared_ptr<mml::symbol> symbol;
  for (auto s_name: _symbolsToDeclare) {
    symbol = _symtab.find(s_name, 0); // FIXME: is 0 relevant here?
    if (symbol->is_foreign())
      _functionsToDeclare.insert(s_name);
    else  {
      _pf.BSS();
      _pf.ALIGN();
      _pf.LABEL(s_name);
      _pf.SALLOC(symbol->type()->size());
    }            
  }

  auto main = new_symbol();
  _functions.push_back(main);
  reset_new_symbol();

  // generate the main function itself
  _symtab.push(); // entering new context
  _pf.TEXT();
  _pf.ALIGN();
  _pf.GLOBAL("_main", _pf.FUNC());
  _pf.LABEL("_main");

  // compute stack size to be reserved for local variables
  frame_size_calculator fsc(_compiler, _symtab, main);

  _pf.ENTER(fsc.localsize());

  _inFunctionBody = true;
  node->block()->accept(this, lvl + 2);
  _inFunctionBody = false;

  _symtab.pop(); // leaving context
  if (!_returnSeen) {
    // programmers aren't forced to return anything in main; by default, we return 0
    _pf.INT(0);
    _pf.STFVAL32();
  }
  _pf.LEAVE();
  _pf.RET();

  _functions.pop_back();
  for (auto forwarded_function: _functionsToDeclare)
    _pf.EXTERN(forwarded_function);
  _returnSeen = false;
}
void mml::postfix_writer::processNonMainFunction(
    mml::function_definition_node *const node, int lvl) {
  auto function = new_symbol();
  if (function) {
    _functions.push_back(function);
    reset_new_symbol();
  }

  _currentBodyReturnLabel = mklbl(++_lbl);

  _offset = 8; // prepare for arguments (4: remember to account for return address)
  _symtab.push(); // args scope

  if (node->arguments()) {
    _inFunctionArgs = true;
    for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
      auto arg = node->arguments()->node(ix);
      if (arg == nullptr) // empty args sequence;; TODO: is this relevant?
        break;
      arg->accept(this, 0);
    }
    _inFunctionArgs = false;
  }

  _pf.TEXT(_currentBodyReturnLabel);
  _pf.ALIGN();
  // FIXME: is it needed to set the function as global?
  _pf.GLOBAL(_currentBodyReturnLabel, _pf.FUNC());
  _pf.LABEL(_currentBodyReturnLabel);

  // compute stack size to be reserved for local variables
  frame_size_calculator fsc(_compiler, _symtab, function);
  node->block()->accept(this, lvl + 2);
  _pf.ENTER(fsc.localsize());

  _offset = 0; // reset offset, prepare for local variables
  auto _previouslyInFunctionBody = _inFunctionBody;
  _inFunctionBody = true;
  node->block()->accept(this, lvl + 2);
  _inFunctionBody = _previouslyInFunctionBody;
  _symtab.pop(); // leaving args scope

  if (!_returnSeen) {
    // for cases such as void functions
    _pf.LEAVE();
    _pf.RET();
  }

  if (function)
    _functions.pop_back();
}
