#include "targets/postfix_writer.h"
#include ".auto/all_nodes.h" // all_nodes.h is automatically generated
#include "targets/type_checker.h"
#include "targets/frame_size_calculator.h"
#include <sstream>
#include <string>

#include "mml_parser.tab.h"

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
  const auto lbl = mklbl(++_lbl);
  if (_inFunctionBody) {
    // NOTE: both here, in strings and in function definitions, when we enter
    // other segments, we still need to be able to come back to our previous
    // label (instead of a random text segment)
    // in doubles in particular it's weird, as we enter an unnamed text segment
    // for (seemingly) no good reason if we're in a function body
    _pf.CALL(lbl);
    _pf.TEXT();
    _pf.ALIGN();
    _pf.LABEL(lbl);
    _pf.START();
    _pf.DOUBLE(node->value());     // load number to the stack
    _pf.STFVAL64();
    _pf.LEAVE();
    _pf.RET();
    _pf.TEXT(_bodyReturnLabels.back());
    _pf.LDFVAL64();
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
    _pf.TEXT(_bodyReturnLabels.back());                    // return to the TEXT segment
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
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT))
    _pf.I2D();
  else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    const auto ref_left = cdk::reference_type::cast(node->right()->type())->referenced();
    _pf.INT(ref_left->size());
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT))
    _pf.I2D();
  else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    const auto ref_right = cdk::reference_type::cast(node->left()->type())->referenced();
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
  else {
    _pf.SUB();
    // pointer - pointer requires a special treatment
    if (
      (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) &&
      cdk::reference_type::cast(node->left()->type())->referenced()->name() != cdk::TYPE_VOID
    ) {
      _pf.INT(cdk::reference_type::cast(node->left()->type())->referenced()->size());
      _pf.DIV();
    }
  }
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
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT))
    _pf.I2D();
  
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT))
    _pf.I2D();
  
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
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

void mml::postfix_writer::do_variable_node(cdk::variable_node *const node,
                                           int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  const auto &id = node->name();
  const auto symbol = _symtab.find(id);
  // a symbol may be global, local, or forwarded from another module
  // note how we want to check if it's foreign before if it's global,
  // as otherwise we wouldn't be CALLing it, but rather branching to it
  if (symbol->is_foreign())
    // if it's been forwarded, we won't branch to it, but rather call it;
    // as such, we'll needs its label (note that this'll be useful in
    // function calls)
    _currentForwardLabel = symbol->name();
  else if (symbol->is_global())
    _pf.ADDR(symbol->name());
  else
    _pf.LOCAL(symbol->offset());
}

void mml::postfix_writer::do_rvalue_node(cdk::rvalue_node *const node,
                                         int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDDOUBLE();
  } else {
    // integers, pointers, strings, functionals
    // note that if we're dealing with forwarded methods, we don't want to
    // branch to them, and as such, loading its first instruction address
    // is irrelevant (as we'll just call it by its label)
    if (_currentForwardLabel.empty())
      _pf.LDINT();
  }
}

void mml::postfix_writer::do_assignment_node(cdk::assignment_node *const node,
                                             int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    if (node->rvalue()->is_typed(cdk::TYPE_INT))
      _pf.I2D();
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE))
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
  int lbl = ++_lbl;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl));
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
  const auto whileLabels = _whileCond.size();
  if (whileLabels == 0) {
    error(node->lineno(), "stop node found outside a while block");
    return;
  }
  const size_t stopLvl = (size_t) node->level();
  if (stopLvl > whileLabels || stopLvl < 1) {
    error(node->lineno(), "invalid stop level");
    return;
  }
  const auto whileEndLbl = _whileEnd[whileLabels - stopLvl];
  _pf.JMP(mklbl(whileEndLbl));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_next_node(mml::next_node *const node, int lvl) {
  const auto whileLabels = _whileCond.size();
  if (whileLabels == 0) {
    error(node->lineno(), "next node found outside a while block");
    return;
  }
  const size_t nextLvl = (size_t) node->level();
  if (nextLvl > whileLabels || nextLvl < 1) {
    error(node->lineno(), "invalid next level");
    return;
  }
  const auto whileCondLbl = _whileCond[whileLabels - nextLvl];
  _pf.JMP(mklbl(whileCondLbl));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_return_node(mml::return_node *const node,
                                         int lvl) {
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
      if (!_functions.back()->is_main()) {
        // allowing covariant return types (i.e., double is considered a valid return type to cast from int)
        // we'll always return doubles from non-main functions (main returns 0 as per convention)
        // instead of ints, to allow covariance
        // the second part of this logic is handled in the function call's visitor, where we _load_ the return value,
        // which should be the address of the first instruction of the function being called
        _pf.I2D();
        _pf.STFVAL64();
      } else {
        _pf.STFVAL32();
      }
      break;
    case cdk::TYPE_STRING:
    case cdk::TYPE_POINTER:
    case cdk::TYPE_FUNCTIONAL:
      _pf.STFVAL32(); // removes 4 bytes (an int) from the stack
      break;
    case cdk::TYPE_DOUBLE:
      if (node->retval()->is_typed(cdk::TYPE_INT))
        _pf.I2D(); // converts int to double
      _pf.STFVAL64(); // removes 8 bytes (a double) from the stack
      break;
    default:
      error(node->lineno(), "invalid return type");
    }
  }

  _pf.LEAVE(); // leaves the function, destroys its local stack data
  _pf.RET(); // returns from a function -- the value being returned has been removed from the stack
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_declaration_node(mml::declaration_node *const node,
                                              int lvl) {
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
}
void mml::postfix_writer::processLocalVariableInitialization(std::shared_ptr<mml::symbol> symbol, cdk::expression_node *const initializer, int lvl) {
  initializer->accept(this, lvl);
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
      initializer->accept(this, lvl);
      _pf.DATA(); // Data segment, for global variables
      _pf.ALIGN();
      if (symbol->qualifier() == tPUBLIC) // TODO: is this needed?
        _pf.GLOBAL(symbol->name(), _pf.OBJ());
      _pf.LABEL(symbol->name());
      _pf.SADDR(_currentBodyReturnLabel);
      _currentBodyReturnLabel.clear();
      break;
    default:
      error(initializer->lineno(), "invalid type for variable initialization");
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_block_node(mml::block_node *const node, int lvl) {
  _symtab.push();
  if (node->declarations())
    node->declarations()->accept(this, lvl + 2);
  if (node->instructions())
    node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
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
  ASSERT_SAFE_EXPRESSIONS;
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

void mml::postfix_writer::do_function_call_node(
    mml::function_call_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::vector<std::shared_ptr<cdk::basic_type>> arg_types;
  const auto function = node->function();
  if (function)
    // in non recursive calls, the arguments are already stored in the node itself
    arg_types = cdk::functional_type::cast(function->type())->input()->components();
  else {
    // in recursive calls, we'll want to fetch the symbol associated with
    // the deepest function we can find, and retrieve its arguments
    auto deepest_function = _functions.back();
    arg_types = cdk::functional_type::cast(deepest_function->type())->input()->components();
  }

  size_t args_size = 0; // size of all the arguments in bytes
  if (node->arguments()) {
    for (int ax = node->arguments()->size() - 1; ax >= 0; ax--) {
      auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ax));
      arg->accept(this, lvl + 2);
      if (arg_types[ax]->name() == cdk::TYPE_DOUBLE && arg->type()->name() == cdk::TYPE_INT) {
        args_size += 4; // if we're passing an integer where a double is expected, we need to allocate 4 additional bytes
        _pf.I2D();      // also need to convert integer to double
      }
      args_size += arg->type()->size();
    }
  }

  // there are 3 cases now: we may want to do a recursive, non-recursive "regular", or forwarded call
  if (function) {
    // non-recursive calls
    _currentForwardLabel.clear();
    // if we accept a forwarded function, the label will once again be set
    function->accept(this, lvl + 2);
    if (_currentForwardLabel.empty()) // it's a "regular" non-recursive call
        _pf.BRANCH();
    else // it's a forwarded call
      _pf.CALL(_currentForwardLabel);
  } else {
    // recursive calls
    _pf.CALL(_bodyReturnLabels.back());
  }

  if (args_size > 0)
    _pf.TRASH(args_size); // removes no-longer-needed arguments from the stack
  
  switch (node->type()->name()) {
  case cdk::TYPE_VOID:
    break;
  case cdk::TYPE_INT:
    if (_currentForwardLabel.empty()) {
      // the second part of allowing covariance to happen (with the first one being handled in the return node's visitor)
      // there, we make every non-main int-returning function actually return a double
      // here, we convert that double back to an int, as it is the callee's responsibility to properly cast the return values
      _pf.LDFVAL64();
      _pf.D2I();
    } else {
      // note how in forwarded methods we don't need to do any conversion, as the return value is already an int
      _pf.LDFVAL32();
    }
    break;
  case cdk::TYPE_STRING:
  case cdk::TYPE_POINTER:
  case cdk::TYPE_FUNCTIONAL:
    _pf.LDFVAL32();
    break;
  case cdk::TYPE_DOUBLE:
    _pf.LDFVAL64();
    break;
  default: // can't happen!
    error(node->lineno(), "cannot call expression of unknown type");
  }

  _currentForwardLabel.clear();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_function_definition_node(
    mml::function_definition_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->main() ? processMainFunction(node, lvl) : processNonMainFunction(node, lvl);
}
void mml::postfix_writer::processMainFunction(
    mml::function_definition_node *const node, int lvl) {
  for (auto s_name: _symbolsToDeclare) {
    auto symbol = _symtab.find(s_name, 0); // FIXME: is 0 relevant here?
    if (symbol->is_foreign())
      _functionsToDeclare.insert(s_name);
    else  {
      _pf.BSS();
      _pf.ALIGN();
      _pf.LABEL(s_name);
      _pf.SALLOC(symbol->type()->size());
    }            
  }
  // Note that it's ok to name the function _main, as no variable may have underscores
  const auto main = mml::make_symbol(cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT)), "_main", 0, tPRIVATE);
  main->set_main();
  _functions.push_back(main);
  _bodyReturnLabels.push_back("_main");

  // generate the main function itself
  _symtab.push(); // entering new context
  _pf.TEXT(_bodyReturnLabels.back());
  _pf.ALIGN();
  _pf.GLOBAL("_main", _pf.FUNC());
  _pf.LABEL("_main");

  // compute stack size to be reserved for local variables
  frame_size_calculator fsc(_compiler, _symtab, main);
  _symtab.push(); // entering new context
  node->accept(&fsc, lvl);
  _symtab.pop(); // leaving context
  _pf.ENTER(fsc.localsize());

  _inFunctionBody = true;
  const bool previous_return_seen = _returnSeen;
  _returnSeen = false;
  node->block()->accept(this, lvl + 2);
  _inFunctionBody = false;

  _symtab.pop(); // leaving context
  _bodyReturnLabels.pop_back();
  if (!_returnSeen) {
    // programmers aren't forced to return anything in main; by default, we return 0
    _pf.INT(0);
    _pf.STFVAL32();
    _pf.LEAVE();
    _pf.RET();
  }

  _functions.pop_back();
  for (auto forwarded_function: _functionsToDeclare)
    _pf.EXTERN(forwarded_function);
  _returnSeen = previous_return_seen;
}
void mml::postfix_writer::processNonMainFunction(
    mml::function_definition_node *const node, int lvl) {
  auto function = make_symbol(node->type(), "@", 0, tPRIVATE);
  if (_symtab.find_local(function->name())) {
    _symtab.replace(function->name(), function);
  } else {
    _symtab.insert(function->name(), function);
  }
  _functions.push_back(function);

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

  const auto currentBodyReturnLabel = mklbl(++_lbl);
  _bodyReturnLabels.push_back(currentBodyReturnLabel);
  _pf.TEXT(currentBodyReturnLabel);
  _pf.ALIGN();
  _pf.LABEL(currentBodyReturnLabel);

  // compute stack size to be reserved for local variables
  frame_size_calculator fsc(_compiler, _symtab, function);
  _symtab.push();
  node->accept(&fsc, lvl);
  _symtab.pop();
  _pf.ENTER(fsc.localsize());

  _offset = 0; // reset offset, prepare for local variables
  auto _previouslyInFunctionBody = _inFunctionBody;
  _inFunctionBody = true;
  if (node->block())
    node->block()->accept(this, lvl + 2);
  _inFunctionBody = _previouslyInFunctionBody;
  _symtab.pop(); // leaving args scope

  if (!_returnSeen) {
    _pf.LEAVE();
    _pf.RET();
  }

  _bodyReturnLabels.pop_back();
  _currentBodyReturnLabel = currentBodyReturnLabel;
  if (function)
    _functions.pop_back();
  
  if (_inFunctionBody) {
    _pf.TEXT(_bodyReturnLabels.back());
    _pf.ADDR(_currentBodyReturnLabel);
  }
}
