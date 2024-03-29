#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated
#include "l22_parser.tab.h"

//---------------------------------------------------------------------------

void l22::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void l22::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
void l22::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  if (_inFunctionBody.back() == false)
    _pf.SDOUBLE(node->value());
  else
    _pf.DOUBLE(node->value());
}
void l22::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  _pf.INT(0);
  _pf.EQ();
}
void l22::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}
void l22::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}
void l22::postfix_writer::do_address_of_node(l22::address_of_node * const node, int lvl) {
  node->lvalue()->accept(this, lvl + 2);
}
void l22::postfix_writer::do_nullptr_node(l22::nullptr_node * const node, int lvl) {
  if (_inFunctionBody.back() == false)
    _pf.SINT(0);
  else
    _pf.INT(0);
}
void l22::postfix_writer::do_variable_declaration_node(l22::variable_declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto id = node->identifier();

  if (node->qualifier() == tFOREIGN) {
    _functions_to_declare.insert(id);
    return;
  } else if (node->qualifier() == tUSE) {
    return;
  }

  int offset = 0, typesize = node->type()->size(); 
  
  if (_inFunctionBody.back()) {
    _offsets.back() -= typesize;
    offset = _offsets.back();
  } else if (_inFunctionArgs.back()) {
    offset = _offsets.back();
    _offsets.back() += typesize;
  } else {
    offset = 0; // global variable
  }
  
  auto symbol = new_symbol();
  if (symbol) {
    symbol->set_offset(offset);
    reset_new_symbol();
  }

  if (_inFunctionBody.back()) {
    // if we are dealing with local variables, then no action is needed
    // unless an initializer exists
    if (node->initializer()) {
      node->initializer()->accept(this, lvl);
      if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_STRING) || node->is_typed(cdk::TYPE_POINTER)) {
        _pf.LOCAL(symbol->offset());
        _pf.STINT();
      } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
        if (node->initializer()->is_typed(cdk::TYPE_INT))
          _pf.I2D();
        _pf.LOCAL(symbol->offset());
        _pf.STDOUBLE();
      } else if (node->is_typed(cdk::TYPE_FUNCTIONAL)) {
        _pf.LOCAL(symbol->offset());
        _pf.STINT();
      } else {
        std::cerr << "cannot initialize" << std::endl;
      }
    }
  } else if (!_inFunctionArgs.back()) {
    if (node->initializer() == nullptr) {
      _pf.BSS();
      _pf.ALIGN();
      if (node->qualifier() == tPUBLIC)
        _pf.GLOBAL(id, _pf.OBJ());
      _pf.LABEL(id);
      _pf.SALLOC(typesize);
    } else {
      if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_DOUBLE) || node->is_typed(cdk::TYPE_POINTER)) {
        _pf.DATA();
        _pf.ALIGN();
        if (node->qualifier() == tPUBLIC)
          _pf.GLOBAL(id, _pf.OBJ());
        _pf.LABEL(id);

        if (node->is_typed(cdk::TYPE_INT)) {
          node->initializer()->accept(this, lvl);
        } else if (node->is_typed(cdk::TYPE_POINTER)) {
          node->initializer()->accept(this, lvl);
        } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
          if (node->initializer()->is_typed(cdk::TYPE_DOUBLE)) {
            node->initializer()->accept(this, lvl);
          } else if (node->initializer()->is_typed(cdk::TYPE_INT)) {
            cdk::integer_node *dclini = dynamic_cast<cdk::integer_node*>(node->initializer());
            cdk::double_node ddi(dclini->lineno(), dclini->value());
            ddi.accept(this, lvl);
          } else {
            std::cerr << node->lineno() << ": '" << id << "' has bad initializer for real value\n";
          }
        }
      } else if (node->is_typed(cdk::TYPE_STRING)) {
          int litlbl;
          _pf.RODATA();
          _pf.ALIGN();
          _pf.LABEL(mklbl(litlbl = ++_lbl));
          _pf.SSTRING(dynamic_cast<cdk::string_node*>(node->initializer())->value());
          _pf.ALIGN();
          _pf.LABEL(id);
          _pf.SADDR(mklbl(litlbl));
      } else if (node->is_typed(cdk::TYPE_FUNCTIONAL)) {
          node->initializer()->accept(this, lvl + 2);
          _pf.DATA();
          _pf.ALIGN();
          if (node->qualifier() == tPUBLIC)
            _pf.GLOBAL(id, _pf.OBJ());
          _pf.LABEL(id);
          _pf.SADDR(mklbl(_currentLabel));
      } else {
        std::cerr << node->lineno() << ": '" << id << "' has unexpected initializer\n";
      }
    }
  }
}

void l22::postfix_writer::do_block_node(l22::block_node * const node, int lvl) {
  _symtab.push(); // for block-local vars
  if (node->declarations()) node->declarations()->accept(this, lvl + 2);
  if (node->instructions()) node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}

void l22::postfix_writer::do_function_node(l22::function_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _symtab.push(); // scope of args

  push_label();
  std::string at_identifier = "@";
  auto at_symbol = std::make_shared<l22::symbol>(tPRIVATE, node->type(), at_identifier);
  at_symbol->value(_currentLabel);
  _symtab.insert(at_identifier, at_symbol);

  if (_inFunctionBody.size() == 2) {
    _pf.TEXT(_currentLabel);
    _pf.ALIGN();
    _pf.LABEL(mklbl(_currentLabel));
  } else {
    _pf.TEXT();
    _pf.ALIGN();
    _pf.LABEL(mklbl(_currentLabel));
  }

  _returnLabels.push(++_lbl);
  _offsets.push_back(0);
  _inFunctionBody.push_back(false);
  _inFunctionArgs.push_back(false);

  if (node->arguments() != nullptr) {
    _offsets.back() = 8;
    _inFunctionArgs.back() = true;
    node->arguments()->accept(this, lvl+2);
    _inFunctionArgs.back() = false;
  }

  frame_size_calculator lsc(_compiler, _symtab);
  node->accept(&lsc, lvl);
  _offsets.back() = 0;
  _pf.ENTER(lsc.localsize()); // total stack size reserved for local variables
  _inFunctionBody.back() = true;

  auto return_type = cdk::functional_type::cast(node->type())->output(0);
  push_return_type(return_type);
  node->block()->accept(this, lvl);
  _symtab.pop(); // scope of args
  _inFunctionBody.back() = false;
  pop_return_type();

  _inFunctionBody.pop_back();
  _inFunctionArgs.pop_back();
  _offsets.pop_back();

  int previous_label = pop_label();

  _pf.LABEL(mklbl(_returnLabels.top()));
  _returnLabels.pop();
  _pf.LEAVE();
  _pf.RET();

  if (_inFunctionBody.back() || _inFunctionArgs.back()) {
    auto litlbl = mklbl(++_lbl);
    _pf.DATA();
    _pf.ALIGN();
    _pf.LABEL(litlbl);
    _pf.SADDR(mklbl(previous_label));

    if ((_labels.size() == 0 || _inFunctionBody.size() == 2) && !_funcCast)
      _pf.TEXT();
    else
      _pf.TEXT(_currentLabel);
    _pf.ALIGN();

    _pf.ADDR(litlbl);
    _pf.LDINT();
  }
}

void l22::postfix_writer::do_function_call_node(l22::function_call_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (node->arguments()->size() > 0) {
    for (int i = node->arguments()->size() - 1; i >= 0; i--) {
      auto arg_type = cdk::functional_type::cast(node->function()->type())->input(i);
      if (arg_type->name() != cdk::TYPE_FUNCTIONAL)
        node->argument(i)->accept(this, lvl + 2);
      if (arg_type->name() == cdk::TYPE_DOUBLE && node->argument(i)->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
      } else if (arg_type->name() == cdk::TYPE_FUNCTIONAL) {
        _funcCast = true;
        push_label();
        _pf.TEXT(_currentLabel);
        _pf.ALIGN();
        _pf.LABEL(mklbl(_currentLabel));
        _pf.ENTER(0);

        auto arg_expected = cdk::functional_type::cast(arg_type);
        auto arg_passed = cdk::functional_type::cast(node->argument(i)->type());
        size_t offset = 8 + cdk::structured_type::cast(arg_passed->input())->size();
        for (int j = arg_expected->input_length() - 1; j >= 0; j--) {
          offset -= arg_passed->input(j)->size();
          _pf.LOCAL(offset);
          if (arg_passed->input(j)->name() == cdk::TYPE_DOUBLE) {
            _pf.LDDOUBLE();
          } else {
            _pf.LDINT();
          }

          if (arg_expected->input(j)->name() == cdk::TYPE_DOUBLE && arg_passed->input(j)->name() == cdk::TYPE_INT) {
            _pf.I2D();
          }
        }
        
        node->argument(i)->accept(this, lvl);
        
        _pf.BRANCH();

        if (cdk::structured_type::cast(arg_expected->input())->size() != 0) {
          _pf.TRASH(cdk::structured_type::cast(arg_expected->input())->size());
        }

        if (arg_passed->output(0)->name() != cdk::TYPE_VOID) {
          if (arg_passed->output(0)->name() == cdk::TYPE_DOUBLE) {
            _pf.LDFVAL64();
          } else {
            _pf.LDFVAL32();
          }

          if (arg_expected->output(0)->name() == cdk::TYPE_DOUBLE && arg_passed->output(0)->name() == cdk::TYPE_INT) {
              _pf.I2D();
          }
          
          if (arg_expected->output(0)->name() == cdk::TYPE_DOUBLE) {
            _pf.STFVAL64();
          } else {
            _pf.STFVAL32();
          }
        }
        _pf.LEAVE();
        _pf.RET();
        int previous_label = pop_label();

        auto litlbl = mklbl(++_lbl);
        _pf.DATA();
        _pf.ALIGN();
        _pf.LABEL(litlbl);
        _pf.SADDR(mklbl(previous_label));

        if (_labels.size() == 0 || _inFunctionBody.size() == 2)
          _pf.TEXT();
        else {
          _pf.TEXT(previous_label);
        }
        _pf.ALIGN();

        _pf.ADDR(litlbl);
        _pf.LDINT();
        _funcCast = false;
      }
    }
  }

  node->function()->accept(this, lvl);

  if (_externFunction != "") {
    _pf.CALL(_externFunction);
    _externFunction = "";
  } else {
    _pf.BRANCH();
  }

  size_t argsSize = cdk::structured_type::cast(cdk::functional_type::cast(node->function()->type())->input())->size();

  if (argsSize != 0) {
    _pf.TRASH(argsSize);
  }

  if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_POINTER) || node->is_typed(cdk::TYPE_STRING) || node->is_typed(cdk::TYPE_FUNCTIONAL)) {
    _pf.LDFVAL32();
  } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDFVAL64();
  }
}

void l22::postfix_writer::do_return_node(l22::return_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (node->retval() != nullptr) {
      node->retval()->accept(this, lvl + 2);

      auto return_type = get_current_return_type();

      if (return_type->name() == cdk::TYPE_INT || return_type->name() == cdk::TYPE_STRING
          || return_type->name() == cdk::TYPE_POINTER || return_type->name() == cdk::TYPE_FUNCTIONAL) {
        _pf.STFVAL32();
      } else if (return_type->name() == cdk::TYPE_DOUBLE) {
        if (node->retval()->type()->name() == cdk::TYPE_INT) _pf.I2D();
        _pf.STFVAL64();
      } else {
        std::cerr << node->lineno() << ": should not happen: unknown return type" << std::endl;
      }
  }

  _pf.JMP(mklbl(_returnLabels.top()));
}

void l22::postfix_writer::do_again_node(l22::again_node *const node, int lvl) {
  if (_whileCond.size() != 0) {
    _pf.JMP(mklbl(_whileCond.top()));
  } else {
    std::cerr << node->lineno() << ": should not happen: 'again' outside of 'while' loop" << std::endl;
  }
}

void l22::postfix_writer::do_stop_node(l22::stop_node *const node, int lvl) {
  if (_whileEnd.size() != 0) {
    _pf.JMP(mklbl(_whileEnd.top()));
  } else {
    std::cerr << node->lineno() << ": should not happen: 'stop' outside of 'while' loop" << std::endl;
  }
}

void l22::postfix_writer::do_index_node(l22::index_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->base()->accept(this, lvl);
  node->index()->accept(this, lvl);

  auto ref = cdk::reference_type::cast(node->base()->type());
  _pf.INT(3);
  _pf.SHTL();
  _pf.ADD();
}

void l22::postfix_writer::do_sizeof_node(l22::sizeof_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _pf.INT(node->expression()->type()->size());
}

void l22::postfix_writer::do_stack_alloc_node(l22::stack_alloc_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->size()->accept(this, lvl);
  _pf.INT(3);
  _pf.SHTL();
  _pf.ALLOC();
  _pf.SP();
}

void l22::postfix_writer::do_identity_node(l22::identity_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if (_inFunctionBody.back() == false)
    _pf.SINT(node->value()); // integer literal is on the stack: push an integer
  else
    _pf.INT(node->value());
}

void l22::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  int lbl1;

  /* generate the string */
  _pf.RODATA();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1 = ++_lbl));
  _pf.SSTRING(node->value());
  if (_inFunctionBody.back() || _inFunctionArgs.back()) {
    if (_labels.size() == 0 || _inFunctionBody.size() == 2) {
      _pf.TEXT();
    } else {
      _pf.TEXT(_currentLabel);
    }
    _pf.ADDR(mklbl(lbl1));
  } else {
    _pf.DATA();
    _pf.SADDR(mklbl(lbl1));
  }
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_neg_node(cdk::neg_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DNEG();
  else
    _pf.NEG();
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.INT(3);
    _pf.SHTL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.INT(3);
    _pf.SHTL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DADD();
  else
    _pf.ADD();
}

void l22::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.INT(3);
    _pf.SHTL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DSUB();
  else
    _pf.SUB();
}

void l22::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
  
  node->right()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DMUL();
  else 
    _pf.MUL();
}

void l22::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
  
  node->right()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE))
    _pf.DDIV();
  else 
    _pf.DIV();
}

void l22::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}

void l22::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();
  
  if (node->right()->type()->name() == cdk::TYPE_DOUBLE || node->right()->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.DCMP();
    _pf.INT(0);
  }
  
  _pf.LT();
}

void l22::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();
  
  if (node->right()->type()->name() == cdk::TYPE_DOUBLE || node->right()->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.DCMP();
    _pf.INT(0);
  }
  
  _pf.LE();
}

void l22::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();
  
  if (node->right()->type()->name() == cdk::TYPE_DOUBLE || node->right()->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.DCMP();
    _pf.INT(0);
  }
  
  _pf.GE();
}

void l22::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();
  
  if (node->right()->type()->name() == cdk::TYPE_DOUBLE || node->right()->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.DCMP();
    _pf.INT(0);
  }
  
  _pf.GT();
}

void l22::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
  
  node->right()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.NE();
}

void l22::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
  
  node->right()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.EQ();
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  const std::string &id = node->name();
  if (_functions_to_declare.find(id) != _functions_to_declare.end()) {
    _externFunction = id;
    return;
  }
  auto symbol = _symtab.find(id);
  if (id == "@") {
    int litlbl;
    _pf.DATA();
    _pf.ALIGN();
    _pf.LABEL(mklbl(litlbl = ++_lbl));
    _pf.SADDR(mklbl(symbol->value()));

    if (_labels.size() == 0 || _inFunctionBody.size() == 2)
      _pf.TEXT();
    else
      _pf.TEXT(_currentLabel);
    
    _pf.ALIGN();
    _pf.ADDR(mklbl(litlbl));
  } else if (symbol->offset() == 0) {
    _pf.ADDR(symbol->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void l22::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);

  if (_externFunction != "") return;
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDDOUBLE();
  } else {
    // integers, pointers, strings and functions
    _pf.LDINT();
  }
}

void l22::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    if (node->rvalue()->type()->name() == cdk::TYPE_INT) _pf.I2D();
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.STDOUBLE();
  } else {
    _pf.STINT();
  }
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_program_node(l22::program_node * const node, int lvl) {
  _pf.TEXT();
  _pf.ALIGN();
  _pf.GLOBAL("_main", _pf.FUNC());
  _pf.LABEL("_main");

  _returnLabels.push(++_lbl);
  _offsets.push_back(0);
  _inFunctionArgs.push_back(false);

  frame_size_calculator lsc(_compiler, _symtab);
  node->accept(&lsc, lvl);
  
  _pf.ENTER(lsc.localsize());
  
  _inFunctionBody.push_back(true);
  push_return_type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  
  node->statements()->accept(this, lvl);
  
  pop_return_type();
  _inFunctionArgs.pop_back();
  _inFunctionBody.pop_back();
  _offsets.pop_back();


  _pf.INT(0);
  _pf.STFVAL32();
  _pf.LABEL(mklbl(_returnLabels.top()));
  _returnLabels.pop();
  _pf.LEAVE();
  _pf.RET();

  for (std::string s : _functions_to_declare)
    _pf.EXTERN(s);
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_evaluation_node(l22::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.TRASH(node->argument()->type()->size());
}

void l22::postfix_writer::do_print_node(l22::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
    auto child = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ix));

    std::shared_ptr<cdk::basic_type> etype = child->type();
    child->accept(this, lvl); // expression to print
    if (etype->name() == cdk::TYPE_INT) {
      _functions_to_declare.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4); // trash int
    } else if (etype->name() == cdk::TYPE_DOUBLE) {
      _functions_to_declare.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8); // trash double
    } else if (etype->name() == cdk::TYPE_STRING) {
      _functions_to_declare.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4); // trash char pointer
    } else {
      std::cerr << "cannot print expression of unknown type" << std::endl;
      return;
    }

  }

  if (node->newline()) {
    _functions_to_declare.insert("println");
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_read_node(l22::read_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (node->is_typed(cdk::TYPE_INT)) {
    _pf.CALL("readi");
    _pf.LDFVAL32();
  } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.CALL("readd");
    _pf.LDFVAL64();
  }
  
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_while_node(l22::while_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  _pf.LABEL(mklbl(lbl1 = ++_lbl));
  _whileCond.push(lbl1);
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl2 = ++_lbl));
  _whileEnd.push(lbl2);
  node->block()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl1));
  _pf.LABEL(mklbl(lbl2));

  _whileCond.pop();
  _whileEnd.pop();
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_if_node(l22::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_if_else_node(l22::if_else_node * const node, int lvl) {
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