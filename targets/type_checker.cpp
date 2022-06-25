#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

void l22::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void l22::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}
void l22::type_checker::do_block_node(l22::block_node *const node, int lvl) {
  // EMPTY
}
void l22::type_checker::do_again_node(l22::again_node *const node, int lvl) {
  // EMPTY
}
void l22::type_checker::do_stop_node(l22::stop_node *const node, int lvl) {
  // EMPTY
}


void l22::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++)
    node->node(i)->accept(this, lvl);
}

void l22::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (node->argument()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong type in unary logical expression");
  }
}

void l22::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  do_BinaryLogicalExpression(node, lvl);
}

void l22::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  do_BinaryLogicalExpression(node, lvl);
}

void l22::type_checker::do_address_of_node(l22::address_of_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);

  if (node->lvalue()->is_typed(cdk::TYPE_FUNCTIONAL)) {
    node->type(node->lvalue()->type());
  } else {
    node->type(cdk::reference_type::create(4, node->lvalue()->type()));
  }
}

void l22::type_checker::do_variable_declaration_node(l22::variable_declaration_node *const node, int lvl) {
  if (node->initializer() != nullptr) {
    node->initializer()->accept(this, lvl + 2);

    if (node->is_typed(cdk::TYPE_INT)) {
      if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)) {
        node->initializer()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      }
      if (!node->initializer()->is_typed(cdk::TYPE_INT)) 
        throw std::string("wrong type for initializer (integer expected).");
    } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
      if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)) {
        node->initializer()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      }
      if (!node->initializer()->is_typed(cdk::TYPE_INT) && !node->initializer()->is_typed(cdk::TYPE_DOUBLE))
        throw std::string("wrong type for initializer (integer or double expected).");
    } else if (node->is_typed(cdk::TYPE_STRING)) {
      if (!node->initializer()->is_typed(cdk::TYPE_STRING))
        throw std::string("wrong type for initializer (string expected).");
    } else if (node->is_typed(cdk::TYPE_POINTER)) {
      if (node->type() != node->initializer()->type())
        throw std::string("wrong type for initializer (pointer expected).");
    } else if (node->is_typed(cdk::TYPE_FUNCTIONAL)) {
      if (node->type() != node->initializer()->type())
        throw std::string("wrong type for initializer (function expected).");
      node->type(node->initializer()->type());
    } else if (node->is_typed(cdk::TYPE_UNSPEC)) {
      if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)) {
        node->initializer()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      }
      node->type(node->initializer()->type());
    } else {
      throw std::string("unknown type for initializer.");
    }
  }
  
  const std::string &id = node->identifier();
  auto symbol = std::make_shared<l22::symbol>(node->qualifier(), node->type(), id);
  if (_symtab.insert(id, symbol)) {
    _parent->set_new_symbol(symbol);
  } else {
    auto existing_symbol = _symtab.find_local(id);
    if (existing_symbol->type() != symbol->type())
      throw std::string("variable '" + id + "' redeclared");
  }
}

void l22::type_checker::do_function_node(l22::function_node *const node, int lvl) {
  // Functions with no arguments already have the correct type
  if (node->arguments()) {
    auto return_type = cdk::functional_type::cast(node->type())->output(0);

    std::vector<std::shared_ptr<cdk::basic_type>> arg_types;
    for (size_t i = 0; i < node->arguments()->size(); i++)
      arg_types.push_back(node->argument(i)->type());
    
    node->type(cdk::functional_type::create(arg_types, return_type));
  }
}

void l22::type_checker::do_function_call_node(l22::function_call_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->function()->accept(this, lvl);

  if (!node->function()->is_typed(cdk::TYPE_FUNCTIONAL))
    throw std::string("expression isn't a function");

  node->type(cdk::functional_type::cast(node->function()->type())->output(0));
  node->arguments()->accept(this, lvl);

  for (size_t i = 0; i < node->arguments()->size(); i++) {
      auto arg_type = cdk::functional_type::cast(node->function()->type())->input(i);
      if (node->argument(i)->is_typed(cdk::TYPE_INT)) {
        if (arg_type->name() != cdk::TYPE_INT && arg_type->name() != cdk::TYPE_DOUBLE)
          throw std::string("invalid argument");
      } else if (node->argument(i)->type() != cdk::functional_type::cast(node->function()->type())->input(i)) {
        throw std::string("invalid argument");
      }
  }
}

void l22::type_checker::do_return_node(l22::return_node *const node, int lvl) {
  if (node->retval() != nullptr) {
      node->retval()->accept(this, lvl + 2);
  }
}

void l22::type_checker::do_index_node(l22::index_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->base()->accept(this, lvl + 2);
  node->index()->accept(this, lvl + 2);

  if (!node->base()->is_typed(cdk::TYPE_POINTER)) {
    throw std::string("pointer expression expected in pointer indexing");
  }

  if (!node->index()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expected in index");
  }

  auto refType = cdk::reference_type::cast(node->base()->type());
  node->type(refType->referenced());
}

void l22::type_checker::do_sizeof_node(l22::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->expression()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void l22::type_checker::do_stack_alloc_node(l22::stack_alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->size()->accept(this, lvl + 2);

  if (node->size()->is_typed(cdk::TYPE_UNSPEC)) {
    node->size()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  
  if (!node->size()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in stack allocation expression");
  }

  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(1, cdk::TYPE_VOID)));
}

void l22::type_checker::do_identity_node(l22::identity_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (node->argument()->is_typed(cdk::TYPE_INT) || node->argument()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(node->argument()->type());
  } else {
    throw std::string("integer or double expression expected in identity expression");
  }
}


//---------------------------------------------------------------------------

void l22::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void l22::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void l22::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

void l22::type_checker::do_nullptr_node(l22::nullptr_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(1, cdk::TYPE_VOID)));
}

//---------------------------------------------------------------------------

void l22::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  
  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (node->argument()->is_typed(cdk::TYPE_INT) || node->argument()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(node->argument()->type());
  } else {
    throw std::string("wrong type in argument of unary expression");
  }
}

//---------------------------------------------------------------------------

void l22::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  do_PIDExpression(node, lvl);
}
void l22::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  do_PIDExpression(node, lvl);
}
void l22::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  do_IDExpression(node, lvl);
}
void l22::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  do_IDExpression(node, lvl);
}
void l22::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  do_IExpression(node, lvl);
}
void l22::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  do_ScalarLogicalExpression(node, lvl);
}
void l22::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  do_ScalarLogicalExpression(node, lvl);
}
void l22::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  do_ScalarLogicalExpression(node, lvl);
}
void l22::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  do_ScalarLogicalExpression(node, lvl);
}
void l22::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  do_GeneralLogicalExpression(node, lvl);
}
void l22::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  do_GeneralLogicalExpression(node, lvl);
}

//---------------------------------------------------------------------------

void l22::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<l22::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void l22::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void l22::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->rvalue()->accept(this, lvl + 2);

  if (node->lvalue()->is_typed(cdk::TYPE_INT)) {
    if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else {
      throw std::string("wrong assignment to integer");
    }
  } else if (node->lvalue()->is_typed(cdk::TYPE_POINTER)) {
    if (node->rvalue()->is_typed(cdk::TYPE_POINTER)) {
      node->type(node->rvalue()->type());
    } else if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_POINTER));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
    } else {
      throw std::string("wrong assignment to pointer");
    }

  } else if (node->lvalue()->is_typed(cdk::TYPE_DOUBLE)) {

    if (node->rvalue()->is_typed(cdk::TYPE_DOUBLE) || node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->rvalue()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else {
      throw std::string("wrong assignment to real");
    }

  } else if (node->lvalue()->is_typed(cdk::TYPE_STRING)) {
    if (node->rvalue()->is_typed(cdk::TYPE_STRING)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } else {
      throw std::string("wrong assignment to string");
    }
  } else if (node->lvalue()->is_typed(cdk::TYPE_FUNCTIONAL)) {

    if (node->rvalue()->is_typed(cdk::TYPE_FUNCTIONAL)) {
      node->type(node->rvalue()->type());
      auto name = dynamic_cast<cdk::variable_node*>(node->lvalue())->name();
      auto symbol = _symtab.find(name);
      if (symbol != nullptr)
        symbol->type(node->rvalue()->type());
    } else {
      throw std::string("wrong assignment to function");
    }
  } else {
    throw std::string("wrong types in assignment");
  }
  
}

//---------------------------------------------------------------------------

void l22::type_checker::do_program_node(l22::program_node *const node, int lvl) {
  // EMPTY
}

void l22::type_checker::do_evaluation_node(l22::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void l22::type_checker::do_print_node(l22::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void l22::type_checker::do_read_node(l22::read_node *const node, int lvl) {
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void l22::type_checker::do_while_node(l22::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void l22::type_checker::do_if_node(l22::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

void l22::type_checker::do_if_else_node(l22::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//--------------------------------------------------------------------------

void l22::type_checker::do_BinaryLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (!node->left()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in binary expression");
  }

  if (!node->right()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in binary expression");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void l22::type_checker::do_IExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

   if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (!node->left()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in binary operator (left)");
  }

  if (!node->right()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in binary operator (right)");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void l22::type_checker::do_IDExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong types in binary expression");
  }
}

void l22::type_checker::do_PIDExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(node->left()->type());
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    node->type(node->right()->type());
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong types in binary expression");
  }
}

void l22::type_checker::do_ScalarLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  
  if (!node->left()->is_typed(cdk::TYPE_INT) && !node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    throw std::string("integer or double expression expected in binary logical expression (left)");
  }

  if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    throw std::string("integer or double expression expected in binary logical expression (right)");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void l22::type_checker::do_GeneralLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  
  if (node->left()->type() != node->right()->type()) {
    throw std::string("same type expected on both sides of equality operator");
  }
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}