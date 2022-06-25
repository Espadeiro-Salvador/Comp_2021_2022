#ifndef __L22_BASIC_AST_VISITOR_H__
#define __L22_BASIC_AST_VISITOR_H__

#include <string>
#include <memory>
#include <vector>
#include <iostream>
#include <cdk/compiler.h>
#include <cdk/symbol_table.h>
#include "targets/symbol.h"

/* do not edit -- include node forward declarations */
#define __NODE_DECLARATIONS_ONLY__
#include ".auto/all_nodes.h"  // automatically generated
#undef __NODE_DECLARATIONS_ONLY__
/* do not edit -- end */

//!
//! Print nodes as XML elements to the output stream.
//!
class basic_ast_visitor {
protected:
  //! The owner compiler
  std::shared_ptr<cdk::compiler> _compiler;

private:

  // last symbol inserted in symbol table
  std::shared_ptr<l22::symbol> _new_symbol;
  std::vector<std::shared_ptr<cdk::basic_type>> _returns;

protected:
  basic_ast_visitor(std::shared_ptr<cdk::compiler> compiler) :
      _compiler(compiler) {
  }

  bool debug() {
    return _compiler->debug();
  }

  std::ostream &os() {
    return *_compiler->ostream();
  }

public:
  virtual ~basic_ast_visitor() {
  }

public:
  std::shared_ptr<l22::symbol> new_symbol() {
    return _new_symbol;
  }

  void set_new_symbol(std::shared_ptr<l22::symbol> symbol) {
    _new_symbol = symbol;
  }

  void reset_new_symbol() {
    _new_symbol = nullptr;
  }

  void push_return_type(std::shared_ptr<cdk::basic_type> return_type) {
    _returns.push_back(return_type);
  }

  void pop_return_type() {
    _returns.pop_back();
  }

  std::shared_ptr<cdk::basic_type> get_current_return_type() {
    return _returns.back();
  }

public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#define __PURE_VIRTUAL_DECLARATIONS_ONLY__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __PURE_VIRTUAL_DECLARATIONS_ONLY__
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

};

#endif
