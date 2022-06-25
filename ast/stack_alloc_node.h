#ifndef __L22_AST_STACK_ALLOC_NODE_H__
#define __L22_AST_STACK_ALLOC_NODE_H__

#include <cdk/ast/unary_operation_node.h>

namespace l22 {

  class stack_alloc_node: public cdk::expression_node {
    cdk::expression_node *_size;
  
  public:
    stack_alloc_node(int lineno, cdk::expression_node *size) :
        cdk::expression_node(lineno), _size(size) {
    }

  public:
    cdk::expression_node *size() {
      return _size;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_stack_alloc_node(this, level);
    }

  };

} // l22

#endif
