#ifndef __L22_TARGETS_POSTFIX_WRITER_H__
#define __L22_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <sstream>
#include <set>
#include <stack>
#include <cdk/emitters/basic_postfix_emitter.h>

namespace l22 {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<l22::symbol> &_symtab;
    cdk::basic_postfix_emitter &_pf;
    int _lbl;
    std::set<std::string> _functions_to_declare;
    std::vector<bool> _inFunctionBody;
    std::vector<bool> _inFunctionArgs;
    std::vector<int> _offsets;
    std::vector<int> _labels;
    std::stack<int> _whileCond, _whileEnd;
    std::stack<int> _returnLabels;
    std::string _externFunction = "";
    bool _funcCast = false;
    int _currentLabel;
    int _offset = 0;

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<l22::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0) {
          _inFunctionBody.push_back(false);
          _inFunctionArgs.push_back(false);
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

    inline void push_label() {
      _labels.push_back(++_lbl);
      _currentLabel = _lbl;
    }

    inline int pop_label() {
      int previous = _labels.back();
      _labels.pop_back();
      if (_labels.size() > 0)
        _currentLabel = _labels.back();
      return previous;
    }

  public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

  };

} // l22

#endif
