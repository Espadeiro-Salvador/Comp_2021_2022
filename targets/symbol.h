#ifndef __L22_TARGETS_SYMBOL_H__
#define __L22_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace l22 {

  class symbol {
    int _qualifier;
    std::shared_ptr<cdk::basic_type> _type;
    std::string _name;
    long _value = 0; // hack!
    int _offset = 0;

  public:
    symbol(int qualifier, std::shared_ptr<cdk::basic_type> type, const std::string &name) :
        _qualifier(qualifier), _type(type), _name(name) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }

    void type(std::shared_ptr<cdk::basic_type> new_type) {
      _type = new_type;
    }

    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }
    const std::string &name() const {
      return _name;
    }
    long value() const {
      return _value;
    }
    long value(long v) {
      return _value = v;
    }
    int offset() {
      return _offset;
    }
    void set_offset(int offset) {
      _offset = offset;
    }
  };

} // l22

#endif
