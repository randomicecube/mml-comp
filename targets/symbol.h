#ifndef __MML_TARGETS_SYMBOL_H__
#define __MML_TARGETS_SYMBOL_H__

#include <cdk/types/basic_type.h>
#include <memory>
#include <string>

namespace mml {

class symbol {
  std::shared_ptr<cdk::basic_type> _type;
  long _value; // hack!
  int _qualifier;

  int _offset = 0;

public:
  symbol(std::shared_ptr<cdk::basic_type> type, long value, int qualifier)
      : _type(type), _value(value), _qualifier(qualifier) {}

  virtual ~symbol() {
    // EMPTY
  }

  std::shared_ptr<cdk::basic_type> type() const { return _type; }
  bool is_typed(cdk::typename_type name) const { return _type->name() == name; }
  long value() const { return _value; }
  long value(long v) { return _value = v; }
  int qualifier() const { return _qualifier; }
  int qualifier(int q) { return _qualifier = q; }
};

inline auto create_symbol(std::shared_ptr<cdk::basic_type> type, long value,
                          int qualifier) {
  return std::make_shared<symbol>(type, value, qualifier);
}

} // namespace mml

#endif
