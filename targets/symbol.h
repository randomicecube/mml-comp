#ifndef __MML_TARGETS_SYMBOL_H__
#define __MML_TARGETS_SYMBOL_H__

#include <cdk/types/basic_type.h>
#include <memory>
#include <string>

namespace mml {

class symbol {
  std::shared_ptr<cdk::basic_type> _type;
  std::string _name;
  long _value; // hack!
  int _qualifier;
  bool _is_main = false;
  bool _is_foreign = false;
  bool _is_forward = false;

public:
  symbol(std::shared_ptr<cdk::basic_type> type, const std::string &name,
         long value, int qualifier)
      : _type(type), _name(name), _value(value), _qualifier(qualifier) {}

  virtual ~symbol() {
    // EMPTY
  }

  std::shared_ptr<cdk::basic_type> type() const { return _type; }
  bool is_typed(cdk::typename_type name) const { return _type->name() == name; }
  const std::string &name() const { return _name; }
  long value() const { return _value; }
  long value(long v) { return _value = v; }
  void set_main() { _is_main = true; }
  bool is_main() const { return _is_main; }
  void set_foreign() { _is_foreign = true; }
  bool is_foreign() const { return _is_foreign; }
  void set_forward() { _is_forward = true; }
  bool is_forward() const { return _is_forward; }
};

inline auto make_symbol(std::shared_ptr<cdk::basic_type> type,
                        const std::string &name, long value, int qualifier) {
  return std::make_shared<symbol>(type, name, value, qualifier);
}

} // namespace mml

#endif
