#ifndef ISTRIVIALLYPATCH
#define ISTRIVIALLYPATCH

namespace std
{
  template <typename T>
  class is_trivially_copyable
  {
  public:
    bool operator()() { return value; };
    static constexpr bool value = true;
  };

  template <typename T>
  class is_trivially_constructible
  {
  public:
    bool operator()() { return value; };
    static constexpr bool value = true;
  };
}

#endif
