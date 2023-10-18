#ifndef ADSCHESS_RANDOM_HPP
#define ADSCHESS_RANDOM_HPP

#include <numeric>
#include <random>
#include <type_traits>

using big = std::int64_t;

class Random {

 private:

  std::mt19937 RNG;

 public:

  Random() : RNG(std::mt19937{std::random_device{}()}) {}
  explicit Random(int seed) {
    RNG.seed(seed);
  }

  template<typename T, std::enable_if_t<std::is_integral<T>::value, bool> = true>
  T rand_int(const T &a, const T &b) {
    T m = b - a + 1;
    if (m == 0) {
      return a;
    } else if (m == 1) {
      return (RNG() & 1) ? a : b;
    } else {
      return a + RNG() % m;
    }
  }

  template<typename T, std::enable_if_t<std::is_floating_point<T>::value, bool> = true>
  T rand_float(const T &a, const T &b) {
    static constexpr big m = 1e9;
    a *= m;
    b *= m;
    big c = rand_int<big>(big(a), big(b));
    return (T) c / (T) m;
  }

  template<typename T, std::enable_if_t<std::is_floating_point<T>::value, bool> = true>
  std::vector<T> rand_float_dist(const T &x, int n) {
    std::vector<T> vec(n);
    for (T &i : vec) {
      i = rand_float(0, x);
    }
    T s = std::accumulate(vec.begin(), vec.end(), T(0));
    for (T &i : vec) {
      i /= s;
    }
    return vec;
  }

  template<typename T>
  T rand_item(const std::vector<T> &vec) {
    int n = vec.size();
    return vec[rand_int(0, n - 1)];
  }

};


#endif //ADSCHESS_RANDOM_HPP
