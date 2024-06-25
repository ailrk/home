---
title: Monotonic queue iterator
date: 2024-06-05
description: example of customized iterator
tags: c++
---


![](/images/nichijoutrain.jpg)

---

Here is an example of writing customized iterator in C++ to solve specific problems. The `monotonic_queue_iterator` has a built-in dequeue and help you solve sliding window problems without even knowing the algorithm. 



## Implementation

The queue can be monotonic increasing or monotonic decreasing. Let's write two tag structs so we can specify the fact in the template. Later we can generate different code base on the type of the iterator we define.

```c++
struct monotonic_increasing {};
struct monotonic_decreasing {};

```

We use SFINAE to add some additional type checking for our direction tags. This is helpful but not necessary. I add it here because I like template meta programming.

```c++
template <typename Tag, typename = void>
struct is_monotonic_iterator_tag : std::false_type {};

template <typename Tag>
struct is_monotonic_iterator_tag<
    Tag, std::void_t<std::enable_if_t<
             std::conjunction_v<std::is_same<Tag, monotonic_increasing>,
                                std::is_same<Tag, monotonic_decreasing>>>>>
    : std::true_type {};
```


We define our iterator following the example of iterators in `stl`. There are several type aliases we need to provide.
```
template <typename Iter, typename Comp> class monotonic_queue_iterator {
  private:
    using T = typename std::iterator_traits<Iter>::value_type;
    std::deque<T> queue;
    size_t win_size;
    Iter first;

    Comp comp;

  public:
    using difference_type = void;
    using value_type = std::deque<T>;
    using pointer = std::deque<T> *;
    using reference = std::deque<T> &;
    using const_reference = const std::deque<T> &;
    using iterator_category = std::input_iterator_tag;

    ...
```

We put the algorithm within the constructor. The comparator is parameterized by `Comp`, we can choose what direction the queue goes by passing monotonic iterator tags defined above, and the implementation we choose the comparator for us.

```
    // initialize the first window.
    explicit constexpr monotonic_queue_iterator(Iter first, size_t window_size,
                                                const Comp &comp)
        : win_size(window_size)
        , first(first)
        , comp(comp) {
        auto it = first;
        auto max_it = it;

        for (size_t i = 0; i < win_size; ++i) {
            std::cout << *it << std::endl;
            if (comp(*it, *max_it)) {
                max_it = it;
            }

            if (i < win_size - 1)
                ++it;
        }

        queue.push_back(*max_it);

        if (max_it != it) {
            queue.push_back(*it);
        }
    }
```

We add two overloads that takes the monotonic tag iterator, then base on the tag it will forward the corresponding comparator to the real constructor.
```
    explicit constexpr monotonic_queue_iterator(Iter first, size_t win_size,
                                                monotonic_decreasing g)
        : monotonic_queue_iterator(
              first, win_size,
              [](const auto &a, const auto &b) { return a > b; }) {}

    explicit constexpr monotonic_queue_iterator(Iter first, size_t win_size,
                                                monotonic_increasing g)
        : monotonic_queue_iterator(
              first, win_size,
              [](const auto &a, const auto &b) { return a < b; }) {}
```

Because said our iterator category is `input_iterator` in the `iterator_category` field above, we need to implement whole bunch of operators that works for input iterator. Here are their definitions ll together. A lot of them are trivial, but there are some interesting points that worth noting.
```
    constexpr inline friend bool
    operator==(const monotonic_queue_iterator &self,
               const monotonic_queue_iterator &other) noexcept {
        return self.win_size == other.win_size && self.first == other.first;
    }

    constexpr inline friend bool
    operator!=(const monotonic_queue_iterator &self,
               const monotonic_queue_iterator &other) noexcept {
        return !(self == other);
    }

    constexpr inline monotonic_queue_iterator<Iter, Comp> operator++() {
        auto it = ++first;
        std::advance(it, win_size - 1);

        auto qit = queue.rbegin();
        while (!queue.empty() && qit != queue.rend() && *qit < *it) {
            qit++;
        }

        queue.erase(qit.base(), queue.end());
        queue.push_back(*it);

        return *this;
    }

    constexpr inline monotonic_queue_iterator<Iter, Comp> operator++(int) {
        monotonic_queue_iterator tmp(*this);
        ++this;
        return tmp;
    }

    constexpr inline const_reference operator*() const noexcept {
        return queue;
    }
    constexpr inline const_reference operator*() noexcept { return queue; }

    constexpr pointer operator->() noexcept {
        return std::addressof(operator*());
    }
};
```


Finally let's create a constructor that creates a pair of iterators points to the window.
```
template <typename Iter, typename Comp, typename Tag,
          typename std::enable_if_t<is_monotonic_iterator_tag<Tag>::value>>
constexpr decltype(auto) make_monotonic_queue_iterators(Iter begin, Iter end,
                                                        size_t window_size,
                                                        Tag tag) {
    return std::make_pair(
        monotonic_queue_iterator{ begin, window_size, tag },
        monotonic_queue_iterator{ end - window_size + 1, window_size, tag });
}

template <typename Iter, typename Comp>
constexpr decltype(auto) make_monotonic_queue_iterators(Iter begin, Iter end,
                                                        size_t window_size,
                                                        const Comp &comp) {
    return std::make_pair(
        monotonic_queue_iterator{ begin, window_size, comp },
        monotonic_queue_iterator{ end - window_size + 1, window_size, comp });
}

template <typename Iter, typename Comp>
constexpr decltype(auto) make_monotonic_queue_iterators(Iter begin, Iter end,
                                                        size_t window_size) {
    return std::make_pair(
        monotonic_queue_iterator{ begin, window_size, monotonic_decreasing() },
        monotonic_queue_iterator{ end - window_size + 1, window_size,
                                  monotonic_decreasing() });
}

template <typename C, typename Comp>
constexpr decltype(auto) make_monotonic_queue_iterators(C container,
                                                        size_t window_size,
                                                        const Comp &comp) {

    return std::make_pair(
        monotonic_queue_iterator{ std::begin(container), window_size, comp },
        monotonic_queue_iterator{ std::end(container) - window_size + 1,
                                  window_size, comp });
}
```

We add the following deduction guides to help with the type deduction.

```
template <typename Iter, typename Tag>
monotonic_queue_iterator(Iter, size_t, Tag) -> monotonic_queue_iterator<
    Iter, std::function<bool(typename std::iterator_traits<Iter>::value_type,
                             typename std::iterator_traits<Iter>::value_type)>>;

template <typename Iter>
monotonic_queue_iterator(Iter, size_t) -> monotonic_queue_iterator<
    Iter, std::function<bool(typename std::iterator_traits<Iter>::value_type,
                             typename std::iterator_traits<Iter>::value_type)>>;
```

## Usage



```
void main() {
    std::vector<int> vec{ 1, 3, 6, 2, 5, 1, 7, 5, 3, 9, 12 };

    auto [mit, mend] = make_monotonic_queue_iterators(vec.begin(), vec.end(), 4,
                                                      monotonic_decreasing());

    for (auto it = mit; it != mend; ++it) {
        auto q = *it;
        for (auto &v : q)
            std::cout << v << ", ";
    }
}
```



## Conclusion

We made a customized iterator that helps us to abstract away the detail of monotonic queue. From now on, to solve anything that looks like a monotonic queue problem, all we need to do is to create a pair iterators and use the easy iterator interface, instead of write the entire algorithm again. 
