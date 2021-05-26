//
// Author: Stanislaw Adaszewski, 2019
//

/*template<typename T, typename... Args>
std::unique_ptr<T> make_unique(Args&&... args) {
  return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}*/

template<class T, int DIM, int MAX_CHILDREN, class DATA> class rtree
{
public:
  typedef rtree<T, DIM, MAX_CHILDREN, DATA> type;
  typedef const type const_type;
  typedef type *type_ptr;
  typedef const type *type_const_ptr;
  typedef std::array<T, DIM * 2> bounds_type;
  typedef DATA data_type;

  rtree(): m_is_leaf(false), m_data()
  {
    for (auto i = 0; i < DIM; i++)
    {
      m_bounds[i] = std::numeric_limits<T>::max();
      m_bounds[i + DIM] = std::numeric_limits<T>::min();
    }
  }

  rtree(data_type data, const bounds_type &bounds): m_is_leaf(true), m_data(data), m_bounds(bounds)
  {
    for (auto i = 0; i < DIM; i++)
      if (bounds[i] > bounds[i + DIM])
        throw std::runtime_error("Bounds minima have to be less than maxima");
  }

  void insert(data_type data, const bounds_type &bounds)
  {
    if (m_is_leaf)
      throw std::runtime_error("Cannot insert into leaves");

    m_bounds = updated_bounds(bounds);
    if (m_children.size() < MAX_CHILDREN)
    {
      auto r = std::make_unique<type>(data, bounds);
      m_children.push_back(std::move(r));
      return;
    }

    std::reference_wrapper<type> best_child = *m_children.begin()->get();
    auto best_volume = volume(best_child.get().updated_bounds(bounds));
    for (auto it = ++m_children.begin(); it != m_children.end(); it++)
    {
      auto v = volume((*it)->updated_bounds(bounds));
      if (v < best_volume)
      {
        best_volume = v;
        best_child = *it->get();
      }
    }

    if (!best_child.get().is_leaf())
    {
      best_child.get().insert(data, bounds);
      return;
    }

    auto leaf = std::make_unique<type>(best_child.get().data(), best_child.get().bounds());
    best_child.get().m_is_leaf = false;
    best_child.get().m_data = data_type();
    best_child.get().m_children.push_back(std::move(leaf));
    best_child.get().insert(data, bounds);
  }

  void intersection(const bounds_type &bounds, std::vector<std::reference_wrapper<const_type>> &res) const
  {
    if (!intersects(bounds))
      return;

    if (m_is_leaf)
    {
      res.push_back(*this);
      return;
    }

    for (auto &ch : m_children)
      ch->intersection(bounds, res);
  }

  std::vector<std::reference_wrapper<const_type>> intersection(const bounds_type& bounds) const
  {
    std::vector<std::reference_wrapper<const_type>> res;
    intersection(bounds, res);
    return res;
  }

  bool intersects(const bounds_type &bounds) const
  {
    for (auto i = 0; i < DIM; i++)
    {
      if (m_bounds[i] > bounds[i + DIM])
        return false;
      if (m_bounds[i + DIM] < bounds[i])
        return false;
    }
    return true;
  }

  void erase(data_type data, const bounds_type &bounds)
  {
    if (m_is_leaf)
      throw std::runtime_error("Cannot erase from leaves");

    if (!intersects(bounds))
      return;

    for (auto it = m_children.begin(); it != m_children.end(); )
    {
      if (!(*it)->m_is_leaf)
      {
        (*it)->erase(data, bounds);
        it++;
      }
      else if ((*it)->m_data == data && (*it)->m_bounds == bounds)
      {
        m_children.erase(it++);
      }
      else
        it++;
    }
  }

  void print(int level = 0)
  {
    // print the entire tree

    for (auto it = m_children.begin(); it != m_children.end(); )
    {
      auto bounds = (*it)->m_bounds;
      std::string pad(level, '\t');
      if ((*it)->m_is_leaf)
      {
        printf ("%s leaf %0.6f %0.6f \n", pad.c_str(), bounds[0], bounds[1]);
      }
      else
      {
        printf ("%s branch %0.6f %0.6f %0.6f %0.6f \n", pad.c_str(), bounds[0], bounds[1], bounds[2], bounds[3]);
        (*it)->print(level + 1);
      }
      it++;
    }
  }

  bounds_type updated_bounds(const bounds_type &child_bounds) const
  {
    bounds_type res;
    for (auto i = 0; i < DIM; i++)
    {
      res[i] = std::min(child_bounds[i], m_bounds[i]);
      res[i + DIM] = std::max(child_bounds[i + DIM], m_bounds[i + DIM]);
    }
    return res;
  }

  static T volume(const bounds_type &bounds)
  {
    T res = 1;
    for (auto i = 0; i < DIM; i++)
    {
      auto delta = bounds[i + DIM] - bounds[i];
      res *= delta;
    }
    return res;
  }

  const bounds_type& bounds() const
  {
    return m_bounds;
  }

  bool is_leaf() const
  {
    return m_is_leaf;
  }

  data_type data() const
  {
    return m_data;
  }

  const std::list<std::unique_ptr<type>>& children() const
  {
    return m_children;
  }

  static std::string bounds_to_string(const bounds_type &bounds)
  {
    std::string res = "( ";
    for (auto i = 0; i < DIM * 2; i++)
    {
      if (i > 0)
        res += ", ";
      res += std::to_string(bounds[i]);
    }
    res += " )";
    return res;
  }

  void to_string(std::string &res, int tab) const
  {
    std::string pad(tab, '\t');

    if (m_is_leaf)
    {
      res += pad + "{ data: " + std::to_string(m_data) +
        ", bounds: " + bounds_to_string(m_bounds) +
        " }";
      return;
    }

    res += pad + "{ bounds: " + bounds_to_string(m_bounds) +
      ", children: [\n";
    auto i = 0;
    for (auto &ch : m_children)
    {
      if (i++ > 0)
        res += "\n";
      ch->to_string(res, tab + 1);
    }
    res += "\n" + pad + "]}";
  }

  std::string to_string() const
  {
    std::string res;
    to_string(res, 0);
    return res;
  }

private:
  bool m_is_leaf;
  data_type m_data;
  std::list<std::unique_ptr<type>> m_children;
  bounds_type m_bounds;
};
