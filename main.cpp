#include <cassert>
#include <functional>
#include <memory>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>
#include <set>
#include <tuple>

/** http://enwp.org/Trie
 *  --------------------

In computer science, a trie, also called digital tree or prefix tree, is a type
of search tree, a tree data structure used for locating specific keys from
within a set. These keys are most often strings, with links between nodes
defined not by the entire key, but by individual characters. In order to access
a key (to recover its value, change it, or remove it), the trie is traversed
depth-first, following the links between nodes, which represent each character
in the key.

*/

template <class T, class Compare = std::less<std::string>, class Allocator = std::allocator<std::pair<const std::string, T> > >
class stupid_trie {
    public:
    typedef std::string key_type;
    typedef T mapped_type;
    typedef std::pair<const key_type, mapped_type> value_type;
    typedef std::pair<const key_type, std::optional<mapped_type> > optional_value_type;
    //typedef std::pair<const std::string, std::optional<T> > value_type;
    typedef Compare key_compare;
    typedef Allocator allocator_type;
    struct trie_node;

    typedef std::set<trie_node*, key_compare> children_container_type;

    template<typename Type>
    class trie_iterator;

    typedef trie_iterator<trie_node> iterator;
    typedef trie_iterator<const trie_node> const_iterator;

    struct trie_node{
      optional_value_type value;
      trie_node* parent;
      children_container_type children;

      
      trie_node() : value(optional_value_type("", std::nullopt)), parent(nullptr), children(children_container_type()) {}
      trie_node(optional_value_type _val, trie_node* _pptr, children_container_type _chld) : value(_val), parent(_pptr), children(_chld) {}

      bool operator==(const trie_node& other) const {return (value == other.value && parent == other.parent && children == other.children) ? true : false;}
    };

    private:
    trie_node head = trie_node();
    
    public:

    //NODE_TYPE is either trie_node or const trie_node
    template <typename NODE_TYPE>
    class trie_iterator : public std::iterator<std::forward_iterator_tag, NODE_TYPE> {
        //note: iterator is deprecated from c++17??
      
      NODE_TYPE* curr;

      public:
      explicit trie_iterator(NODE_TYPE* _elem) : curr(_elem) {}
        //iterate through the child elements, and recursively look for anything that has value, if none can be found, look into the parent too
      trie_iterator<NODE_TYPE> operator++(int)
      {
          auto it = curr->children.begin();
          while (it != curr->children.end() && (nullptr == first_valid<NODE_TYPE*>(*it)))
          {
            ++it;
          }
          return (it == curr->children.end()) ? trie_iterator<NODE_TYPE>(backtrack<NODE_TYPE*>(curr)) : trie_iterator<NODE_TYPE>(*it);
      }

      trie_iterator<NODE_TYPE> operator++()
      {
        auto it = curr->children.begin();
          while (it != curr->children.end() && (nullptr == first_valid<NODE_TYPE*>(*it)))
          {
            ++it;
          }
          (it == curr->children.end()) ? curr = backtrack<NODE_TYPE*>(curr) : curr = *it;
          return *this;
          
      }

      //TODO operator--
      trie_iterator& operator=(const trie_iterator& other) {this->curr = other->curr; return this;}
      bool operator==(const trie_iterator& other) const {return curr == (other.curr);}
      bool operator!=(const trie_iterator& other) const {return !(*this == other);}
      NODE_TYPE& operator*() const { return curr; }
      NODE_TYPE * operator->() { return curr; }
      NODE_TYPE const * operator->() const { return curr; }
    };

    

    //begin should be the first valid element found, if empty then the head element
    iterator begin() {
      trie_node* ret = first_valid<trie_node*>(&head);
      return (nullptr == ret) ? iterator(&head) : iterator(ret);
    }
    const_iterator begin() const {
      const trie_node* ret = first_valid<const trie_node*>(&head);
      return (nullptr == ret) ? const_iterator(&head) : const_iterator(ret);
    }

    //returns the first valid element from the children set, if none can be found recursively, returns a nullptr
    template<typename NODE_TYPE_PTR> //NODE_TYPE_PTR is either trie_node* or const trie_node*
    NODE_TYPE_PTR first_valid(NODE_TYPE_PTR nodeptr) const{
      if (nodeptr->value.second.has_value()){ return nodeptr; }
      auto it = nodeptr->children.begin();
      while (it != nodeptr->children.end() && (nullptr == first_valid(*it)) ){++it;}
      return (nodeptr->children.end() == it) ? nullptr : *it;
    }
    //end should be the head element
    iterator end() {return iterator(&head);}
    const_iterator end() const {return const_iterator(&head);}

    //called when there are no more valid children of the node to iterate on, must go a level higher to find next
    template<typename NODE_TYPE_PTR> //NODE_TYPE_PTR is either trie_node* or const trie_node*
    NODE_TYPE_PTR backtrack(NODE_TYPE_PTR curr_node_ptr) const{
      if (nullptr == curr_node_ptr->parent) {return curr_node_ptr;} // its the root element, can't backtrack further, return it for end() comparison
      auto continue_from = ++(curr_node_ptr->parent->children.find(curr_node_ptr->value.first));
      while (continue_from!=curr_node_ptr->parent->children.end() && (nullptr == first_valid(continue_from)))
      {
        ++continue_from;
      }
      //if the parent doesn't have any valid descendants, go another level higher
      return (continue_from == curr_node_ptr->parent->children.end()) ? 
      backtrack(curr_node_ptr->parent) : 
      continue_from;
    }

    stupid_trie() : head(trie_node()) {}; //TODO give a parent to the head element and make that end? if "" key has value it can break

    //TODO operator =, should work on const too, should only work if the type is copyable, otherwise std::move

    //TODO operator[], return an optional
    std::optional<mapped_type>& operator[](key_type _key) const{
      trie_node* ret = traverse_branch(_key);
      return (ret->value.second.has_value()) ? ret->value.second : std::nullopt;
    }
    

    ~stupid_trie() = default;

//TODO count fnc - can the key be found?
    int count(key_type _key) const{
      int counter = 0; //note: there should be no duplicate keys, this function should always return 0 or 1
      for (auto it = (*this).begin(); it != (*this).end(); ++it) { 
        if (_key == (it->value).first) { ++counter; }
      }
      return counter;
    }
//TODO implement size fnc (count the values, not the keys) -also iterator?
    int size() const{
      int ret = 0;
      for (auto it = begin(); it != end(); ++it) { ++ret; }
      return ret;
    }
//TODO is_empty fnc, 0 == size
    bool empty() const {return (begin() == end());}
/*TODO emplace fnc, return a pair, first is a (string, iterator(to the value)) pair, 2nd is a bool if it already existed
also don't insert into const
keep inserting by substring into the child element container, if there's a node already then skip that insert
but keep inserting on that node

raise std::bad_optional_access if you try to insert an optional with no value

*/
    std::pair<iterator, bool> emplace(const key_type _key, const std::optional<mapped_type> _val){
      if (!(_val.has_value())) { throw std::bad_optional_access(); }
      auto it = make_branch(_key);
      if (it->value.second.has_value()) {
        return std::make_pair<iterator, bool>(it, false);
      }
      it->value.second = _val;
      return std::make_pair<iterator, bool>(it, true);
    }

    std::pair<iterator, bool> emplace(const optional_value_type pair){
      if (!(pair.second.has_value())) { throw std::bad_optional_access(); }
      auto it = make_branch(pair.first);
      if (it->value.second.has_value()) {
        return std::make_pair<iterator, bool>(it, false);
      }
      it->value.second = pair.second;
      return std::make_pair<iterator, bool>(it, true);
    }

    iterator make_branch(key_type _key) {
      trie_node* curr = &head;
      for (int i = 1; i <= _key.length(); ++i)
      {
        //TODO write custom find?
        auto it = curr->children.begin();
        while (it != curr->children.end() && it->value.first != _key.substr(0,i))
        {
          ++it;
        }
        if (it == curr->children.end()){ //TODO delete the new here
          it = curr->children.emplace(new trie_node(std::make_pair<optional_value_type>(_key.substr(0,i), std::nullopt), curr, children_container_type()));
        }
        curr = it;
      }
      return iterator(curr);
    }

    trie_node* traverse_branch(key_type _key) const{
      trie_node* curr = &head;
      for (int i = 1; i <= _key.length(); ++i)
      {
        auto it = curr->children.begin();
        while (it != curr->children.end() && it->value.first != _key.substr(0,i))
        {
          ++it;
        }
        if (it == curr.children.end()){
          return &head;
        }
        curr = it;
      }
      return curr;
    }



//TODO at FNC, throw out_of_range if its not there
    T& at(const key_type&){

        //if unreachable
        throw std::out_of_range();
    }
//TODO do we have to delete an element? maybe invalidate nodes which don't have a value? (aka optional values)
};

// Extra challenge: implement your own optional!
template <typename T>
using optional = std::optional<T>;

int stupid() {
  // Conventional trie: map strings to some T.
  // Try implement as much as the std::map interface as possible. Most of it
  // applies to our use case.

  // The internal representation of the nodes should be something like this:
  /*

g
├─ gs
│  ├─ gsd -> 42
w
├─ wh
│  ├─ whi
│  │  ├─ whis
│  │  │  ├─ whisp
│  │  │  │  ├─ whispy -> 69
x
├─ xa
│  ├─ xaz
│  │  ├─ xaza
│  │  │  ├─ xazax -> 1337

  */

  stupid_trie<int> STI;
  static_assert(std::is_same_v<decltype(STI)::key_type, std::string>);
  static_assert(std::is_same_v<decltype(STI)::mapped_type, int>);
  static_assert(std::is_same_v<decltype(STI)::value_type,
                               std::pair<const std::string, int>>);
  
  assert(STI.empty() && STI.size() == 0 && STI.count("whispy") == 0);
  //STI.count(static_cast<void*>(0)); // !!! Should not compile.
  /*
  const decltype(STI)& cSTI = STI;
  // Callable on const.
  assert(STI.empty() && cSTI.size() == 0 && cSTI.count("whispy") == 0);

  // Like std::map emplace.
  auto InsertGSD = STI.emplace("gsd", 42);
  auto InsertWhispy = STI.emplace("whispy", 69);
  auto InsertXazax = STI.emplace("xazax", 1337);

  assert(!cSTI.empty() && cSTI.size() == 3);
  assert(cSTI.count("gsd") == 1 && cSTI.count("whispy") == 1 &&
         cSTI.count("xazax") == 1);

  assert(InsertGSD.first->first == "gsd" && InsertGSD.first->second == 42 &&
         InsertGSD.second == true);
  assert(InsertWhispy.first->first == "whispy" &&
         InsertWhispy.first->second == 69 && InsertWhispy.second == true);
  assert(InsertXazax.first->first == "xazax" &&
         InsertXazax.first->second == 1337 && InsertXazax.second == true);

  auto InsertGSDAgain = STI.emplace("gsd", 43);
  // Insertion does not happen, gsd is already inserted, return iterator to
  // already existing element.
  assert(InsertGSDAgain.second == false && InsertGSDAgain.first->second == 42);

  cSTI.emplace("inserting into const should not happen", -1); // !!! Should not compile.

  try {
    STI.at("foo");
    assert(false && "Should have been unreachable.");
  } catch (std::out_of_range) {
  }

  try {
    cSTI.at("bar");
    assert(false && "Should have been unreachable.");
  } catch (std::out_of_range) {
  }

  // This is where we diverge from the std::map interface a little bit.
  // operator[] will not return a default constructed T like it does for map,
  // but instead an optional!
  auto MaybeElement = STI["gsd"];
  static_assert(std::is_same_v<decltype(MaybeElement), optional<int>>);

  // And because we return optional, operator[] is viable on const instances!
  const auto MaybeElementOnConst = cSTI["abel"];
  static_assert(std::is_same_v<decltype(MaybeElementOnConst),
                               const optional<int>>);

  assert(MaybeElement.has_value() && MaybeElement.value() == 42);
  assert(!MaybeElementOnConst.has_value() &&
         MaybeElementOnConst.value_or(-1) == -1);
  try {
    STI.emplace("This Element Does Not Exist", MaybeElementOnConst.value());
    assert(false && "Should have been unreachable.");
  } catch (std::bad_optional_access) {
  }

  assert(cSTI.count("This Element Does Not Exist") == 0);

  // The elements should be iterated in the natural order of the keys, in this
  // case, lexicographical.
  std::ostringstream OS;
  for (const decltype(STI)::value_type& Elem : STI) {
    OS << '(' << Elem.first << "->" << Elem.second << "),";
  }
  std::string Result = OS.str();
  Result.pop_back();
  std::string Expected = "(gsd->42),(whispy->69),(xazax->1337)";
  assert(Result == Expected);

  STI["gsd"] = 43;
  STI.emplace("abel", 16);

  // Now we add a new value. CAREFUL: "gs" is prefix of "gsd"! This should
  // definitely not break our internal representation!
  STI.emplace("gs", -24);

  OS.str("");
  for (const decltype(STI)::value_type& Elem : STI) {
    OS << '(' << Elem.first << "->" << Elem.second << "),";
  }
  Result = OS.str();
  Result.pop_back();
  Expected = "(abel->16),(gs->-24),(gsd->43),(whispy->69),(xazax->1337)";
  assert(Result == Expected);
  */
  return 1;
}

int stupid_noncopyable() {
  /*stupid_trie<int> CopyableTrie;
  CopyableTrie.emplace("foo", 1);
  CopyableTrie.emplace("bar", 2);
  std::ostringstream OS;
  for (const decltype(CopyableTrie)::value_type& Elem : CopyableTrie) {
    OS << '(' << Elem.first << "->" << Elem.second << "),";
  }
  std::string Result = OS.str();
  Result.pop_back();
  std::string Expected = "(bar->2),(foo->1)";
  assert(Result == Expected);

  decltype(CopyableTrie) Copy = CopyableTrie; // Int can be copied.
  Copy["foo"] = 8;
  OS.str("");
  for (const decltype(CopyableTrie)::value_type& Elem : CopyableTrie) {
    OS << '(' << Elem.first << "->" << Elem.second << "),";
  }
  Result = OS.str();
  Result.pop_back();
  Expected = "(bar->2),(foo->1)"; // Changing Copy doesn't change original obj.
  assert(Result == Expected);

  stupid_trie<std::unique_ptr<int>> NonCopyableTrie;
  NonCopyableTrie.emplace("int1", std::make_unique<int>(1234));

  decltype(NonCopyableTrie) Copy2 = NonCopyableTrie;
  // !!! Should not compile, unique_ptrs, and thus the nodes of it are not copyable!

  decltype(NonCopyableTrie) Moved = std::move(NonCopyableTrie); // Should work.

  OS.str("");
  for (const decltype(Moved)::value_type& Elem : Moved) {
    OS << '(' << Elem.first << "->" << *Elem.second << "),";
  }
  Result = OS.str();
  Result.pop_back();
  Expected = "(int1->1234)";
  assert(Result == Expected);
*/
  return 1;
}

int generic() {
  // Alright, let's go all in this time. The problem with the conventional trie
  // is that std::strings might be expensive to store. There is also no need to
  // store "g" and "gs" and "gsd", there should only be 'g', 's', and 'd' stored
  // in the nodes to get to the value of "gsd". This is what a **real** trie
  // does.
  //
  // To make this work, we will need a few type parameters.
  //  - The individual key piece type. In our case, it's char.
  //  - The mapped type. Could be anything, let's do int.
  //  - We will need to be able to concatenate the key pieces together.
  //  - We will need to be able to compare the key pieces.
  //  - We need a type that represents the **full** key to the user. This is
  //    string.
  //  - Do not forget, string types usually take two additional template
  //    parameters, the "char traits" and the allocator. We need to pass these
  //    too.
  //
  // The interface shall take ***TEMPLATE TEMPLATES*** for the last three type
  // parameters! (This is so that you look into what "template templates" are.)
  //
  // Other predicatesd and functors might also be needed, feel free to
  // experiment!
  /*
  const auto& CharToStringConcat = [](std::string& Seq, char C)
      -> std::string& {
    Seq.push_back(C);
    return Seq;
  };

  using default_template_parameters = trie<char, int,
                                           decltype(CharToStringConcat)>;
  static_assert(std::is_same_v<default_template_parameters::key_type,
                               std::basic_string<char>>);
  static_assert(std::is_same_v<default_template_parameters::value_type,
                               std::pair<const std::basic_string<char>, int>>);
  static_assert(std::is_same_v<default_template_parameters::key_compare,
                               std::less<char>>);
  static_assert(std::is_same_v<default_template_parameters::key_concat,
                               decltype(CharToStringConcat)>);
  static_assert(std::is_same_v<default_template_parameters::key_type
                                 ::traits_type,
                               std::char_traits<char>>);
  static_assert(std::is_same_v<default_template_parameters::key_type
                                 ::allocator_type,
                               std::allocator<char>>);

  using fully_specified = trie<char,
                               int,
                               decltype(CharToStringConcat),
                               std::less,
                               std::basic_string,
                               std::char_traits,
                               std::allocator>;
  static_assert(std::is_same_v<default_template_parameters, fully_specified>);

  */
  // The internal representation of the nodes should be something like this:
  /*

g
├─ s
│  ├─ d -> 42
w
├─ h
│  ├─ i
│  │  ├─ s
│  │  │  ├─ p
│  │  │  │  ├─ y -> 69
x
├─ a
│  ├─ z
│  │  ├─ a
│  │  │  ├─ x -> 1337

  */
  /*
  // The interface for the generic trie shall be roughly the same as the stupid
  // one's. It is only the representation that is different, emphasising cache
  // locality and smaller memory footprint.
  default_template_parameters GTI{CharToStringConcat};

  assert(GTI.empty() && GTI.size() == 0 && GTI.count("whispy") == 0);
  GTI.count(static_cast<void*>(0)); // !!! Should not compile.

  const decltype(GTI)& cGTI = GTI;
  // Callable on const.
  assert(GTI.empty() && cGTI.size() == 0 && cGTI.count("whispy") == 0);

  // Like std::map emplace.
  auto InsertGSD = GTI.emplace("gsd", 42);
  auto InsertWhispy = GTI.emplace("whispy", 69);
  auto InsertXazax = GTI.emplace("xazax", 1337);

  assert(!cGTI.empty() && cGTI.size() == 3);
  assert(cGTI.count("gsd") == 1 && cGTI.count("whispy") == 1
         && cGTI.count("xazax") == 1);

  assert(InsertGSD.first->first == "gsd" && InsertGSD.first->second == 42 &&
         InsertGSD.second == true);
  assert(InsertWhispy.first->first == "whispy" &&
         InsertWhispy.first->second == 69 && InsertWhispy.second == true);
  assert(InsertXazax.first->first == "xazax" &&
         InsertXazax.first->second == 1337 && InsertXazax.second == true);

  auto InsertGSDAgain = GTI.emplace("gsd", 43);
  // Insertion does not happen, gsd is already inserted, return iterator to
  // already existing element.
  assert(InsertGSDAgain.second == false && InsertGSDAgain.first->second == 42);

  cGTI.emplace("inserting into const should not happen", -1); // !!! Should not compile.

  try {
    GTI.at("foo");
    assert(false && "Should have been unreachable.");
  } catch (std::out_of_range) {
  }

  try {
    cGTI.at("bar");
    assert(false && "Should have been unreachable.");
  } catch (std::out_of_range) {
  }

  // This is where we diverge from the std::map interface a little bit.
  // operator[] will not return a default constructed T like it does for map,
  // but instead an optional!
  auto MaybeElement = GTI["gsd"];
  static_assert(std::is_same_v<decltype(MaybeElement), optional<int>>);

  // And because we return optional, operator[] is viable on const instances!
  const auto MaybeElementOnConst = cGTI["abel"];
  static_assert(std::is_same_v<decltype(MaybeElementOnConst),
                               const optional<int>>);

  assert(MaybeElement.has_value() && MaybeElement.value() == 42);
  assert(!MaybeElementOnConst.has_value() &&
         MaybeElementOnConst.value_or(-1) == -1);
  try {
    GTI.emplace("This Element Does Not Exist", MaybeElementOnConst.value());
    assert(false && "Should have been unreachable.");
  } catch (std::bad_optional_access) {
  }

  assert(cGTI.count("This Element Does Not Exist") == 0);

  // The elements should be iterated in the natural order of the keys, in this
  // case, lexicographical.
  std::ostringstream OS;
  for (const decltype(GTI)::value_type& Elem : GTI) {
    OS << '(' << Elem.first << "->" << Elem.second << "),";
  }
  std::string Result = OS.str();
  Result.pop_back();
  std::string Expected = "(gsd->42),(whispy->69),(xazax->1337)";
  assert(Result == Expected);

  GTI["gsd"] = 43;
  GTI.emplace("abel", 16);

  // Now we add a new value. CAREFUL: "gs" is prefix of "gsd"! This should
  // definitely not break our internal representation!
  GTI.emplace("gs", -24);
  // Be really really careful here: you'll have to ensure that the node for
  // "gs" (node g -> node s) exists properly, and place the value inside there.
  // Use optional<T> for your advantage!

  OS.str("");
  for (const decltype(GTI)::value_type& Elem : GTI) {
    OS << '(' << Elem.first << "->" << Elem.second << "),";
  }
  Result = OS.str();
  Result.pop_back();
  Expected = "(abel->16),(gs->-24),(gsd->43),(whispy->69),(xazax->1337)";
  assert(Result == Expected);
  */
  return 1;
}

/** Additional excercise
 *  --------------------

Think about Huffmann encoding and Huffmann trees. Huffmann trees store the
encoding key to the individual symbols based on the order of descent in the
binary tree. Going left is a '0', going right is a '1', and the tree is binary.

But the fact that getting to the key is based on the descent itself, it means
Huffmann trees are tries too!
However, storing the key as *string* would be wasteful: you could store bytes
only, and the full key is some sufficiently large number (unsigned long long,
or __uint128_t or larger if your compiler supports it). The "concatenation"
is shifting the bit into the key under work.

Use the above trie template (preferably **WITHOUT** an explicit specialisation!)
with <unsigned long long, bool, ...> to work as a Huffmann tree!

*/

int main() {
  int8_t grade = 1;
  if (stupid() && stupid_noncopyable())
    ++grade;
  if (generic())
    ++grade;
  return grade;
}
