-- follow a path p in a tree t
--
-- A leaf in the tree is represented as null.
-- A node is represented as a nested pair (d,(l,r)),
-- where d is the data at the node, and l and r are the
-- left and right subtrees.
--
-- A path is a list, where null represents the empty list,
-- and (h,t) represents cons'ing head h onto tail list t.

path (t,p) {
  if null?(t) {
    return null;
  } else {
    if null?(p) {
      return t.1;
    }
    else {
      if p.1 {
        x = t.2.1;
        y = p.2;
        return path(x,y);
      } else {
        x = t.2.2;
        y = p.2;
        return path(x,y);
      }
    }
  }
}

!

-- create a tree q that has 1 at the root (q), and then
-- descends to singleton nodes (x and y) holding 2 and 3
-- respectively.
e = (null,null);
x = (2,e);
y = (3,e);
z = (x,y);
q = (1,z);

b = (1,null);

! 

path(q,b) = 2