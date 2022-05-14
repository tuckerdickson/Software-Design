-- append lists l1 and l2.  The empty list is
-- represented as null, and cons'ing h onto t
-- is represented by the pair (h,t).
app(l1,l2) {
  if null?(l1) {
    return l2;
  } else {
    h = l1.1;
    l1 = l1.2;
    x = app(l1,l2);
    return (h,x);
  }
}

-- naive list reverse
rev(l1) {
    if null?(l1) {
        return l1;
    } else {
        h = l1.1;
        t = l1.2;
        x = rev(t);
        y = app(x,h);
        return y;
    }
}

!

l1 = (3,null);
l1 = (2,l1);
l1 = (1,l1);

l2 = (1,null);
l2 = (2,l2);
l2 = (3,l2);

!

rev(l1) = l2