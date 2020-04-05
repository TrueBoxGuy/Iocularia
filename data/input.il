id x = x;
const x y = x;
zero f x = x;
succ n f x = f ((n f) x);
pred n f x = ((n (predF f)) (const x)) id;
predF f g h = h (g f);
add n m = (m succ) n;
sub n m = (m pred) n;
mult n m = (n (add m)) zero;
isZero n a b = (n (const b)) a;

fact n = ((isZero n) (succ zero)) ((mult n) (fact (pred n)));

main = fact (succ (succ (succ (succ (succ zero)))));
