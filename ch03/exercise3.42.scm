;; 安全，不会有任何不同，原来的单个账户里面的withdraw和deposit可以并发执行，现在不行，现在只能是账户并发执行，里面操作是不允许并发。

;;It's safe to do that change. There is nothing different about concurrency in these two version. The only difference is new one serialize the procedures before call the functions, but the original one do it when call withdraw or deposit.
