The paper "A Modest Proposal: C++ Resyntaxed" tries to derive a new LALR(1) syntax for C++ http://www.csse.monash.edu.au/~damian/papers/PS/ModestProposal.ps, it contains a interesting propose that solve a lot of C++ problems and contains the following example:

    type Stack<[type T]> : class;
    type Cell<[type T]> : class 
    {
        [friend] type Stack<[T]> : class;
        [private] obj next : ^Cell; obj rep : ^T;
        func ctor : (r:^T, c:^Cell<[T]>) initially rep(r), next(c)
        {
        }
    }

    type Stack<[type T]> : class
    {
        [public] func pop : (void -> ^T);
        func top : (void -> ^T) { return rep^.rep; }
        func push : (v:^T -> void) { rep := new Cell<[T]>(v,rep); }
        func empty : (void -> int) { return rep=0; }
        func ctor : (void)
        {
            rep := 0;
        }
        [private] obj rep : ^Cell<[T]>;
    }
    
    func Stack<[type T]>::pop : (void -> ^T)
    {
        obj ret(rep^.rep) : ^T;
         obj c(rep) : ^Cell<[T]>;
        rep := rep^.next;
        delete c;
        return ret;
    }

    func sort<[type S]> : ((elements: [] S, nelements: const int) -> void)
    {
        obj flip:=0, sz:=nelements-1 : int;
        do
        {
            for (obj j:=0, flip:=0 : int; j<sz; j++)
            {
                if (elements[j] < elements[j+1])
                {
                    obj t:=elements[j+1] : S;
                    elements[j+1] := elements[j];
                    elements[j] := t flip++;
                }
            }
        } while (flip);
    }

Maybe we can improve this example a little with some modern touchs. Let us analyze it line by line:

    SPECS
    type Stack<[type T]> : class;

    C++
    template <typename T> class Stack<T>;

I like that SPECS proposes a more concise way of defining templates, but to be honest I do not like the <[...]> approach.

    PROPOSE
    type Stack;

Next the paper defines the Cell class:

    SPECS
    type Cell<[type T]> : class 
    {
        [friend] type Stack<[T]> : class;
        [private] obj next : ^Cell; obj rep : ^T;
        func ctor : (r:^T, c:^Cell<[T]>) initially rep(r), next(c)
        {
        }
    }

    PROPOSE
    type Cell =  T ->
    {
        func ctor : r:^T -> c:^Cell<[T]> -> auto
        {
            val rep : ^T = c;
            val next : ^Cell = c;
        }
        friend Stack<T>;
    }

Given that the type of the constructor parameters are the same as the fields we can let the transpiler do its job:

    PROPOSE
    type Cell = T ->
    {
        func ctor : r -> c -> auto
        {
            val rep : ^T = c;
            val next : ^Cell = c;
        }
        friend Stack<T>;
    }

It can be strange to declare the fields inside the constructor, but it is a way to decrease the quantity of boilerplate code. If you hae multiple constructor, the class will have the union of all fields.

A simpler DTO (in C++, all public struct could be written like):

    PROPOSE
    type Cell = T -> { val rep : ^T, next : ^Cell; }

Continuing we have the actual Stack type:

    PROPOSE
    type Stack : T ->
    {
        public func ctor : void -> auto
        {
            val cell : ^Cell<[T]> = null;
        }

        public func pop : void -> ^T = pop;
        public func top : void -> ^T { return cell.rep; }
        public func push : v -> void { cell = Cell(T)(v,cell); }
        public func isEmpty : void -> bool { return cell == null; }
    }

    PROPOSE
    func pop : (Stack(T) -> ^T)
    {
        result = {rep.rep};
        val c : ^Cell(T) = {rep};
        result = result.next;
        delete c;        
    }

The idea is that {...} defines an object, always. //TODO How interpret function and block using {}. So, if the compiler knows what type is expected it will call the appropriated object constructor.

If you need to call placement new use the "@". Sending the obect to the "default" value will call the new operator without placement;

    PROPOSE
    val c1 : ^Cell(T) = {rep} @ default;
    val c2 : ^Cell(T) = {rep} @ memadr;

Method declarion on a object is a mixin of a function. In this case, method calling became an option, for example:

    PROPOSES
    val top = pop stack;

or

    PROPOSES
    val top = stack pop;

The dot is not necessary in this case and we gain method call as a prefix or a posfix operation.