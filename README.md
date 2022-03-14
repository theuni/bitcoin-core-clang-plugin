This is intended to be a plugin that enforces the following clang AST query:
```
functionDecl(
  hasDescendant(
    callExpr(
      callee(
        functionDecl(
          returns(
            hasDeclaration(
              classTemplateSpecializationDecl(
                hasName("early_exit_t")))))))),
  unless(
    returns(
      hasDeclaration(
        classTemplateSpecializationDecl(
          hasName("early_exit_t")))))
)
```

Example usage:

`$ bin/clang++ -fsyntax-only -fplugin=libbitcoin-core-clang.so ../example.cc`

```
../example.cc:23:6: warning: Function 'caller2' calls into a function that returns early_exit_t but does not itself return early_exit_t
void caller2() // should warn for not returning early_exit_t.
     ^
../example.h:11:6: note: 'caller2' declared here
void caller2();
     ^
../example.cc:25:33: note: early_exit_t returned here
    auto foo = maybe_early_exit();
                                ^
../example.cc:26:33: note: early_exit_t returned here
    auto bar = maybe_early_exit();
                                ^
../example.cc:30:6: warning: Function 'caller3' calls into a function that returns early_exit_t but does not itself return early_exit_t
void caller3() // should warn for not returning early_exit_t.
     ^
../example.cc:32:33: note: early_exit_t returned here
    auto foo = maybe_early_exit();
                                ^
../example.cc:33:33: note: early_exit_t returned here
    auto bar = maybe_early_exit();
                                ^
2 warnings generated.
```
