# morley-sop (beta)

## Introduction

`morley-sop` provides a type of extended entrypoint (`EpPath`) that supports
arbitrarily nested `or`'s and `pair`'s. This allows an extended entrypoint
with a field reference to point to exactly one of the non-`or`/`pair`
constituents of the type.

This allows us to normalize arbitrary Michelson types:

- The extended entrypoints themselves normalize the type up to distribution of `pair` over `or`
- Collecting all of the extended entrypoints normalizes the type up to associativity of `or/`pair`
- Sorting the resulting list of lists normalizes the type up to commutativity of `or/`pair`

The resulting normalized type inherits the annotations of the original type
and may be used as a sort of Michelson "first class record type".

Known limitations:
- The current unicification method is over-eager and the resulting annotations often have the `_[number]` suffix
- The current method of pretty-printing `EpPath`'s is verbose
- No Michelson code to convert between types is generated

This library provides a CLI tool that allows specifying an extended entrypoint to
craft a value of that type using human-readable arguments.

Key modules:
- `Michelson.Typed.T.Alg`: isomorphism between `T` and `TAlg`: a version of `T` where all but `or`/`pair` is opaque
- `Michelson.Typed.EntryPoints.Sing.Alg.Paths`: functions to enumerate all `EpPath`'s and uniqify annotations so that all paths and fields will be unique
- `Michelson.Typed.EntryPoints.Sing.Alg.Types`: functions to resolve individual fields of an extended entrypoint
- `Michelson.Typed.EntryPoints.Sing.Alg.Lens`: a lens to access a field using an extended entrypoint


## Building

To build the executable, run `stack build`
and then either run `stack install` to copy the executable to your `$PATH`
or prefix the following commands with `stack exec --`,
e.g. `stack exec -- morley-sop ..`

Haskell [stack](https://docs.haskellstack.org/en/stable/README/) may be found
[here](https://docs.haskellstack.org/en/stable/README/).

### Documentation

Haddock-generated documentation is available at [docs/index.html](docs/index.html).

To rebuild the documentation, run: `stack haddock` and copy the generated `doc`
directory to `docs`.

## Examples

Example commands:

```bash
$ morley-sop "$(cat contracts/fa2ext_stub.tz | tr -d '\n')" --help

Available commands:

Usage: morley-sop %_2%balance_of%(*) --callback (contract (list (pair nat (pair address nat))))
                                     --requests (list (pair address nat))

Usage: morley-sop %_2%is_operator%(***(All_tokens%)) --All_tokens unit
                                                     --callback_1 (contract (pair bool (pair address (pair address (or unit (set nat))))))
                                                     --operator_1 address
                                                     --owner address

Usage: morley-sop %_2%is_operator%(***(Some_tokens%)) --Some_tokens (set nat)
                                                      --callback_1 (contract (pair bool (pair address (pair address (or unit (set nat))))))
                                                      --operator_1 address
                                                      --owner address

Usage: morley-sop %_3%_5%permissions_descriptor% --permissions_descriptor (contract (pair (pair (option (pair (option address) string)) (or unit unit)) (pair (or unit (or unit unit)) (pair (or unit unit) (or unit (or unit unit))))))

Usage: morley-sop %_3%_5%set_administrator% --set_administrator address

Usage: morley-sop %_3%mint%(***) --address address --amount nat --symbol string
                                 --token_id nat

Usage: morley-sop _1%_8%set_pause% --set_pause bool

Usage: morley-sop _1%_8%token_metadata%(*) --callback_2 (contract (list (pair nat (pair string (pair string (pair nat (map string string)))))))
                                           --token_ids (list nat)

Usage: morley-sop _1%_9%_10%_11%single_transfer%(***) --amount_1 nat
                                                      --from_ address
                                                      --to_ address
                                                      --token_id_1 nat

Usage: morley-sop _1%_9%_10%_11%transfer% --transfer (list (pair address (pair address (pair nat nat))))

Usage: morley-sop _1%_9%_10%_12%_15%Add_operator%(**(All_tokens_1%)) --All_tokens_1 unit
                                                                     --operator_2 address
                                                                     --owner_1 address

Usage: morley-sop _1%_9%_10%_12%_15%Add_operator%(**(Some_tokens_1%)) --Some_tokens_1 (set nat)
                                                                      --operator_2 address
                                                                      --owner_1 address

Usage: morley-sop _1%_9%_10%_12%_15%Remove_operator%(**(All_tokens_2%)) --All_tokens_2 unit
                                                                        --operator_3 address
                                                                        --owner_2 address

Usage: morley-sop _1%_9%_10%_12%_15%Remove_operator%(**(Some_tokens_2%)) --Some_tokens_2 (set nat)
                                                                         --operator_3 address
                                                                         --owner_2 address

Usage: morley-sop _1%_9%_10%_12%update_operators% --update_operators (list (or (pair address (pair address (or unit (set nat)))) (pair address (pair address (or unit (set nat))))))

Usage: morley-sop _1%_9%total_supply%(*) --callback_3 (contract (list (pair nat nat)))
                                         --token_ids_1 (list nat)
```

```bash
$ morley-sop "$(cat contracts/fa2ext_stub.tz | tr -d '\n')" \
  '_1%_9%_10%_11%single_transfer%(***)' --help

Usage: morley-sop _1%_9%_10%_11%single_transfer%(***) --amount_1 nat
                                                      --from_ address
                                                      --to_ address
                                                      --token_id_1 nat

Available options:
  -h,--help                Show this help text
```

```bash
$ stack build --fast && stack exec -- morley-sop \
  "$(cat contracts/fa2ext_stub.tz | tr -d '\n')" \
  '_1%_9%_10%_11%single_transfer%(***)' \
  --amount_1 42 --from_ "\"$ALICE_ADDRESS\"" --to_ "\"$BOB_ADDRESS\"" \
  --token_id_1 0

Right (Right (Right (Left (Right (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" (Pair 0 42)))))))
```

