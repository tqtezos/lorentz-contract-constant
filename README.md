
See the FA1.2 [Quick Start Tutorial](https://assets.tqtezos.com/quickstart) for more detail.

# CLI Interface

If you want to specify the type of your constant, you can use the `print` command:

```bash
❯❯❯ ./stack exec -- lorentz-contract-constant Constant print --help
Usage: lorentz-contract-constant Constant print --constant-valueType Michelson Type
                                                --constant-value Michelson Value
                                                [-o|--output FILEPATH]
                                                [--oneline]
  Dump the Constant contract in form of Michelson code

Available options:
  -h,--help                Show this help text
  --constant-valueType Michelson Type
                           The Michelson Type of constant-value
  --constant-value Michelson Value
                           The Michelson Value: constant-value
  -o,--output FILEPATH     File to use as output. If not specified, stdout is
                           used.
  --oneline                Force single line output
  -h,--help                Show this help text
```

Otherwise, if you just want to use strings, use the `print-metadata` command:

```bash
❯❯❯ ./stack exec -- lorentz-contract-constant Constant print-metadata --help
Usage: lorentz-contract-constant Constant print-metadata --metadata [STRING]
                                                         [-o|--output FILEPATH]
                                                         [--oneline]
  Dump the Constant contract, specialized to a tuple of the given MText's, in
  form of Michelson code

Available options:
  -h,--help                Show this help text
  --metadata [STRING]      List of strings representing metadata values to store
  -o,--output FILEPATH     File to use as output. If not specified, stdout is
                           used.
  --oneline                Force single line output
  -h,--help                Show this help text
```

For example, if our metadata was just the string `"hello world"`:

```bash
❯❯❯ ./stack exec -- lorentz-contract-constant Constant print-metadata --metadata "[\"hello world\"]" --oneline
parameter (pair unit (contract string));storage unit;code { CAR;CDR;PUSH mutez 0;PUSH string "hello world";TRANSFER_TOKENS;DIP { UNIT;NIL operation };CONS;PAIR };
```

And if our metadata is both the strings `"hello world"` and `"other stuff"`:

```bash
❯❯❯ ./stack exec -- lorentz-contract-constant Constant print-metadata --metadata "[\"hello world\",\"other stuff\"]" --oneline
parameter (pair unit (contract (pair string string)));storage unit;code { CAR;CDR;PUSH mutez 0;PUSH (pair string string) (Pair "hello world" "other stuff");TRANSFER_TOKENS;DIP { UNIT;NIL operation };CONS;PAIR };
```

We can then originate the contract:

```bash
❯❯❯ alpha-client --wait none originate contract MetadataPair \
  transferring 0 from $ALICE_ADDRESS running \
  "$(./stack exec -- lorentz-contract-constant Constant \
  print-metadata --metadata \
  "[\"hello world\",\"other stuff\"]" --oneline)" \
  --burn-cap 0.368

Waiting for the node to be bootstrapped before injection...
Current head: BM5s1LBtpDvA (timestamp: 2020-01-27T20:02:14-00:00, validation: 2020-01-27T20:02:23-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 12652 units (will add 100 for safety)
Estimated storage: 368 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opJLbU3wjhPECw7jgkC1m9ijWRA2MYnNnn8QPr3R4BCKjCyqYuh'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opJLbU3wjhPECw7jgkC1m9ijWRA2MYnNnn8QPr3R4BCKjCyqYuh to be included --confirmations 30 --branch BM5s1LBtpDvAY1YoZtFs7fuqgYWyxHWE98ckNZPFpYwLkcUpnZ6
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.001616
    Expected counter: 111802
    Gas limit: 12752
    Storage limit: 388 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............. -ꜩ0.001616
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,118) ... +ꜩ0.001616
    Origination:
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      Credit: ꜩ0
      Script:
        { parameter (pair unit (contract (pair string string))) ;
          storage unit ;
          code { CAR ;
                 CDR ;
                 PUSH mutez 0 ;
                 PUSH (pair string string) (Pair "hello world" "other stuff") ;
                 TRANSFER_TOKENS ;
                 DIP { UNIT ; NIL operation } ;
                 CONS ;
                 PAIR } }
        Initial storage: Unit
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1VKobohN2grLtAGNhVWgFitPiwLgPLDmub
        Storage size: 111 bytes
        Paid storage size diff: 111 bytes
        Consumed gas: 12652
        Balance updates:
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.111
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.257

New contract KT1VKobohN2grLtAGNhVWgFitPiwLgPLDmub originated.
Contract memorized as MetadataPair.
```

