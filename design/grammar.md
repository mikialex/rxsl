expression

```
EXP
=> EXP * EXP
=> EXP / EXP
=> EXP + EXP
=> EXP - EXP
=> EXP % EXP
=> EXP POSTFIX
=> - EXP
=> ! EXP
=> ( EXP )
=> EXP.ident..
=> EXP [EXP]..
=> number
=> bool
=> ident
=> FUNCTION_CALL

FUNCTION_CALL
=> ident( EXP.. )

============
after disambiguity and remove left recursion
============

EXP
=> EXP_NO_ADDICTIVE ADDICTIVE_OP? EXP_NO_ADDICTIVE?

ADDICTIVE_OP
 => + -

EXP_NO_ADDICTIVE
=> EXP_WITH_POSTFIX MULTIPLICATIVE_OP? EXP_WITH_POSTFIX?

MULTIPLICATIVE_OP
=> * / %

EXP_WITH_POSTFIX
=> EXP_SINGLE POSTFIX..?
=> EXP_SINGLE

POSTFIX
=> [EXP_SINGLE]
=> .ident

EXP_SINGLE
=> ( EXP )
=> - EXP
=> ! EXP
=> number
=> bool
```

statement & block

```
BlOCK
=> { STATEMENT? }


STATEMENT
=> return EXP? ;
=> IF;

IF
=> if EXP BLOCK ELSEIF? ELSE? ;?

ELSEIF
=> elseif BlOCK

ELSE
=> else BLOCK


```