{-# LANGUAGE TemplateHaskell,QuasiQuotes #-}

import LazyKQQ
import Control.Applicative ((<$>))

program =
    [lazyK|
````sii``s``s`ks``s`k`s`ks``s``s`ks``s`kk``s`ks``s`k`s`k``si`kk``s`k`s``s``si`
kk`k``si`k`ki``s`kk``s``s`k``s``s`ks``s`kk``s`k``s``s`ksk``s`k``s``s`kski```s`
`siii``s``s`kski``s``s`ks``s`kk``s`ks``s`k`sik`kk``s`k`s``s`k``s``s`kski``s``s
`ks``s`kk``s`ks``s`k`sik`kk``s``s`ks``s`kk``s`ks``s`k`si``s`kk``s`k`s`k``sii``
s``s`ks``s`k`s`ks``s`k`s`kk``s`k`s`ks`s`k`s``s``s``s``si`kk`k``si`k`ki`k````s`
`s`kski```s``s`ksk```sii``s``s`kski``s`k`s``si`k`kik``s``si`kk`k```sii``s`k``s
`k`s``si`k`kik``sii`kk`k`k``s``s`ks``s`kk``sii`k``si`k`ki``s`k`s`kk``s`k`s``s`
k``s``s`kski``s`k``s``s`ksk```sii``s``s`kski``s``s`ks``s`kk``s`ks``s`k`sik`kk`
`s`k`s`k``s`k`s``si`k``s``s`ks``s``s`ksk`k``s`k`si``s`kk``s`k`s``s`ksk``s`kk``
s`ks``s`kk``s`ks```ss`s``s`ks``s`kk``s`ks``s`k`si```ss`si`kk`kk`k``si`k`kik``s
`k`s``s`k```s``siii``s``s`kski``s``s`ks``s`kk``s`ks``s`k`sik`kk``s`k`s`k``s`k`
s``si`k``s``s`ks``s``s`ksk`k``s`k`si``s`kk``s`k`s``s`ksk``s`kk``s``s`ks``s`k`s
`ks``s`k`s`k`s`ks``s``s`ks``s`kk``s`ks``s`k`s`ks``s`k`s``s`ksk``s`kk``s`k`s`k`
`s`k`sik``s`k`s`k``si`kk``s`k`s``s``si`kk`k``si`k`ki``s`kk``s``s``si`kk`k``s`k
`s``si`kik`k``s``si`k```sii``s`k`s``s`ksk``s`kk``s`k`s`kk``s`k`si``s`kk``sii`k
```sii``s`k``s`k`s``si`kik``sii`k``s`kkk`k`k`ki`k``si`k`kik``s`k`s`k``s`k`s``s
i`k``si`k``si`k``s``s`ksk`k``s``s`ks``s`k`s`ks``s`k`s``s`ks``s``s`ksk`k``s`k`s
i``s`kk``s`k``si`kk``s``s``s``si`k`ki`kk`k``si`k`ki`k`````sii```sii``s``s`kski
``s`k`s``si`kik`k```sii``s`k`s``s`ksk``s`kk``s`k`s`kk``s`k`si``s`kk``sii``s`kk
k`k`k``si`k`kik``s`k`s`k``si`k`ki``s`k`s``s`k``s``s`kski``s`k```s``siii``s``s`
kski``s``s`ks``s`kk``s`ks``s`k`sik`kk``s``s`ks``s`kk``s`ks``s`k`si``s`kk``s``s
`ksk``s``s`ks``s`kk``s`ksk`k``s``s`ks``s`kk``s`ksk`k``s``s`ks``s`kk``s`ksk`k``
s``s`ks``s`kk``s`ks``s`k`sik`kk`k``s`kk``s``s`k``s``s`kski``s``s`ks``s`kk``s`k
s``s`k`sik`kk``s`k`s``si`k``si`k``si`k``s``s`ksk`k``s``s`ks``s`k`si``s`kk``s`k
`si``s`kk``s`k`s`kk``s`k`sikk``s`kk``s`k`s``si`k``si`k``si`k``s`k`si``s`kk``s`
k`s``s`ksk``s`kk``s``s`ks``s`kk``s`ksk`k``s`k`s``s`ks``s`k`si``s`kk``s`k`sik``
s`kkk``s`kk``s`k`s``si`k``si`k``si`k``s`kk``si`k`k`k`k```sii```sii``s``s`kski`
`s`kk``s``s`k``s``s`ksk``s``s`kski``s``s`ks``s`kk``s`ks``s`k`sik`kk``s`k`s``si
`k``si`k``si`ki``s`kk``s``s`ks``s`k`sik``s`kk``s`k`s``si`k``si`k``si`k``s``s`k
sk`k``s``s`ksk`k``s`k`s``s`ksk``s`kk``s`k`s`kk``s`k`sik``s`kk``s``s`k``s``s`ks
ki``s`k``s``s`ksk``s``s`kski``s``s`ks``s`kk``s`ks``s`k`sik`kk``s`k`s``si`k``si
`k``si`k``s``s`ksk`k`s`k`s`k``s`k`s``si`k``s`k``s``s`kski``s``s`ksk```sii``s``
s`kskik``s`kk``s`k`s``si`k``si`k``si`k``s``s`ksk`k``s``s`ksk`k``s`k`s``s`ksk``
s`kk``s`k`s``s`ksk``s`kk``s`k`s`k`s``s`ksk``s`k`s`kk``s``s`ks``s`kk``s`ks``s`k
k``s`ks``s``s`ksk`k``s`k`sik`k``s``s`ks``s`kk``s`ks``s`k`s`ks``s`k`s`k`si``s`k
`s`kk``s``s`ksk`k``s`k`sik`k``s`kkk``s`kk``s``s`k``s``s`kski``s``s`ks``s`kk``s
`ks``s`k`sik`kk``s`k`s``si`k``si`k``si`k```sii``s`k`s``s`ksk``s`kk``s`k`s`kk``
s`k`si``s`kk``sii``s`kk``s``s`k``s``s`ksk```sii``s``s`kski``s``s`ks``s`kk``s`k
s``s`k`sik`kk``s`k`s``si`k``si`k``si`k``s``s`ksk`k``s``s`ks``s`k`s`ks``s`k`s``
s`ks``s``s`ksk`k``s`k`si``s`kk``s``s``s`k``si`kk``s``s``si`kk`k``si`k`ki`k````
`sii```sii``s``s`kski``s`k`s``si`kkk`k`ki``s`k`s``s`ksk``s`kk``s`ks``s`kk``s`k
s```ss`s``s`ks``s`kk``s`ks``s`k`si```ss`si`kk`kk`k```sii``s`k`s``s`ksk``s`kk``
s`k`s`kk``s`k`si``s`kk``sii``s`kkk`k`ki``s`kk```ss`s``s`ks```ss`s``s`ks``s`kk`
`s`ks``s`k`sik`kk`k``sii``sii`k`k``s`k`s``si`k``s``s`ksk``s`k``s``s`kski`````s
ii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kskik`kk`k`k``si`k`ki``s``s`ks``s
`kk``si`k`k`k`k```sii```sii``s``s`kski`k``s`k`s``si`k```sii```sii``s``s`kskik
    |]
main = output . eval =<< (program :$) . encode <$> getContents