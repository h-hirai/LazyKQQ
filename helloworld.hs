{-# LANGUAGE TemplateHaskell,QuasiQuotes #-}

import LazyKQQ

program =
    [lazyK|
     `k``s``si`k``s`k```sii``s``s`kski``s``s`ksk``s``s`ksk```s``siii``s``s`k
     ski`k``s``si`k``s``s`ksk```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s
     ``s`kski`k``s``si`k``s`k```sii``s``s`kski```sii``s``s`ksk``s``s`kski`k`
     `s``si`k``s`k```sii``s``s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k``s
     ``s`ksk``s`k``s``s`kski``s``s`ksk``s`k``s``s`kski```sii``s``s`ksk``s``s
     `kski`k``s``si`k````s``s`ksk```s``siii``s``s`kski`s``s`ksk```sii``s``s`
     ksk``s``s`kski`k``s``si`k``s`k``s``s`kski```s``siii``s``s`kski`k``s``si
     `k``s`k``s``s`ksk``s`k``s``s`kski``s``s`ksk``s``s`kski``s``s`ksk```s``s
     iii``s``s`kski`k``s``si`k``s``s`ksk``s`k``s``s`kski``s``s`ksk``s`k``s``
     s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k``s`k``s``s`kski``s``s`ksk`
     `s`k``s``s`kski``s``s`ksk```sii``s``s`ksk``s``s`kski`k``s``si`k``s`k```
     sii``s``s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k```s``s`kski``s`k``
     s``s`kski``s``s`ksk```sii``s``s`kski`k``s``si`k``s``s`ksk``s`k``s``s`ks
     ki```s``siii``s``s`kski`k``s``si`k``s`k``s``s`kski``s``s`ksk```sii``s``
     s`kski`k``s``si`k```sii```sii``s``s`kski`k```sii```sii``s``s`kski
     |]
main =
    do input <- getContents
       output $ eval $ program :$ (encode input)
