module Samples exposing (..)

import TestUtils exposing (..)
import RedBlackZipper exposing (Zipper, zipper, Color(..))

smallZip : Zipper Int String
smallZip = ( zipper (n Black 11
                       (n Red 2
                          (t Black 1)
                          (n Black 7
                             (t Red 5)
                             (t Red 8)
                          )
                       )
                       (n Black 14
                          l
                          (t Red 15)
                       )
                    )
           )              

mediumZip : Zipper Int String
mediumZip = ( zipper (n Black 12
                        (n Black 5
                           (n Black 3
                              l
                              (t Red 4)
                           )
                           (n Red 10
                              (n Black 7
                                 (t Red 6)
                                 (t Red 8)
                              )
                              (t Black 11)
                           )
                        )
                        (n Black 15
                           (n Black 13
                              l
                              (t Red 14)
                           )
                           (t Black 17)
                        )
                     )
            )
