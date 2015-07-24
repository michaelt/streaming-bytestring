# bytestring-streaming

              copy 200M file    divide it on lines, 
                                adding '!' to each 
                                
    lazy      0m0.813s          0m8.597s
    streaming 0m0.783s          0m9.664s
    pipes     0m0.771s          0m49.176s
    conduit	  0m1.068s          2m25.437s

