!==============================================================
      program &


            & congruent
            IMPORT, NONE
        !==============================================================
        ! example of linear congruential generator
        ! X_(k+1) = mod(a*X_k + c,m) -> x_k = X_k)/m 
        ! M. D'Elia - 09/2018
        !==============================================================
        
        ! declaration of variable types
        
              implicit none
              integer(16) m,a,c,xk,xkp1,xtemp,seed ! we require quite large integers for the generator
              integer iloop
              real*8 x,y      
        
        
        
        ! definition of the "modulus"
              m = 10**8 + 1  ! original Lehmer implementation 1949 working on ENIAC 
                              ! which was indeed a 8-decimal digit number machine
        !      m = 2147483647 ! 2^31 - 1  ! Park-Miller 1988, this is a Mersenne prime
          
        ! definition of the "multiplier"
              a = 23     ! original Lehmer implementation 1949 workin on ENIAC 
        !      a = 16807  ! Park-Miller 1988
        !      a = 48271  ! Park-Miller 1993
        
        ! definition of the "increment"
        ! when c = 0 the generator is called "multiplicative congruential generator"
              c = 0       ! Both Lehmer and Park-Miller implementations
        
        
        
              seed = 2      ! define the starting values of pseudo-random sequence
        
        
              xk = seed      
        
20            do iloop = 1,10   ! change as you want
        
                xtemp = xk*a + c            ! the generator in three lines: linear transformation
                xkp1  = xtemp - m*(xtemp/m) ! this is the mod(xtemp,m) operation. Notice that going
                xk    = xkp1                ! through xkp1 is useless, could save one line, 
                                            ! just for the sake of clarity .. 
                x = float(xk)/float(m)      ! x in [0,1), the actual random number  
        
                xtemp = xk*a + c            ! we repeat twice to draw a pair of consecutive 
                xkp1  = xtemp - m*(xtemp/m) ! random numbers in the sequence
                xk    = xkp1                ! 
                                            ! 
                y = float(xk)/float(m)      ! 
        
                print *, x,y             ! try to plot the pairs [0,1]x[0,1] to see if they
                                            ! follow some regular structure. Lehmer implementation
                                            ! is ugly, Park-Miller looks much better
        !      if (x.lt.0.001.and.y.lt.0.001) write (*,*) x,y  ! but if you try to zoom
                                                               ! also Park-Miller shows regular  
                                                               ! structures, even if at much smaller
                                                               ! scales
        
              enddo   ! close the loop on the generation of random pairs

              print & ! hello

      ! hello

               *, 'DONE'
        
                  
              stop
              end