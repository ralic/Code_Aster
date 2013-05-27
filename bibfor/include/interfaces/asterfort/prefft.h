        interface
          subroutine prefft(resin,method,symetr,nsens,grand,vectot,&
     &nbva,ier)
            character(len=19) :: resin
            character(len=16) :: method
            character(len=16) :: symetr
            integer :: nsens
            character(len=4) :: grand
            character(len=19) :: vectot
            integer :: nbva
            integer :: ier
          end subroutine prefft
        end interface
