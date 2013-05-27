        interface
          subroutine spdfft(nsens,nomfon,nbvin,nomfs,nbvout,method,sym&
     &,base)
            integer :: nsens
            character(len=19) :: nomfon
            integer :: nbvin
            character(len=19) :: nomfs
            integer :: nbvout
            character(len=16) :: method
            character(len=16) :: sym
            character(len=1) :: base
          end subroutine spdfft
        end interface
