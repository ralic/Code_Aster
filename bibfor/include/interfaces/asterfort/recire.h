        interface
          subroutine recire(typopt,iderre,frexci,fremin,fremax,pas,&
     &nbptmd)
            character(len=4) :: typopt
            integer :: iderre
            character(len=4) :: frexci
            real(kind=8) :: fremin
            real(kind=8) :: fremax
            real(kind=8) :: pas
            integer :: nbptmd
          end subroutine recire
        end interface
