        interface
          subroutine rstran(interp,resu,motcle,iocc,kdisc,krang,nbdisc&
     &,ier)
            character(*) :: interp
            character(len=19) :: resu
            character(*) :: motcle
            integer :: iocc
            character(len=19) :: kdisc
            character(len=19) :: krang
            integer :: nbdisc
            integer :: ier
          end subroutine rstran
        end interface
