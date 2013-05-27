        interface
          subroutine cpeen(icompo,cpiter,tf,numpas,nomvar,idim,parami,&
     &info)
            integer :: icompo
            integer(kind=4) :: cpiter
            real(kind=4) :: tf
            integer(kind=4) :: numpas
            character(len=144) :: nomvar
            integer(kind=4) :: idim
            integer(kind=4) :: parami(20)
            integer(kind=4) :: info
          end subroutine cpeen
        end interface
