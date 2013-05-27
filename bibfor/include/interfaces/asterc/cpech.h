        interface
          subroutine cpech(icompo,cpiter,tf,numpas,nomvar,idim,nompal,&
     &info)
            integer :: icompo
            integer(kind=4) :: cpiter
            real(kind=4) :: tf
            integer(kind=4) :: numpas
            character(len=144) :: nomvar
            integer(kind=4) :: idim
            character(len=8) :: nompal
            integer(kind=4) :: info
          end subroutine cpech
        end interface
