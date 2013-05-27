        interface
          subroutine cpedb(icompo,cpiter,tf,numpas,nomvar,idim,paramr,&
     &info)
            integer :: icompo
            integer(kind=4) :: cpiter
            real(kind=8) :: tf
            integer(kind=4) :: numpas
            character(len=144) :: nomvar
            integer(kind=4) :: idim
            real(kind=8) :: paramr(20)
            integer(kind=4) :: info
          end subroutine cpedb
        end interface
