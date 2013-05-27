        interface
          subroutine cplch(icompo,cpiter,ti,tf,numpas,nomvar,idim,&
     &taille,nompal,info)
            integer :: icompo
            integer(kind=4) :: cpiter
            real(kind=4) :: ti
            real(kind=4) :: tf
            integer(kind=4) :: numpas
            character(len=144) :: nomvar
            integer(kind=4) :: idim
            integer(kind=4) :: taille
            character(len=6) :: nompal
            integer(kind=4) :: info
          end subroutine cplch
        end interface
