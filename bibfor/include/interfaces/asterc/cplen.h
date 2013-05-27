        interface
          subroutine cplen(icompo,cpiter,ti,tf,numpas,nomvar,idim,&
     &taille,parami,info)
            integer :: icompo
            integer(kind=4) :: cpiter
            real(kind=4) :: ti
            real(kind=4) :: tf
            integer(kind=4) :: numpas
            character(len=144) :: nomvar
            integer(kind=4) :: idim
            integer(kind=4) :: taille
            integer(kind=4) :: parami
            integer(kind=4) :: info
          end subroutine cplen
        end interface
