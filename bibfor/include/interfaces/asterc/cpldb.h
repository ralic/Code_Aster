        interface
          subroutine cpldb(icompo,cpiter,ti,tf,numpas,nomvar,idim,&
     &taille,paramr,info)
            integer :: icompo
            integer(kind=4) :: cpiter
            real(kind=8) :: ti
            real(kind=8) :: tf
            integer(kind=4) :: numpas
            character(len=144) :: nomvar
            integer(kind=4) :: idim
            integer(kind=4) :: taille
            real(kind=8) :: paramr(6)
            integer(kind=4) :: info
          end subroutine cpldb
        end interface
