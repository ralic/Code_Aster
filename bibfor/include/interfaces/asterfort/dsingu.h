        interface
          subroutine dsingu(ndim,nelem,nnoem,nsommx,nelcom,degre,icnc,&
     &numeli,xy,erreur,energi,mesu,alpha,nalpha)
            integer :: nelcom
            integer :: nsommx
            integer :: nnoem
            integer :: nelem
            integer :: ndim
            integer :: degre
            integer :: icnc(nsommx+2,nelem)
            integer :: numeli(nelcom+2,nnoem)
            real(kind=8) :: xy(3,nnoem)
            real(kind=8) :: erreur(nelem)
            real(kind=8) :: energi(nelem)
            real(kind=8) :: mesu(nelem)
            real(kind=8) :: alpha(nelem)
            integer :: nalpha
          end subroutine dsingu
        end interface
