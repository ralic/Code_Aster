        interface
          subroutine dalp2d(nelem,nnoem,degre,nsommx,icnc,nelcom,&
     &numeli,xy,erreur,energi,aire,alpha,nalpha)
            integer :: nelcom
            integer :: nsommx
            integer :: nnoem
            integer :: nelem
            integer :: degre
            integer :: icnc(nsommx+2,nelem)
            integer :: numeli(nelcom+2,nnoem)
            real(kind=8) :: xy(3,nnoem)
            real(kind=8) :: erreur(nelem)
            real(kind=8) :: energi(nelem)
            real(kind=8) :: aire(nelem)
            real(kind=8) :: alpha(nelem)
            integer :: nalpha
          end subroutine dalp2d
        end interface
