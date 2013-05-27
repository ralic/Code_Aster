        interface
          subroutine nmpr3d(mode,nno,npg,poidsg,vff,dff,geom,p,vect,&
     &matc)
            integer :: npg
            integer :: nno
            integer :: mode
            real(kind=8) :: poidsg(npg)
            real(kind=8) :: vff(nno,npg)
            real(kind=8) :: dff(2,nno,npg)
            real(kind=8) :: geom(3,nno)
            real(kind=8) :: p(npg)
            real(kind=8) :: vect(3,nno)
            real(kind=8) :: matc(3,nno,3,nno)
          end subroutine nmpr3d
        end interface
