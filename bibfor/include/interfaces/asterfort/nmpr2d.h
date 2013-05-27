        interface
          subroutine nmpr2d(mode,laxi,nno,npg,poidsg,vff,dff,geom,p,&
     &vect,matc)
            integer :: npg
            integer :: nno
            integer :: mode
            logical :: laxi
            real(kind=8) :: poidsg(npg)
            real(kind=8) :: vff(nno,npg)
            real(kind=8) :: dff(nno,npg)
            real(kind=8) :: geom(2,nno)
            real(kind=8) :: p(2,npg)
            real(kind=8) :: vect(2,nno)
            real(kind=8) :: matc(2,nno,2,nno)
          end subroutine nmpr2d
        end interface
