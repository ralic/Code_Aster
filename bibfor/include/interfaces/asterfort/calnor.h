        interface
          subroutine calnor(chdim,geom,iare,nnos,nnoa,orien,nno,npg,&
     &noe,ifa,tymvol,idfde,jac,nx,ny,nz,tx,ty,hf)
            character(len=2) :: chdim
            real(kind=8) :: geom(*)
            integer :: iare
            integer :: nnos
            integer :: nnoa
            real(kind=8) :: orien
            integer :: nno
            integer :: npg
            integer :: noe(9,6,4)
            integer :: ifa
            integer :: tymvol
            integer :: idfde
            real(kind=8) :: jac(9)
            real(kind=8) :: nx(9)
            real(kind=8) :: ny(9)
            real(kind=8) :: nz(9)
            real(kind=8) :: tx(3)
            real(kind=8) :: ty(3)
            real(kind=8) :: hf
          end subroutine calnor
        end interface
