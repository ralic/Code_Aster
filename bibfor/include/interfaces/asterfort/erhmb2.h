        interface
          subroutine erhmb2(perman,ino,nbs,ndim,theta,instpm,jac,nx,ny&
     &,tx,ty,nbcmp,geom,ivois,sielnp,sielnm,adsip,iagd,tbref2,iade2,&
     &iava2,ncmpm2,iaptm2,iade3,iava3,ncmpm3,iaptm3,tm2h1b)
            integer :: ndim
            logical :: perman
            integer :: ino
            integer :: nbs
            real(kind=8) :: theta
            real(kind=8) :: instpm(2)
            real(kind=8) :: jac(3)
            real(kind=8) :: nx(3)
            real(kind=8) :: ny(3)
            real(kind=8) :: tx(3)
            real(kind=8) :: ty(3)
            integer :: nbcmp
            real(kind=8) :: geom(ndim,*)
            integer :: ivois
            real(kind=8) :: sielnp(90)
            real(kind=8) :: sielnm(90)
            integer :: adsip
            integer :: iagd
            integer :: tbref2(12)
            integer :: iade2
            integer :: iava2
            integer :: ncmpm2
            integer :: iaptm2
            integer :: iade3
            integer :: iava3
            integer :: ncmpm3
            integer :: iaptm3
            real(kind=8) :: tm2h1b(3)
          end subroutine erhmb2
        end interface
