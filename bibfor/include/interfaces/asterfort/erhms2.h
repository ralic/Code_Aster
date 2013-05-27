        interface
          subroutine erhms2(perman,ino,nbs,theta,jac,nx,ny,sielnp,&
     &adsip,sielnm,nbcmp,typmav,tbref1,tbref2,ivois,tm2h1s)
            logical :: perman
            integer :: ino
            integer :: nbs
            real(kind=8) :: theta
            real(kind=8) :: jac(3)
            real(kind=8) :: nx(3)
            real(kind=8) :: ny(3)
            real(kind=8) :: sielnp(90)
            integer :: adsip
            real(kind=8) :: sielnm(90)
            integer :: nbcmp
            character(len=8) :: typmav
            integer :: tbref1(12)
            integer :: tbref2(12)
            integer :: ivois
            real(kind=8) :: tm2h1s(3)
          end subroutine erhms2
        end interface
