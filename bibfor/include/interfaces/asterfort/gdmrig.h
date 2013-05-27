        interface
          subroutine gdmrig(kp,nno,ajacob,pjacob,en,enprim,x0pg,rot0,&
     &rotk,granc,pn,pm,rigi)
            integer :: kp
            integer :: nno
            real(kind=8) :: ajacob
            real(kind=8) :: pjacob
            real(kind=8) :: en(3,2)
            real(kind=8) :: enprim(3,2)
            real(kind=8) :: x0pg(3)
            real(kind=8) :: rot0(3,3)
            real(kind=8) :: rotk(3,3)
            real(kind=8) :: granc(6)
            real(kind=8) :: pn(3)
            real(kind=8) :: pm(3)
            real(kind=8) :: rigi(18,18)
          end subroutine gdmrig
        end interface
