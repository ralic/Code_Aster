        interface
          subroutine defgen(testl1,testl2,nno,r,x3,sina,cosa,cour,vf,&
     &dfds,depl,eps,epsx3)
            logical :: testl1
            logical :: testl2
            integer :: nno
            real(kind=8) :: r
            real(kind=8) :: x3
            real(kind=8) :: sina
            real(kind=8) :: cosa
            real(kind=8) :: cour
            real(kind=8) :: vf(*)
            real(kind=8) :: dfds(*)
            real(kind=8) :: depl(*)
            real(kind=8) :: eps(*)
            real(kind=8) :: epsx3
          end subroutine defgen
        end interface
