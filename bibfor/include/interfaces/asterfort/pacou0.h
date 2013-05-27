        interface
          subroutine pacou0(x,fvec,qt,r,c,d,fvcold,g,p,s,t,w,xold,work&
     &,check,vecr1,vecr2,typflu,vecr3,amor,masg,vecr4,vecr5,veci1,vg,&
     &indic,nbm,nmode,nt)
            integer :: nt
            real(kind=8) :: x(*)
            real(kind=8) :: fvec(*)
            real(kind=8) :: qt(nt,*)
            real(kind=8) :: r(nt,*)
            real(kind=8) :: c(*)
            real(kind=8) :: d(*)
            real(kind=8) :: fvcold(*)
            real(kind=8) :: g(*)
            real(kind=8) :: p(*)
            real(kind=8) :: s(*)
            real(kind=8) :: t(*)
            real(kind=8) :: w(*)
            real(kind=8) :: xold(*)
            real(kind=8) :: work(*)
            logical :: check
            real(kind=8) :: vecr1(*)
            real(kind=8) :: vecr2(*)
            character(len=8) :: typflu
            real(kind=8) :: vecr3(*)
            real(kind=8) :: amor(*)
            real(kind=8) :: masg(*)
            real(kind=8) :: vecr4(*)
            real(kind=8) :: vecr5(*)
            integer :: veci1(*)
            real(kind=8) :: vg
            integer :: indic
            integer :: nbm
            integer :: nmode
          end subroutine pacou0
        end interface
