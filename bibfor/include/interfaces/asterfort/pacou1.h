        interface
          subroutine pacou1(x,fvec,df,work,eps,vecr1,vecr2,typflu,&
     &vecr3,amor,masg,vecr4,vecr5,veci1,vg,indic,nbm,nmode,n)
            integer :: n
            real(kind=8) :: x(*)
            real(kind=8) :: fvec(*)
            real(kind=8) :: df(n,*)
            real(kind=8) :: work(*)
            real(kind=8) :: eps
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
          end subroutine pacou1
        end interface
