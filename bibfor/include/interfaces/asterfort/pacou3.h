        interface
          subroutine pacou3(xold,fold,g,p,x,f,fvec,stpmax,check,tolx,&
     &vecr1,vecr2,typflu,vecr3,amor,masg,vecr4,vecr5,veci1,vg,indic,nbm,&
     &nmode,n)
            real(kind=8) :: xold(*)
            real(kind=8) :: fold
            real(kind=8) :: g(*)
            real(kind=8) :: p(*)
            real(kind=8) :: x(*)
            real(kind=8) :: f
            real(kind=8) :: fvec(*)
            real(kind=8) :: stpmax
            logical :: check
            real(kind=8) :: tolx
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
            integer :: n
          end subroutine pacou3
        end interface
