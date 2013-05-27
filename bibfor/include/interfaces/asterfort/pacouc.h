        interface
          subroutine pacouc(typflu,vecr1,vecr2,vite,vecr3,masg,freq,&
     &amor,nbno,indic,nbpv,w,veci1,vecr4,vecr5,ier)
            character(len=8) :: typflu
            real(kind=8) :: vecr1(*)
            real(kind=8) :: vecr2(*)
            real(kind=8) :: vite(*)
            real(kind=8) :: vecr3(*)
            real(kind=8) :: masg(*)
            real(kind=8) :: freq(*)
            real(kind=8) :: amor(*)
            integer :: nbno
            integer :: indic
            integer :: nbpv
            real(kind=8) :: w(*)
            integer :: veci1(*)
            real(kind=8) :: vecr4(*)
            real(kind=8) :: vecr5(*)
            integer :: ier
          end subroutine pacouc
        end interface
