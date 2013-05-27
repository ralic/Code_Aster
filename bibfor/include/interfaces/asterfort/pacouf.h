        interface
          subroutine pacouf(x,fvect,vecr1,vecr2,typflu,vecr3,amor,masg&
     &,vecr4,vecr5,veci1,vgap,indic,nbm,nmode)
            real(kind=8) :: x(2)
            real(kind=8) :: fvect(2)
            real(kind=8) :: vecr1(*)
            real(kind=8) :: vecr2(*)
            character(len=8) :: typflu
            real(kind=8) :: vecr3(*)
            real(kind=8) :: amor(*)
            real(kind=8) :: masg(*)
            real(kind=8) :: vecr4(*)
            real(kind=8) :: vecr5(*)
            integer :: veci1(*)
            real(kind=8) :: vgap
            integer :: indic
            integer :: nbm
            integer :: nmode
          end subroutine pacouf
        end interface
