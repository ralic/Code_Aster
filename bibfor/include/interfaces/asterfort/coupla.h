        interface
          subroutine coupla(np1,nbm,indic,tpfl,veci1,vgap,vecr4,vecr1,&
     &vecr2,vecr5,vecr3,masg,puls,locflc,amflu0,amfluc,xsi0)
            integer :: np1
            integer :: nbm
            integer :: indic
            character(len=8) :: tpfl
            integer :: veci1(*)
            real(kind=8) :: vgap
            real(kind=8) :: vecr4(*)
            real(kind=8) :: vecr1(*)
            real(kind=8) :: vecr2(*)
            real(kind=8) :: vecr5(*)
            real(kind=8) :: vecr3(*)
            real(kind=8) :: masg(*)
            real(kind=8) :: puls(*)
            logical :: locflc(*)
            real(kind=8) :: amflu0(np1,*)
            real(kind=8) :: amfluc(np1,*)
            real(kind=8) :: xsi0(*)
          end subroutine coupla
        end interface
