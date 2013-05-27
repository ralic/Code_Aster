        interface
          subroutine calvol(np1,nbm,icoupl,indic,kmod00,cmod00,amor00,&
     &puls00,pulsi,amori,masgi,tpfl,veci1,vecr1,vecr2,vecr5,vecr3,vgap,&
     &vecr4,locfl0,amflu0,xsi0)
            integer :: np1
            integer :: nbm
            integer :: icoupl
            integer :: indic
            real(kind=8) :: kmod00(np1,*)
            real(kind=8) :: cmod00(np1,*)
            real(kind=8) :: amor00(*)
            real(kind=8) :: puls00(*)
            real(kind=8) :: pulsi(*)
            real(kind=8) :: amori(*)
            real(kind=8) :: masgi(*)
            character(len=8) :: tpfl
            integer :: veci1(*)
            real(kind=8) :: vecr1(*)
            real(kind=8) :: vecr2(*)
            real(kind=8) :: vecr5(*)
            real(kind=8) :: vecr3(*)
            real(kind=8) :: vgap
            real(kind=8) :: vecr4(*)
            logical :: locfl0(*)
            real(kind=8) :: amflu0(np1,*)
            real(kind=8) :: xsi0(*)
          end subroutine calvol
        end interface
