        interface
          subroutine mdconf(typflu,base,noma,nbm,lnoe,nuor,iimpr,indic&
     &,veci1,vecr1,vecr2,vecr3,vecr4,vecr5)
            character(len=8) :: typflu
            character(len=8) :: base
            character(len=8) :: noma
            integer :: nbm
            integer :: lnoe
            integer :: nuor(*)
            integer :: iimpr
            integer :: indic
            integer :: veci1(*)
            real(kind=8) :: vecr1(*)
            real(kind=8) :: vecr2(*)
            real(kind=8) :: vecr3(*)
            real(kind=8) :: vecr4(*)
            real(kind=8) :: vecr5(*)
          end subroutine mdconf
        end interface
