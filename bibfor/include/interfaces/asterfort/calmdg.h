        interface
          subroutine calmdg(model,modgen,nugene,num,nu,ma,mate,moint,&
     &moflui,ndble,itxsto,itysto,itzsto,iprsto,nbmo,iadirg)
            character(len=2) :: model
            character(len=8) :: modgen
            character(len=14) :: nugene
            character(len=14) :: num
            character(len=14) :: nu
            character(len=8) :: ma
            character(*) :: mate
            character(len=8) :: moint
            character(len=8) :: moflui
            integer :: ndble
            integer :: itxsto
            integer :: itysto
            integer :: itzsto
            integer :: iprsto
            integer :: nbmo
            integer :: iadirg
          end subroutine calmdg
        end interface
