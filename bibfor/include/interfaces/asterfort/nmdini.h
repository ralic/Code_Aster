        interface
          subroutine nmdini(motfac,iocc,provli,instin,linsei,tole,&
     &nbinst,linsti,numini)
            character(len=16) :: motfac
            integer :: iocc
            character(len=19) :: provli
            real(kind=8) :: instin
            logical :: linsei
            real(kind=8) :: tole
            integer :: nbinst
            logical :: linsti
            integer :: numini
          end subroutine nmdini
        end interface
