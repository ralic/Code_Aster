        interface
          subroutine nmcrls(sddisc,provli,numini,numfin,linsti,instin,&
     &nbtemp,dtmin)
            character(len=19) :: sddisc
            character(len=19) :: provli
            integer :: numini
            integer :: numfin
            logical :: linsti
            real(kind=8) :: instin
            integer :: nbtemp
            real(kind=8) :: dtmin
          end subroutine nmcrls
        end interface
