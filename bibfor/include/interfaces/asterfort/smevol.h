        interface
          subroutine smevol(temper,modelz,mate,compor,option,phasin,&
     &numpha)
            character(len=8) :: temper
            character(*) :: modelz
            character(len=24) :: mate
            character(len=24) :: compor
            character(len=16) :: option
            character(len=24) :: phasin
            integer :: numpha
          end subroutine smevol
        end interface
