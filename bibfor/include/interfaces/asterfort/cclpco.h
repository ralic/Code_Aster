        interface
          subroutine cclpco(option,resuou,numord,nbpaou,lipaou,lichou)
            character(len=16) :: option
            character(len=8) :: resuou
            integer :: numord
            integer :: nbpaou
            character(len=8) :: lipaou(*)
            character(len=24) :: lichou(*)
          end subroutine cclpco
        end interface
