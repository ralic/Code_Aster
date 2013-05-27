        interface
          subroutine nmdopi(modelz,numedd,method,lreli,sdpilo)
            character(*) :: modelz
            character(len=24) :: numedd
            character(len=16) :: method(*)
            logical :: lreli
            character(len=19) :: sdpilo
          end subroutine nmdopi
        end interface
