        interface
          subroutine pcfalu(n,inc,ipc,inx,ipx,index,imp)
            integer :: n
            integer :: inc(n)
            integer(kind=4) :: ipc(*)
            integer :: inx(0:n)
            integer(kind=4) :: ipx(*)
            integer :: index(*)
            integer :: imp
          end subroutine pcfalu
        end interface
