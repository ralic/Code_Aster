        interface
          subroutine cgnop0(nbnoe,coor,x0,vecnor,prec,nbno,lisnoe)
            integer :: nbnoe
            real(kind=8) :: coor(*)
            real(kind=8) :: x0(*)
            real(kind=8) :: vecnor(*)
            real(kind=8) :: prec
            integer :: nbno
            integer :: lisnoe(*)
          end subroutine cgnop0
        end interface
