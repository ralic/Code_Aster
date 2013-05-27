        interface
          subroutine hujci1(crit,mater,deps,sigd,i1f,tract,iret)
            real(kind=8) :: crit(*)
            real(kind=8) :: mater(22,2)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: i1f
            logical :: tract
            integer :: iret
          end subroutine hujci1
        end interface
