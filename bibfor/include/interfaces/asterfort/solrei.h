        interface
          subroutine solrei(gamp,s,i1n,parame,nbmat,mater,q,vecn,&
     &codret)
            integer :: nbmat
            real(kind=8) :: gamp
            real(kind=8) :: s(6)
            real(kind=8) :: i1n
            real(kind=8) :: parame(5)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: q(6)
            real(kind=8) :: vecn(6)
            integer :: codret
          end subroutine solrei
        end interface
