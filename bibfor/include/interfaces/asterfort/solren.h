        interface
          subroutine solren(sn,nbmat,mater,q,codret)
            integer :: nbmat
            real(kind=8) :: sn(6)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: q(6)
            integer :: codret
          end subroutine solren
        end interface
