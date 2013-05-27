        interface
          subroutine lkdfds(nbmat,mater,s,para,var,ds2hds,ucri,dfdsig)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: s(6)
            real(kind=8) :: para(3)
            real(kind=8) :: var(4)
            real(kind=8) :: ds2hds(6)
            real(kind=8) :: ucri
            real(kind=8) :: dfdsig(6)
          end subroutine lkdfds
        end interface
