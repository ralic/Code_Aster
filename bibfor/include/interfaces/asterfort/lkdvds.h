        interface
          subroutine lkdvds(dt,nbmat,mater,gv,dfdsv,seuilv,dvds)
            integer :: nbmat
            real(kind=8) :: dt
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: gv(6)
            real(kind=8) :: dfdsv(6)
            real(kind=8) :: seuilv
            real(kind=8) :: dvds(6,6)
          end subroutine lkdvds
        end interface
