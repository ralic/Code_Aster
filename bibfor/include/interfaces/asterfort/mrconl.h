        interface
          subroutine mrconl(oper,lmat,neq2,typev,rvect,nvect)
            character(len=4) :: oper
            integer :: lmat
            integer :: neq2
            character(len=1) :: typev
            real(kind=8) :: rvect(*)
            integer :: nvect
          end subroutine mrconl
        end interface
