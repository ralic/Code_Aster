        interface
          subroutine mcconl(oper,lmat,neq2,typev,cvect,nvect)
            character(len=4) :: oper
            integer :: lmat
            integer :: neq2
            character(len=1) :: typev
            complex(kind=8) :: cvect(*)
            integer :: nvect
          end subroutine mcconl
        end interface
