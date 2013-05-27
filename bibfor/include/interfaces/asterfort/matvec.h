        interface
          subroutine matvec(nordre,amat,nombv,v1,v2,vecres)
            integer :: nordre
            real(kind=8) :: amat(*)
            integer :: nombv
            real(kind=8) :: v1(*)
            real(kind=8) :: v2(*)
            real(kind=8) :: vecres(*)
          end subroutine matvec
        end interface
