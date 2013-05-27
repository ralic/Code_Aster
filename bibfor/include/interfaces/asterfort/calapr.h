        interface
          subroutine calapr(nbddl,mu,afmu,ddl,atmu)
            integer :: nbddl
            real(kind=8) :: mu
            real(kind=8) :: afmu(*)
            integer :: ddl(nbddl)
            real(kind=8) :: atmu(*)
          end subroutine calapr
        end interface
