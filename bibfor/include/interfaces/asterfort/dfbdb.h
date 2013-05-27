        interface
          subroutine dfbdb(dim,b,e,deuxmu,lambda,ecrob,dsidep)
            integer :: dim
            real(kind=8) :: b(6)
            real(kind=8) :: e(6)
            real(kind=8) :: deuxmu
            real(kind=8) :: lambda
            real(kind=8) :: ecrob
            real(kind=8) :: dsidep(6,6)
          end subroutine dfbdb
        end interface
