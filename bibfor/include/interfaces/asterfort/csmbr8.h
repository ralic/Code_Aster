        interface
          subroutine csmbr8(nommat,ccll,ccii,neq,vcine,vsmb)
            character(*) :: nommat
            integer :: ccll(*)
            integer :: ccii(*)
            integer :: neq
            real(kind=8) :: vcine(*)
            real(kind=8) :: vsmb(*)
          end subroutine csmbr8
        end interface
