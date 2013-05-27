        interface
          subroutine csmbc8(nommat,ccll,ccii,neq,vcine,vsmb)
            character(*) :: nommat
            integer :: ccll(*)
            integer :: ccii(*)
            integer :: neq
            complex(kind=8) :: vcine(*)
            complex(kind=8) :: vsmb(*)
          end subroutine csmbc8
        end interface
