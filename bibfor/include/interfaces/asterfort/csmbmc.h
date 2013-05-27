        interface
          subroutine csmbmc(nommat,neq,vsmb)
            character(*) :: nommat
            integer :: neq
            complex(kind=8) :: vsmb(*)
          end subroutine csmbmc
        end interface
