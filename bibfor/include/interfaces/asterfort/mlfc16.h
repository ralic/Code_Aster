        interface
          subroutine mlfc16(nommat,npivot,neq,typsym,eps,renumz)
            character(*) :: nommat
            integer :: npivot
            integer :: neq
            integer :: typsym
            real(kind=8) :: eps
            character(*) :: renumz
          end subroutine mlfc16
        end interface
