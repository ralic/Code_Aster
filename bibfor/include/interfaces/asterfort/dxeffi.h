        interface
          subroutine dxeffi(option,nomte,pgl,cont,ind,effint)
            character(*) :: option
            character(len=16) :: nomte
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: cont(*)
            integer :: ind
            real(kind=8) :: effint(*)
          end subroutine dxeffi
        end interface
