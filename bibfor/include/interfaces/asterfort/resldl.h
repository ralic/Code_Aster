        interface
          subroutine resldl(solveu,nommat,vcine,nsecm,rsolu,csolu,&
     &prepos)
            character(len=19) :: solveu
            character(*) :: nommat
            character(*) :: vcine
            integer :: nsecm
            real(kind=8) :: rsolu(*)
            complex(kind=8) :: csolu(*)
            logical :: prepos
          end subroutine resldl
        end interface
