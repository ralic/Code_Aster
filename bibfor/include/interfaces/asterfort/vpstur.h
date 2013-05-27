        interface
          subroutine vpstur(lmatk,valshi,lmatm,lmatsh,mantis,expo,&
     &pivot,ier,solveu,caldet,calfac)
            integer :: lmatk
            real(kind=8) :: valshi
            integer :: lmatm
            integer :: lmatsh
            real(kind=8) :: mantis
            integer :: expo
            integer :: pivot
            integer :: ier
            character(len=19) :: solveu
            logical :: caldet
            logical :: calfac
          end subroutine vpstur
        end interface
