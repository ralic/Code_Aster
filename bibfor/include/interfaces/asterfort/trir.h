        interface
          subroutine trir(clef,tab,ntab,n)
            integer :: n
            integer :: clef(*)
            real(kind=8) :: tab(n,*)
            integer :: ntab
          end subroutine trir
        end interface
