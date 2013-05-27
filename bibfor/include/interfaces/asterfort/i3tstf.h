        interface
          subroutine i3tstf(k,f,desc,desctm,conexk,coordo,gauche,epsi)
            integer :: k
            integer :: f
            integer :: desc(*)
            integer :: desctm(*)
            integer :: conexk(*)
            real(kind=8) :: coordo(*)
            logical :: gauche
            real(kind=8) :: epsi
          end subroutine i3tstf
        end interface
