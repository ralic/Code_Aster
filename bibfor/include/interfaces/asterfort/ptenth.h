        interface
          subroutine ptenth(ul,xl,f,n,mat,itype,enerth)
            integer :: n
            real(kind=8) :: ul(12)
            real(kind=8) :: xl
            real(kind=8) :: f
            real(kind=8) :: mat(n,n)
            integer :: itype
            real(kind=8) :: enerth
          end subroutine ptenth
        end interface
