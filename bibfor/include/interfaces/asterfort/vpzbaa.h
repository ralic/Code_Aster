        interface
          subroutine vpzbaa(n,ib,a,ia,ibas,lhi,d)
            integer :: ia
            integer :: n
            integer :: ib
            real(kind=8) :: a(ia,n)
            integer :: ibas
            integer :: lhi
            real(kind=8) :: d(n)
          end subroutine vpzbaa
        end interface
