        interface
          subroutine klg(nno,nbrddl,pgl,k)
            integer :: nbrddl
            integer :: nno
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: k(nbrddl,nbrddl)
          end subroutine klg
        end interface
