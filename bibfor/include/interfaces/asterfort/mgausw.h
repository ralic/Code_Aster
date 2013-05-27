        interface
          subroutine mgausw(a,b,dim,nordre,nb,det,iret)
            integer :: nb
            integer :: dim
            real(kind=8) :: a(dim,dim)
            real(kind=8) :: b(dim,nb)
            integer :: nordre
            real(kind=8) :: det
            logical :: iret
          end subroutine mgausw
        end interface
