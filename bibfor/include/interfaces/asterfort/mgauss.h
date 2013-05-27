        interface
          subroutine mgauss(cara,a,b,dim,nordre,nb,det,iret)
            integer :: nb
            integer :: dim
            character(*) :: cara
            real(kind=8) :: a(dim,dim)
            real(kind=8) :: b(dim,nb)
            integer :: nordre
            real(kind=8) :: det
            integer :: iret
          end subroutine mgauss
        end interface
