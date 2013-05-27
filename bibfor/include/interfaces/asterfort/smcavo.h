        interface
          subroutine smcavo(x,ind,nbhist,trc)
            integer :: nbhist
            real(kind=8) :: x(5)
            integer :: ind(6)
            real(kind=8) :: trc((3*nbhist),5)
          end subroutine smcavo
        end interface
