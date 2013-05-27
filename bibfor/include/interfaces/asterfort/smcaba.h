        interface
          subroutine smcaba(ftrc,trc,nbhist,x,dz,ind)
            integer :: nbhist
            real(kind=8) :: ftrc((3*nbhist),3)
            real(kind=8) :: trc((3*nbhist),5)
            real(kind=8) :: x(5)
            real(kind=8) :: dz(4)
            integer :: ind(6)
          end subroutine smcaba
        end interface
