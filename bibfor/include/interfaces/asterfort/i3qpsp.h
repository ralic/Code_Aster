        interface
          subroutine i3qpsp(epsi,k,f,sgt,coorsm,res,nbpt)
            real(kind=8) :: epsi
            integer :: k
            integer :: f
            real(kind=8) :: sgt(*)
            real(kind=8) :: coorsm(3,*)
            real(kind=8) :: res(3,*)
            integer :: nbpt
          end subroutine i3qpsp
        end interface
