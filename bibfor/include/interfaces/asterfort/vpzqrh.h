        interface
          subroutine vpzqrh(h,neq,ih,k,l,wr,wi,z,iz,mxiter,ier,nitqr)
            integer :: iz
            integer :: ih
            integer :: neq
            real(kind=8) :: h(ih,neq)
            integer :: k
            integer :: l
            real(kind=8) :: wr(neq)
            real(kind=8) :: wi(neq)
            real(kind=8) :: z(iz,neq)
            integer :: mxiter
            integer :: ier
            integer :: nitqr
          end subroutine vpzqrh
        end interface
