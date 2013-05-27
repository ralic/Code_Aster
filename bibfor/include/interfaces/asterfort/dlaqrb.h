        interface
          subroutine dlaqrb(wantt,n,ilo,ihi,h,ldh,wr,wi,z,info)
            integer :: ldh
            logical :: wantt
            integer :: n
            integer :: ilo
            integer :: ihi
            real(kind=8) :: h(ldh,*)
            real(kind=8) :: wr(*)
            real(kind=8) :: wi(*)
            real(kind=8) :: z(*)
            integer :: info
          end subroutine dlaqrb
        end interface
