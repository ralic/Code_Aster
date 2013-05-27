        interface
          subroutine flahqr(wantt,wantz,n,ilo,ihi,h,ldh,wr,wi,iloz,&
     &ihiz,z,ldz,info)
            integer :: ldz
            integer :: ldh
            logical :: wantt
            logical :: wantz
            integer :: n
            integer :: ilo
            integer :: ihi
            real(kind=8) :: h(ldh,*)
            real(kind=8) :: wr(*)
            real(kind=8) :: wi(*)
            integer :: iloz
            integer :: ihiz
            real(kind=8) :: z(ldz,*)
            integer :: info
          end subroutine flahqr
        end interface
