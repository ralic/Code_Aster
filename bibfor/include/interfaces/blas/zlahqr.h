        interface
          subroutine zlahqr(wantt,wantz,n,ilo,ihi,h,ldh,w,iloz,ihiz,z,&
     &ldz,info)
            integer :: ldz
            integer :: ldh
            logical :: wantt
            logical :: wantz
            integer :: n
            integer :: ilo
            integer :: ihi
            complex(kind=8) :: h(ldh,*)
            complex(kind=8) :: w(*)
            integer :: iloz
            integer :: ihiz
            complex(kind=8) :: z(ldz,*)
            integer :: info
          end subroutine zlahqr
        end interface
