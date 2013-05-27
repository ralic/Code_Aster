        interface
          subroutine trvpmd(np1,n,m,rr,loc,indxf,npoint,lpoint,tp,rtp)
            integer :: np1
            integer :: n
            integer :: m
            real(kind=8) :: rr(*)
            logical :: loc(*)
            integer :: indxf(*)
            integer :: npoint(*)
            integer :: lpoint(*)
            real(kind=8) :: tp(*)
            real(kind=8) :: rtp(*)
          end subroutine trvpmd
        end interface
