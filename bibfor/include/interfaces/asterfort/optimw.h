        interface
          subroutine optimw(method,nrupt,x,y,prob,sigw,nt,nur,nbres,&
     &calm,cals,mk,sk,mkp,skp,impr,ifm,dept,indtp,nbtp)
            character(len=16) :: method
            integer :: nrupt
            real(kind=8) :: x(*)
            real(kind=8) :: y(*)
            real(kind=8) :: prob(*)
            real(kind=8) :: sigw(*)
            integer :: nt(*)
            integer :: nur(*)
            integer :: nbres
            logical :: calm
            logical :: cals
            real(kind=8) :: mk
            real(kind=8) :: sk(*)
            real(kind=8) :: mkp
            real(kind=8) :: skp(*)
            logical :: impr
            integer :: ifm
            logical :: dept
            integer :: indtp(*)
            integer :: nbtp
          end subroutine optimw
        end interface
