        interface
          subroutine ntweib(nrupt,cals,sk,sigw,nur,nt,nbres,x1,x2,xacc&
     &,rtsafe,impr,ifm,indtp,nbtp)
            integer :: nrupt
            logical :: cals
            real(kind=8) :: sk(*)
            real(kind=8) :: sigw(*)
            integer :: nur(*)
            integer :: nt(*)
            integer :: nbres
            real(kind=8) :: x1
            real(kind=8) :: x2
            real(kind=8) :: xacc
            real(kind=8) :: rtsafe
            logical :: impr
            integer :: ifm
            integer :: indtp(*)
            integer :: nbtp
          end subroutine ntweib
        end interface
