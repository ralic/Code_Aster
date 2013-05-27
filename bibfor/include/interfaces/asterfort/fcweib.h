        interface
          subroutine fcweib(nrupt,cals,sk,sigw,nur,nt,nbres,indtp,nbtp&
     &,m,fc,dfc)
            integer :: nrupt
            logical :: cals
            real(kind=8) :: sk(*)
            real(kind=8) :: sigw(*)
            integer :: nur(*)
            integer :: nt(*)
            integer :: nbres
            integer :: indtp(*)
            integer :: nbtp
            real(kind=8) :: m
            real(kind=8) :: fc
            real(kind=8) :: dfc
          end subroutine fcweib
        end interface
