        interface
          subroutine nmtaac(type,ndimsi,mat,sigel,vim,epm,dp,sp,xi,&
     &sigp,vip)
            integer :: ndimsi
            integer :: type
            real(kind=8) :: mat(14)
            real(kind=8) :: sigel(ndimsi)
            real(kind=8) :: vim(9)
            real(kind=8) :: epm(ndimsi)
            real(kind=8) :: dp
            real(kind=8) :: sp
            real(kind=8) :: xi
            real(kind=8) :: sigp(ndimsi)
            real(kind=8) :: vip(9)
          end subroutine nmtaac
        end interface
