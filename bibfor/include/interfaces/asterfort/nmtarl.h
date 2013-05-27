        interface
          subroutine nmtarl(mode,ndimsi,mat,sigel,vim,epm,dp,sp,xi,&
     &dirdp,dirsp,dirxi,min,rho,ener)
            integer :: mode
            integer :: ndimsi
            real(kind=8) :: mat(*)
            real(kind=8) :: sigel(*)
            real(kind=8) :: vim(*)
            real(kind=8) :: epm(*)
            real(kind=8) :: dp
            real(kind=8) :: sp
            real(kind=8) :: xi
            real(kind=8) :: dirdp
            real(kind=8) :: dirsp
            real(kind=8) :: dirxi
            real(kind=8) :: min
            real(kind=8) :: rho
            real(kind=8) :: ener
          end subroutine nmtarl
        end interface
