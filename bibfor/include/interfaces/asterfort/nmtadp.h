        interface
          subroutine nmtadp(ndimsi,crit,mat,sigel,vim,epm,dp,sp,xi,g,&
     &iret)
            integer :: ndimsi
            real(kind=8) :: crit(3)
            real(kind=8) :: mat(14)
            real(kind=8) :: sigel(*)
            real(kind=8) :: vim(9)
            real(kind=8) :: epm(*)
            real(kind=8) :: dp
            real(kind=8) :: sp
            real(kind=8) :: xi
            real(kind=8) :: g
            integer :: iret
          end subroutine nmtadp
        end interface
