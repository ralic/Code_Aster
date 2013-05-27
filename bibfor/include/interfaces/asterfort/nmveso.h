        interface
          subroutine nmveso(rb,nb,rp,np,drbdb,drbdp,drpdb,drpdp,dp,&
     &dbeta,nr,cplan)
            integer :: np
            integer :: nb
            real(kind=8) :: rb(nb)
            real(kind=8) :: rp(np)
            real(kind=8) :: drbdb(nb,nb)
            real(kind=8) :: drbdp(nb,np)
            real(kind=8) :: drpdb(np,nb)
            real(kind=8) :: drpdp(np,np)
            real(kind=8) :: dp(np)
            real(kind=8) :: dbeta(nb)
            integer :: nr
            logical :: cplan
          end subroutine nmveso
        end interface
