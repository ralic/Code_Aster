        interface
          subroutine nmveot(drbdb,drbdp,drpdb,drpdp,drbde,drpde,dsgde,&
     &dsgdb,dsgdp,np,nb,nr,dsidep)
            integer :: nb
            integer :: np
            real(kind=8) :: drbdb(nb,nb)
            real(kind=8) :: drbdp(nb,np)
            real(kind=8) :: drpdb(np,nb)
            real(kind=8) :: drpdp(np,np)
            real(kind=8) :: drbde(nb,nb)
            real(kind=8) :: drpde(np,nb)
            real(kind=8) :: dsgde(nb,nb)
            real(kind=8) :: dsgdb(nb,nb)
            real(kind=8) :: dsgdp(nb,np)
            integer :: nr
            real(kind=8) :: dsidep(nb,nb)
          end subroutine nmveot
        end interface
