        interface
          subroutine dicor2(k0,p1,p2,dur,dryr,dxu,dryu,feq,nu,mu,uu,tt&
     &,si1,dnsdu,dmsdt,dnsdt,varip1,varip2,si2)
            real(kind=8) :: k0(78)
            real(kind=8) :: p1
            real(kind=8) :: p2
            real(kind=8) :: dur
            real(kind=8) :: dryr
            real(kind=8) :: dxu
            real(kind=8) :: dryu
            real(kind=8) :: feq
            real(kind=8) :: nu
            real(kind=8) :: mu
            real(kind=8) :: uu
            real(kind=8) :: tt
            real(kind=8) :: si1(12)
            real(kind=8) :: dnsdu
            real(kind=8) :: dmsdt
            real(kind=8) :: dnsdt
            real(kind=8) :: varip1
            real(kind=8) :: varip2
            real(kind=8) :: si2(12)
          end subroutine dicor2
        end interface
