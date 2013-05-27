        interface
          subroutine dicor5(k0,sim,p1,pi,ui,ti,dxu1,dxu2,dryu1,dryu2,&
     &nu1,nu2,mu1,mu2,c1,dbar2,uu,tt,dur,dryr,dnsdu,dmsdt,dnsdt,dnsdu2,&
     &dmsdt2,dnsdt2,si,varip2,varip3)
            real(kind=8) :: k0(78)
            real(kind=8) :: sim(12)
            real(kind=8) :: p1
            real(kind=8) :: pi
            real(kind=8) :: ui
            real(kind=8) :: ti
            real(kind=8) :: dxu1
            real(kind=8) :: dxu2
            real(kind=8) :: dryu1
            real(kind=8) :: dryu2
            real(kind=8) :: nu1
            real(kind=8) :: nu2
            real(kind=8) :: mu1
            real(kind=8) :: mu2
            real(kind=8) :: c1
            real(kind=8) :: dbar2
            real(kind=8) :: uu
            real(kind=8) :: tt
            real(kind=8) :: dur
            real(kind=8) :: dryr
            real(kind=8) :: dnsdu
            real(kind=8) :: dmsdt
            real(kind=8) :: dnsdt
            real(kind=8) :: dnsdu2
            real(kind=8) :: dmsdt2
            real(kind=8) :: dnsdt2
            real(kind=8) :: si(12)
            real(kind=8) :: varip2
            real(kind=8) :: varip3
          end subroutine dicor5
        end interface
