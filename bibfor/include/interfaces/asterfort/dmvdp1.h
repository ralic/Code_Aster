        interface
          function dmvdp1(rho11,rho12,sat,dsatp1,biot,phi,cs,pvp,emmag&
     &,em)
            real(kind=8) :: rho11
            real(kind=8) :: rho12
            real(kind=8) :: sat
            real(kind=8) :: dsatp1
            real(kind=8) :: biot
            real(kind=8) :: phi
            real(kind=8) :: cs
            real(kind=8) :: pvp
            logical :: emmag
            real(kind=8) :: em
            real(kind=8) :: dmvdp1
          end function dmvdp1
        end interface
