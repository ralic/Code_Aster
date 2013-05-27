        interface
          function dmasp1(rho11,rho12,rho21,sat,dsatp1,biot,phi,cs,pas&
     &,emmag,em)
            real(kind=8) :: rho11
            real(kind=8) :: rho12
            real(kind=8) :: rho21
            real(kind=8) :: sat
            real(kind=8) :: dsatp1
            real(kind=8) :: biot
            real(kind=8) :: phi
            real(kind=8) :: cs
            real(kind=8) :: pas
            logical :: emmag
            real(kind=8) :: em
            real(kind=8) :: dmasp1
          end function dmasp1
        end interface
