        interface
          subroutine evali2(isz,pg,nma,phi,valpar,posmai,ipg,pdgi,icmp&
     &,nocmpi,sphi)
            character(len=8) :: isz
            character(len=19) :: pg
            integer :: nma
            character(len=19) :: phi
            real(kind=8) :: valpar(7)
            integer :: posmai
            integer :: ipg
            real(kind=8) :: pdgi
            integer :: icmp
            character(len=8) :: nocmpi
            character(len=19) :: sphi
          end subroutine evali2
        end interface
