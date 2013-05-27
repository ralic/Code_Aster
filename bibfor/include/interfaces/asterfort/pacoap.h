        interface
          subroutine pacoap(lisi1z,lisi2z,lonlis,centre,theta,t,nomaz,&
     &liso1z,liso2z)
            character(*) :: lisi1z
            character(*) :: lisi2z
            integer :: lonlis
            real(kind=8) :: centre(3)
            real(kind=8) :: theta(3)
            real(kind=8) :: t(3)
            character(*) :: nomaz
            character(*) :: liso1z
            character(*) :: liso2z
          end subroutine pacoap
        end interface
