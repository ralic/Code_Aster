        interface
          subroutine iniqs4(nno,sdfde,sdfdk,poipg,coopg)
            integer :: nno
            real(kind=8) :: sdfde(4,4)
            real(kind=8) :: sdfdk(4,4)
            real(kind=8) :: poipg(4)
            real(kind=8) :: coopg(8)
          end subroutine iniqs4
        end interface
