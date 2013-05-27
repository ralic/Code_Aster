        interface
          subroutine lcmhsr(necoul,necris,nbsys,nbcoef,coefh,nsg,hsr)
            integer :: nsg
            character(len=16) :: necoul
            character(len=16) :: necris
            integer :: nbsys
            integer :: nbcoef
            real(kind=8) :: coefh(6)
            real(kind=8) :: hsr(nsg,nsg)
          end subroutine lcmhsr
        end interface
