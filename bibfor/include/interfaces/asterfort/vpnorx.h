        interface
          subroutine vpnorx(nbmode,neq,exclus,vecp,resufk)
            integer :: neq
            integer :: nbmode
            integer :: exclus(*)
            real(kind=8) :: vecp(neq,*)
            character(*) :: resufk(*)
          end subroutine vpnorx
        end interface
