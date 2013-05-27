        interface
          subroutine wpnorx(nbmode,neq,exclus,vecp,resufk)
            integer :: neq
            integer :: nbmode
            integer :: exclus(*)
            complex(kind=8) :: vecp(neq,nbmode)
            character(*) :: resufk(*)
          end subroutine wpnorx
        end interface
