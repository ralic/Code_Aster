        interface
          subroutine crcnct(base,nomch,mailla,gd,nbcmp,licmp,rcmp)
            integer :: nbcmp
            character(*) :: base
            character(*) :: nomch
            character(*) :: mailla
            character(*) :: gd
            character(*) :: licmp(nbcmp)
            real(kind=8) :: rcmp(nbcmp)
          end subroutine crcnct
        end interface
