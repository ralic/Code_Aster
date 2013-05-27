        interface
          subroutine cesred(ces1z,nbma,lima,nbcmp,licmp,base,ces2z)
            integer :: nbma
            character(*) :: ces1z
            integer :: lima(nbma)
            integer :: nbcmp
            character(*) :: licmp(*)
            character(*) :: base
            character(*) :: ces2z
          end subroutine cesred
        end interface
