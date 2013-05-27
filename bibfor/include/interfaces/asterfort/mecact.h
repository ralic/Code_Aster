        interface
          subroutine mecact(base,nomcar,moclez,nomco,nomgdz,ncmp,licmp&
     &,icmp,rcmp,ccmp,kcmp)
            integer :: ncmp
            character(*) :: base
            character(*) :: nomcar
            character(*) :: moclez
            character(*) :: nomco
            character(*) :: nomgdz
            character(*) :: licmp(ncmp)
            integer :: icmp(ncmp)
            real(kind=8) :: rcmp(ncmp)
            complex(kind=8) :: ccmp(ncmp)
            character(*) :: kcmp(ncmp)
          end subroutine mecact
        end interface
