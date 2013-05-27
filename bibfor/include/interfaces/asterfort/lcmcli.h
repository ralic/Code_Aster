        interface
          subroutine lcmcli(comp,nomfam,nbsys,is,pgl,sigf,sicl)
            character(len=16) :: comp(*)
            character(len=16) :: nomfam
            integer :: nbsys
            integer :: is
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: sigf(6)
            real(kind=8) :: sicl
          end subroutine lcmcli
        end interface
