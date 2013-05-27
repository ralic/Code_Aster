        interface
          subroutine mefgri(ntypg,nbgtot,zg,hg,itypg,zmin,zmax)
            integer :: nbgtot
            integer :: ntypg
            real(kind=8) :: zg(nbgtot)
            real(kind=8) :: hg(ntypg)
            integer :: itypg(nbgtot)
            real(kind=8) :: zmin
            real(kind=8) :: zmax
          end subroutine mefgri
        end interface
