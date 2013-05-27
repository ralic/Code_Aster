        interface
          subroutine exchem(modloc,tcmp,nbc,nbsp,tvale,valcmp,taberr)
            integer :: modloc(*)
            integer :: tcmp(*)
            integer :: nbc
            integer :: nbsp
            real(kind=8) :: tvale(*)
            real(kind=8) :: valcmp(*)
            integer :: taberr(*)
          end subroutine exchem
        end interface
