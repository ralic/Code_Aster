        interface
          subroutine nmextv(neff,formul,nomcmp,valcmp,nvalcp,valres)
            integer :: neff
            character(len=8) :: formul
            character(len=8) :: nomcmp(*)
            real(kind=8) :: valcmp(*)
            integer :: nvalcp
            real(kind=8) :: valres(*)
          end subroutine nmextv
        end interface
