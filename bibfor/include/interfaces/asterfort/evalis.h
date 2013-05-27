        interface
          subroutine evalis(isz,pg,phi,sphi,freq,iff,nomres)
            character(len=8) :: isz
            character(len=19) :: pg
            character(len=19) :: phi
            character(len=19) :: sphi
            real(kind=8) :: freq
            integer :: iff
            character(len=8) :: nomres
          end subroutine evalis
        end interface
