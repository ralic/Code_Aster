        interface
          subroutine matrc(nomte,nno,kcis,matc,vectt)
            character(*) :: nomte
            integer :: nno
            real(kind=8) :: kcis
            real(kind=8) :: matc(5,5)
            real(kind=8) :: vectt(3,3)
          end subroutine matrc
        end interface
