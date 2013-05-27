        interface
          subroutine matrc2(nbpar,nompar,valpar,kcis,matc,vectt)
            integer :: nbpar
            character(len=8) :: nompar(*)
            real(kind=8) :: valpar(*)
            real(kind=8) :: kcis
            real(kind=8) :: matc(5,5)
            real(kind=8) :: vectt(3,3)
          end subroutine matrc2
        end interface
