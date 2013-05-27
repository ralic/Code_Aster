        interface
          subroutine fcesnd(nomte,ind,xi1,xi2,xi3,char,vf)
            character(len=8) :: nomte
            integer :: ind
            real(kind=8) :: xi1
            real(kind=8) :: xi2
            real(kind=8) :: xi3
            character(len=2) :: char
            real(kind=8) :: vf(*)
          end subroutine fcesnd
        end interface
