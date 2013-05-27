        interface
          subroutine utvois(typmac,lmaj,nbf,nsomf,poinc1,poinc2,elrefe&
     &,ndegre)
            character(len=8) :: typmac
            logical :: lmaj
            integer :: nbf
            integer :: nsomf
            real(kind=8) :: poinc1
            real(kind=8) :: poinc2
            character(len=8) :: elrefe
            integer :: ndegre
          end subroutine utvois
        end interface
