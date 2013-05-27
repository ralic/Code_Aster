        interface
          subroutine c3drep(nomte,epais,alpha,beta,coord,numnoe,pgl)
            character(len=16) :: nomte
            real(kind=8) :: epais
            real(kind=8) :: alpha
            real(kind=8) :: beta
            real(kind=8) :: coord(3,9)
            integer :: numnoe
            real(kind=8) :: pgl(3,3)
          end subroutine c3drep
        end interface
