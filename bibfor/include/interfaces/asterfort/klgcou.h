        interface
          subroutine klgcou(nno,nbrddl,pgl1,pgl2,pgl3,pgl4,k)
            integer :: nbrddl
            integer :: nno
            real(kind=8) :: pgl1(3,3)
            real(kind=8) :: pgl2(3,3)
            real(kind=8) :: pgl3(3,3)
            real(kind=8) :: pgl4(3,3)
            real(kind=8) :: k(nbrddl,nbrddl)
          end subroutine klgcou
        end interface
