        interface
          subroutine vlgglc(nno,nbrddl,pgl1,pgl2,pgl3,pgl4,v,code,p,&
     &vtemp)
            integer :: nbrddl
            integer :: nno
            real(kind=8) :: pgl1(3,3)
            real(kind=8) :: pgl2(3,3)
            real(kind=8) :: pgl3(3,3)
            real(kind=8) :: pgl4(3,3)
            real(kind=8) :: v(nbrddl)
            character(len=2) :: code
            real(kind=8) :: p(nbrddl,nbrddl)
            real(kind=8) :: vtemp(nbrddl)
          end subroutine vlgglc
        end interface
