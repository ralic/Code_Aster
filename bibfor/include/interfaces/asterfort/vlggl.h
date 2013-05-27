        interface
          subroutine vlggl(nno,nbrddl,pgl,v,code,p,vtemp)
            integer :: nbrddl
            integer :: nno
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: v(nbrddl)
            character(len=2) :: code
            real(kind=8) :: p(nbrddl,nbrddl)
            real(kind=8) :: vtemp(nbrddl)
          end subroutine vlggl
        end interface
