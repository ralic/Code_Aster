        interface
          subroutine ascopr(lmasym,lmesym,tt,jtmp2,nrmax,jresl,rcoef,&
     &jvalm)
            logical :: lmasym
            logical :: lmesym
            character(len=2) :: tt
            integer :: jtmp2
            integer :: nrmax
            integer :: jresl
            real(kind=8) :: rcoef
            integer :: jvalm(2)
          end subroutine ascopr
        end interface
