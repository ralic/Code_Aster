        interface
          subroutine mdfdas(dnorm,vnorm,vitloc,cost,sint,coefk1,coefk2&
     &,coefpy,coefc,coefad,xmax,fdispo,flocal)
            real(kind=8) :: dnorm
            real(kind=8) :: vnorm
            real(kind=8) :: vitloc(6)
            real(kind=8) :: cost
            real(kind=8) :: sint
            real(kind=8) :: coefk1
            real(kind=8) :: coefk2
            real(kind=8) :: coefpy
            real(kind=8) :: coefc
            real(kind=8) :: coefad
            real(kind=8) :: xmax
            real(kind=8) :: fdispo
            real(kind=8) :: flocal(3)
          end subroutine mdfdas
        end interface
