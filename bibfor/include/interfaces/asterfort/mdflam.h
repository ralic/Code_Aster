        interface
          subroutine mdflam(dnorm,vitloc,knorm,cost,sint,flim,fseuil,&
     &rigifl,defpla,fnorma,flocal,vnorm)
            real(kind=8) :: dnorm
            real(kind=8) :: vitloc(3)
            real(kind=8) :: knorm
            real(kind=8) :: cost
            real(kind=8) :: sint
            real(kind=8) :: flim
            real(kind=8) :: fseuil
            real(kind=8) :: rigifl
            real(kind=8) :: defpla
            real(kind=8) :: fnorma
            real(kind=8) :: flocal(3)
            real(kind=8) :: vnorm
          end subroutine mdflam
        end interface
