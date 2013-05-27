        interface
          subroutine fnorm(dnorm,vitloc,knorm,cnorm,cost,sint,fnorma,&
     &flocal,vnorm)
            real(kind=8) :: dnorm
            real(kind=8) :: vitloc(3)
            real(kind=8) :: knorm
            real(kind=8) :: cnorm
            real(kind=8) :: cost
            real(kind=8) :: sint
            real(kind=8) :: fnorma
            real(kind=8) :: flocal(3)
            real(kind=8) :: vnorm
          end subroutine fnorm
        end interface
