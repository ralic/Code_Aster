        interface
          subroutine fornor(dnorm,vitloc,knorm,cnorm,cost,sint,fnorma,&
     &flocal,vnorm,iforn)
            real(kind=8) :: dnorm
            real(kind=8) :: vitloc(*)
            real(kind=8) :: knorm
            real(kind=8) :: cnorm
            real(kind=8) :: cost
            real(kind=8) :: sint
            real(kind=8) :: fnorma
            real(kind=8) :: flocal(*)
            real(kind=8) :: vnorm
            integer :: iforn
          end subroutine fornor
        end interface
