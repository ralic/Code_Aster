        interface
          subroutine xtdepm(ndim,jnnm,jnne,ndeple,nsinge,nsingm,ffe,&
     &ffm,jdepde,rre,rrm,jddle,jddlm,ddeple,ddeplm)
            integer :: ndim
            integer :: jnnm(3)
            integer :: jnne(3)
            integer :: ndeple
            integer :: nsinge
            integer :: nsingm
            real(kind=8) :: ffe(20)
            real(kind=8) :: ffm(20)
            integer :: jdepde
            real(kind=8) :: rre
            real(kind=8) :: rrm
            integer :: jddle(2)
            integer :: jddlm(2)
            real(kind=8) :: ddeple(3)
            real(kind=8) :: ddeplm(3)
          end subroutine xtdepm
        end interface
