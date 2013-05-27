        interface
          subroutine dngets(ishift,which,kev,np,ritzr,ritzi,bounds,&
     &shiftr,shifti)
            integer :: np
            integer :: kev
            integer :: ishift
            character(len=2) :: which
            real(kind=8) :: ritzr(kev+np)
            real(kind=8) :: ritzi(kev+np)
            real(kind=8) :: bounds(kev+np)
            real(kind=8) :: shiftr(1)
            real(kind=8) :: shifti(1)
          end subroutine dngets
        end interface
