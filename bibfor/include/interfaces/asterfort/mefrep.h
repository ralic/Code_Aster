        interface
          subroutine mefrep(nbz,nbmod,nbcyl,nbgrp,numgrp,z,freq0,rho,&
     &visc,rint,phix,phiy,dcent,matma)
            integer :: nbz
            integer :: nbmod
            integer :: nbcyl
            integer :: nbgrp
            integer :: numgrp(*)
            real(kind=8) :: z(*)
            real(kind=8) :: freq0(*)
            real(kind=8) :: rho(*)
            real(kind=8) :: visc(*)
            real(kind=8) :: rint(*)
            real(kind=8) :: phix(*)
            real(kind=8) :: phiy(*)
            real(kind=8) :: dcent(*)
            real(kind=8) :: matma(*)
          end subroutine mefrep
        end interface
