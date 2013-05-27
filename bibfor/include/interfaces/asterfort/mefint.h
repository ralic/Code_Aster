        interface
          subroutine mefint(nbz,nbgrp,nbmod,nbnoe,nbddl,irot,numnog,&
     &nbnog,zint,defm,phix,phiy,z,num)
            integer :: nbnoe
            integer :: nbmod
            integer :: nbgrp
            integer :: nbz
            integer :: nbddl
            integer :: irot(3)
            integer :: numnog(nbgrp)
            integer :: nbnog(nbgrp)
            real(kind=8) :: zint(nbz,nbgrp)
            real(kind=8) :: defm(6*nbnoe,nbmod)
            real(kind=8) :: phix(nbz,nbgrp,nbmod)
            real(kind=8) :: phiy(nbz,nbgrp,nbmod)
            real(kind=8) :: z(*)
            integer :: num(nbz)
          end subroutine mefint
        end interface
