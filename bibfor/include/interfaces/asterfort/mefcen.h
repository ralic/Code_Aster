        interface
          subroutine mefcen(caelem,iequiv,nbcyl,nbz,irot,numnog,nbnog,&
     &nummag,numgrp,coor,cent,req,xint,yint,zint,rint,nbgrp)
            integer :: nbgrp
            integer :: nbz
            integer :: nbcyl
            character(len=19) :: caelem
            integer :: iequiv
            integer :: irot(3)
            integer :: numnog(*)
            integer :: nbnog(*)
            integer :: nummag(*)
            integer :: numgrp(*)
            real(kind=8) :: coor(*)
            real(kind=8) :: cent(2*nbcyl)
            real(kind=8) :: req(nbgrp)
            real(kind=8) :: xint(nbcyl)
            real(kind=8) :: yint(nbcyl)
            real(kind=8) :: zint(nbz,nbgrp)
            real(kind=8) :: rint(nbcyl)
          end subroutine mefcen
        end interface
