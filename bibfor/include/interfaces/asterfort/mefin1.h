        interface
          function mefin1(nbz,nbgrp,imod,icyl,jmod,jcyl,z,f1,f2,f3)
            integer :: nbgrp
            integer :: nbz
            integer :: imod
            integer :: icyl
            integer :: jmod
            integer :: jcyl
            real(kind=8) :: z(*)
            real(kind=8) :: f1(nbz*nbgrp,*)
            real(kind=8) :: f2(nbz*nbgrp,*)
            real(kind=8) :: f3(*)
            real(kind=8) :: mefin1
          end function mefin1
        end interface
