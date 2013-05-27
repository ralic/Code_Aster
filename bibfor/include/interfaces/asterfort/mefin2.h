        interface
          function mefin2(nbz,nbgrp,imod,icyl,jmod,jcyl,z,f1,f2,g)
            integer :: nbgrp
            integer :: nbz
            integer :: imod
            integer :: icyl
            integer :: jmod
            integer :: jcyl
            real(kind=8) :: z(*)
            real(kind=8) :: f1(nbz*nbgrp,*)
            real(kind=8) :: f2(nbz*nbgrp,*)
            real(kind=8) :: g(*)
            real(kind=8) :: mefin2
          end function mefin2
        end interface
