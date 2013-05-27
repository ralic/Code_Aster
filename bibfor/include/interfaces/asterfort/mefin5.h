        interface
          function mefin5(nbz,nbgrp,imod,icyl,jmod,jcyl,z,f1,f2,f3,g)
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
            real(kind=8) :: g(*)
            real(kind=8) :: mefin5
          end function mefin5
        end interface
