        interface
          subroutine lrmngr(ngrmax,ngrp,numgrp,nomgrp,jnogrp,jlggrp,&
     &nbnufa,nomjng,nomjlg)
            integer :: ngrmax
            integer :: ngrp
            integer :: numgrp
            character(len=24) :: nomgrp
            integer :: jnogrp
            integer :: jlggrp
            integer :: nbnufa
            character(*) :: nomjng
            character(*) :: nomjlg
          end subroutine lrmngr
        end interface
