        interface
          subroutine stkgrp(ifl,icl,iv,rv,cv,cnl,mcl,nbm,numn,numm,grn&
     &,grm,irteti)
            integer :: nbm
            integer :: ifl
            integer :: icl
            integer :: iv
            real(kind=8) :: rv
            character(*) :: cv
            character(len=14) :: cnl
            character(len=8) :: mcl(nbm)
            integer :: numn
            integer :: numm
            character(len=24) :: grn
            character(len=24) :: grm
            integer :: irteti
          end subroutine stkgrp
        end interface
