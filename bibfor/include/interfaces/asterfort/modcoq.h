        interface
          subroutine modcoq(base,nuor,nbm,mater1,mater2,noma,nomgrp,&
     &iaxe,kec,geom,vicoq,torco,tcoef,ifreba)
            integer :: nbm
            character(len=8) :: base
            integer :: nuor(nbm)
            character(len=8) :: mater1
            character(len=8) :: mater2
            character(len=8) :: noma
            character(len=24) :: nomgrp(*)
            integer :: iaxe
            integer :: kec
            real(kind=8) :: geom(9)
            integer :: vicoq(nbm)
            real(kind=8) :: torco(4,nbm)
            real(kind=8) :: tcoef(10,nbm)
            integer :: ifreba
          end subroutine modcoq
        end interface
