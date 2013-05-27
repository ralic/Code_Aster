        interface
          subroutine geocoq(noma,nomgrp,caelem,iaxe,geom)
            character(len=8) :: noma
            character(len=24) :: nomgrp(*)
            character(len=8) :: caelem
            integer :: iaxe
            real(kind=8) :: geom(9)
          end subroutine geocoq
        end interface
