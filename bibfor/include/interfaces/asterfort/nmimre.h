        interface
          subroutine nmimre(numedd,sdimpr,sdconv,vrela,vmaxi,vrefe,&
     &vcomp,vfrot,vgeom,irela,imaxi,irefe,noddlm,icomp,nfrot,ngeom)
            character(len=24) :: numedd
            character(len=24) :: sdimpr
            character(len=24) :: sdconv
            real(kind=8) :: vrela
            real(kind=8) :: vmaxi
            real(kind=8) :: vrefe
            real(kind=8) :: vcomp
            real(kind=8) :: vfrot
            real(kind=8) :: vgeom
            integer :: irela
            integer :: imaxi
            integer :: irefe
            character(len=8) :: noddlm
            integer :: icomp
            character(len=16) :: nfrot
            character(len=16) :: ngeom
          end subroutine nmimre
        end interface
