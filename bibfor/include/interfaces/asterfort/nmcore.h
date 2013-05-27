        interface
          subroutine nmcore(sdcrit,sderro,sdconv,defico,numins,iterat,&
     &fonact,relite,eta,parcri,vresi,vrela,vmaxi,vchar,vrefe,vcomp,vfrot&
     &,vgeom)
            character(len=19) :: sdcrit
            character(len=24) :: sderro
            character(len=24) :: sdconv
            character(len=24) :: defico
            integer :: numins
            integer :: iterat
            integer :: fonact(*)
            integer :: relite
            real(kind=8) :: eta
            real(kind=8) :: parcri(*)
            real(kind=8) :: vresi
            real(kind=8) :: vrela
            real(kind=8) :: vmaxi
            real(kind=8) :: vchar
            real(kind=8) :: vrefe
            real(kind=8) :: vcomp
            real(kind=8) :: vfrot
            real(kind=8) :: vgeom
          end subroutine nmcore
        end interface
