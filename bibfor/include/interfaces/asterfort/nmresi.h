        interface
          subroutine nmresi(noma,mate,numedd,sdnume,fonact,sddyna,&
     &sdconv,sdimpr,defico,resoco,matass,numins,conv,resigr,eta,comref,&
     &valinc,solalg,veasse,measse,vrela,vmaxi,vchar,vresi,vrefe,vinit,&
     &vcomp,vfrot,vgeom)
            character(len=8) :: noma
            character(len=24) :: mate
            character(len=24) :: numedd
            character(len=19) :: sdnume
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=24) :: sdconv
            character(len=24) :: sdimpr
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: matass
            integer :: numins
            real(kind=8) :: conv(*)
            real(kind=8) :: resigr
            real(kind=8) :: eta
            character(len=24) :: comref
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veasse(*)
            character(len=19) :: measse(*)
            real(kind=8) :: vrela
            real(kind=8) :: vmaxi
            real(kind=8) :: vchar
            real(kind=8) :: vresi
            real(kind=8) :: vrefe
            real(kind=8) :: vinit
            real(kind=8) :: vcomp
            real(kind=8) :: vfrot
            real(kind=8) :: vgeom
          end subroutine nmresi
        end interface
