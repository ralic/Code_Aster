        interface
          subroutine mmconv(noma,sdimpr,defico,resoco,valinc,solalg,&
     &vfrot,nfrot,vgeom,ngeom)
            character(len=8) :: noma
            character(len=24) :: sdimpr
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            real(kind=8) :: vfrot
            character(len=16) :: nfrot
            real(kind=8) :: vgeom
            character(len=16) :: ngeom
          end subroutine mmconv
        end interface
