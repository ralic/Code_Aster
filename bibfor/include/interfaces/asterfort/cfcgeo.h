        interface
          subroutine cfcgeo(noma,defico,resoco,solalg,dvgeom,geoerr,&
     &geonoe,geoval)
            character(len=8) :: noma
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: solalg(*)
            logical :: dvgeom
            logical :: geoerr
            character(len=16) :: geonoe
            real(kind=8) :: geoval
          end subroutine cfcgeo
        end interface
