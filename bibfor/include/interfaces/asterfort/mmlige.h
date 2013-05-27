        interface
          subroutine mmlige(noma,defico,resoco,typelt,nbtyp,comptc,&
     &comptf,nndtot,nbgrel)
            integer :: nbtyp
            character(len=8) :: noma
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=24) :: typelt
            integer :: comptc(nbtyp)
            integer :: comptf(nbtyp)
            integer :: nndtot
            integer :: nbgrel
          end subroutine mmlige
        end interface
