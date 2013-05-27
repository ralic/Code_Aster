        interface
          subroutine deltau(jrwork,jnbpg,nbpgt,nbordr,ordini,nmaini,&
     &nbmap,numpaq,tspaq,nommet,nomcri,nomfor,grdvie,forvie,cesr)
            integer :: jrwork
            integer :: jnbpg
            integer :: nbpgt
            integer :: nbordr
            integer :: ordini
            integer :: nmaini
            integer :: nbmap
            integer :: numpaq
            integer :: tspaq
            character(len=16) :: nommet
            character(len=16) :: nomcri
            character(len=16) :: nomfor
            character(len=8) :: grdvie
            character(len=16) :: forvie
            character(len=19) :: cesr
          end subroutine deltau
        end interface
