        interface
          subroutine avgrma(vwork,tdisp,vnbpg,nbpgt,nbordr,nmaini,&
     &nbmap,numpaq,tspaq,nomcri,nomfor,grdvie,forvie,fordef,proaxe,cesr)
            integer :: nbmap
            integer :: tdisp
            real(kind=8) :: vwork(tdisp)
            integer :: vnbpg(nbmap)
            integer :: nbpgt
            integer :: nbordr
            integer :: nmaini
            integer :: numpaq
            integer :: tspaq
            character(len=16) :: nomcri
            character(len=16) :: nomfor
            character(len=8) :: grdvie
            character(len=16) :: forvie
            logical :: fordef
            character(len=16) :: proaxe
            character(len=19) :: cesr
          end subroutine avgrma
        end interface
