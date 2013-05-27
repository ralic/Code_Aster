        interface
          subroutine avgrno(vwork,tdisp,lisnoe,nbnot,nbordr,nnoini,&
     &nbnop,tspaq,nomcri,nomfor,grdvie,forvie,fordef,nommai,proaxe,&
     &nommap,cnsr,post,resu)
            integer :: nbnop
            integer :: tdisp
            real(kind=8) :: vwork(tdisp)
            integer :: lisnoe(nbnop)
            integer :: nbnot
            integer :: nbordr
            integer :: nnoini
            integer :: tspaq
            character(len=16) :: nomcri
            character(len=16) :: nomfor
            character(len=8) :: grdvie
            character(len=16) :: forvie
            logical :: fordef
            character(len=8) :: nommai
            character(len=16) :: proaxe
            character(len=8) :: nommap
            character(len=19) :: cnsr
            logical :: post
            real(kind=8) :: resu(7)
          end subroutine avgrno
        end interface
