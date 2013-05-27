        interface
          subroutine dtauno(jrwork,lisnoe,nbnot,nbordr,ordini,nnoini,&
     &nbnop,tspaq,nommet,nomcri,nomfor,grdvie,forvie,nommai,cnsr,nommap,&
     &post,valpar,vresu)
            integer :: nbnot
            integer :: jrwork
            integer :: lisnoe(nbnot)
            integer :: nbordr
            integer :: ordini
            integer :: nnoini
            integer :: nbnop
            integer :: tspaq
            character(len=16) :: nommet
            character(len=16) :: nomcri
            character(len=16) :: nomfor
            character(len=8) :: grdvie
            character(len=16) :: forvie
            character(len=8) :: nommai
            character(len=19) :: cnsr
            character(len=8) :: nommap
            logical :: post
            real(kind=8) :: valpar(22)
            real(kind=8) :: vresu(24)
          end subroutine dtauno
        end interface
