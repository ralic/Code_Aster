        interface
          subroutine xpomax(mo,malini,mailx,nbnoc,nbmac,prefno,nogrfi,&
     &maxfem,cns1,cns2,ces1,ces2,cesvi1,cesvi2,listgr,dirgrm,nivgrm,&
     &resuco,ngfon,comps1,comps2)
            character(len=8) :: mo
            character(len=8) :: malini
            character(len=24) :: mailx
            integer :: nbnoc
            integer :: nbmac
            character(len=2) :: prefno(4)
            character(len=24) :: nogrfi
            character(len=8) :: maxfem
            character(len=19) :: cns1
            character(len=19) :: cns2
            character(len=19) :: ces1
            character(len=19) :: ces2
            character(len=19) :: cesvi1
            character(len=19) :: cesvi2
            character(len=24) :: listgr
            character(len=24) :: dirgrm
            character(len=24) :: nivgrm
            character(len=8) :: resuco
            integer :: ngfon
            character(len=19) :: comps1
            character(len=19) :: comps2
          end subroutine xpomax
        end interface
