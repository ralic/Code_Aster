        interface
          subroutine xpodim(malini,mailc,modvis,licham,nsetot,nnntot,&
     &ncotot,listno,cns1,cns2,ces1,ces2,cel2,cesvi1,cesvi2,ior,resuco,&
     &nbnoc,nbmac,logrma,dirgrm,maxfem,ngfon,comps1,comps2)
            character(len=8) :: malini
            character(len=24) :: mailc
            character(len=8) :: modvis
            character(len=24) :: licham
            integer :: nsetot
            integer :: nnntot
            integer :: ncotot
            character(len=24) :: listno
            character(len=19) :: cns1
            character(len=19) :: cns2
            character(len=19) :: ces1
            character(len=19) :: ces2
            character(len=19) :: cel2
            character(len=19) :: cesvi1
            character(len=19) :: cesvi2
            integer :: ior
            character(len=8) :: resuco
            integer :: nbnoc
            integer :: nbmac
            character(len=24) :: logrma
            character(len=24) :: dirgrm
            character(len=8) :: maxfem
            integer :: ngfon
            character(len=19) :: comps1
            character(len=19) :: comps2
          end subroutine xpodim
        end interface
