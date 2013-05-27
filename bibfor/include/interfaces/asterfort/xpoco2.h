        interface
          subroutine xpoco2(malini,dirno,nbno,dirma,nbma,cns1,cns2,&
     &ces1,ces2,cesvi1,cesvi2,resuco,comps1,comps2)
            integer :: nbma
            integer :: nbno
            character(len=8) :: malini
            integer :: dirno(nbno)
            integer :: dirma(nbma)
            character(len=19) :: cns1
            character(len=19) :: cns2
            character(len=19) :: ces1
            character(len=19) :: ces2
            character(len=19) :: cesvi1
            character(len=19) :: cesvi2
            character(len=8) :: resuco
            character(len=19) :: comps1
            character(len=19) :: comps2
          end subroutine xpoco2
        end interface
