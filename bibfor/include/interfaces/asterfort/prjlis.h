        interface
          subroutine prjlis(moda,maa,modb,mab,nbnoa,nbnob,motcle,linta&
     &,lintb,intfa,intfb,fpliao,fplibo,iada,iadb,numlis,matprj,modgen,&
     &ssta,sstb)
            character(len=8) :: moda
            character(len=8) :: maa
            character(len=8) :: modb
            character(len=8) :: mab
            integer :: nbnoa
            integer :: nbnob
            character(len=16) :: motcle(2)
            character(len=8) :: linta
            character(len=8) :: lintb
            character(len=8) :: intfa
            character(len=8) :: intfb
            character(len=24) :: fpliao
            character(len=24) :: fplibo
            integer :: iada(3)
            integer :: iadb(3)
            integer :: numlis
            character(len=8) :: matprj
            character(len=8) :: modgen
            character(len=8) :: ssta
            character(len=8) :: sstb
          end subroutine prjlis
        end interface
