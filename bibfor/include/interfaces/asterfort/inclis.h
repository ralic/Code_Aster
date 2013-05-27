        interface
          subroutine inclis(nomres,ssta,sstb,intfa,intfb,fmlia,fplian,&
     &fplibn,fpliao,fplibo,iada,iadb,numlis,matprj)
            character(len=8) :: nomres
            character(len=8) :: ssta
            character(len=8) :: sstb
            character(len=8) :: intfa
            character(len=8) :: intfb
            character(len=24) :: fmlia
            character(len=24) :: fplian
            character(len=24) :: fplibn
            character(len=24) :: fpliao
            character(len=24) :: fplibo
            integer :: iada(3)
            integer :: iadb(3)
            integer :: numlis
            character(len=8) :: matprj
          end subroutine inclis
        end interface
