        interface
          subroutine rvaffs(mcf,iocc,sdlieu,sdeval,sdmoy,quant,option,&
     &rep,nomtab,ncheff,i1,isd)
            character(*) :: mcf
            integer :: iocc
            character(len=24) :: sdlieu
            character(len=19) :: sdeval
            character(len=24) :: sdmoy
            character(*) :: quant
            character(*) :: option
            character(*) :: rep
            character(len=19) :: nomtab
            character(len=16) :: ncheff
            integer :: i1
            integer :: isd
          end subroutine rvaffs
        end interface
