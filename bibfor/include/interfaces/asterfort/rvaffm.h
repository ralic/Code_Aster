        interface
          subroutine rvaffm(mcf,iocc,sdlieu,sdeval,sdmoy,oper,quant,&
     &option,rep,nomtab,xnovar,ncheff,i1,isd)
            character(*) :: mcf
            integer :: iocc
            character(len=24) :: sdlieu
            character(len=19) :: sdeval
            character(len=24) :: sdmoy
            character(len=16) :: oper
            character(*) :: quant
            character(*) :: option
            character(*) :: rep
            character(*) :: nomtab
            character(len=24) :: xnovar
            character(len=16) :: ncheff
            integer :: i1
            integer :: isd
          end subroutine rvaffm
        end interface
