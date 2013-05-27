        interface
          subroutine rvaffe(mcf,iocc,sdlieu,sdeval,sdmail,typaff,quant&
     &,option,rep,nomtab,xnovar,ncheff,i1,isd)
            character(*) :: mcf
            integer :: iocc
            character(len=24) :: sdlieu
            character(len=19) :: sdeval
            character(len=24) :: sdmail
            character(len=1) :: typaff
            character(*) :: quant
            character(*) :: option
            character(*) :: rep
            character(len=19) :: nomtab
            character(len=24) :: xnovar
            character(len=16) :: ncheff
            integer :: i1
            integer :: isd
          end subroutine rvaffe
        end interface
