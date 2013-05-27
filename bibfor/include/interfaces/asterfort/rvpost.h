        interface
          subroutine rvpost(mcf,iocc,dim,i1,i2,ncheff,xnomcp,nresu,&
     &nch19,nlsmac,nlsnac,nomtab,xnovar)
            character(*) :: mcf
            integer :: iocc
            character(len=2) :: dim
            integer :: i1
            integer :: i2
            character(len=16) :: ncheff
            character(len=24) :: xnomcp
            character(len=8) :: nresu
            character(len=19) :: nch19
            character(len=24) :: nlsmac
            character(len=24) :: nlsnac
            character(len=19) :: nomtab
            character(len=24) :: xnovar
          end subroutine rvpost
        end interface
