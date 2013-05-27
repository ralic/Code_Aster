        interface
          subroutine irchor(ioccur,leresu,lresul,nchsym,nnuord,nlicmp,&
     &novcmp,nnopar,nbnosy,nbordr,nbrcmp,nbcmdu,nbpara,codret)
            integer :: ioccur
            character(len=8) :: leresu
            logical :: lresul
            character(*) :: nchsym
            character(*) :: nnuord
            character(*) :: nlicmp
            character(*) :: novcmp
            character(*) :: nnopar
            integer :: nbnosy
            integer :: nbordr
            integer :: nbrcmp
            integer :: nbcmdu
            integer :: nbpara
            integer :: codret
          end subroutine irchor
        end interface
