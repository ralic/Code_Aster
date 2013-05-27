        interface
          subroutine lrvcpg(idfimd,nbpgm,nbpga,nomtm,tygeos,elrefa,&
     &elrefm,fapg,nloc,permu,nutyma,codret)
            integer :: nbpgm
            integer :: idfimd
            integer :: nbpga
            character(len=8) :: nomtm
            integer :: tygeos
            character(len=8) :: elrefa
            character(len=8) :: elrefm
            character(len=8) :: fapg
            integer :: nloc
            integer :: permu(nbpgm)
            integer :: nutyma
            integer :: codret
          end subroutine lrvcpg
        end interface
