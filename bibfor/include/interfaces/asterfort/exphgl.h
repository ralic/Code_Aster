        interface
          subroutine exphgl(nomres,typsd,modcyc,profno,indirf,mailsk,&
     &nbsec,numdia,nbmode)
            character(len=8) :: nomres
            character(len=16) :: typsd
            character(len=8) :: modcyc
            character(len=19) :: profno
            character(len=24) :: indirf
            character(len=8) :: mailsk
            integer :: nbsec
            integer :: numdia
            integer :: nbmode
          end subroutine exphgl
        end interface
