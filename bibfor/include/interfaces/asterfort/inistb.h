        interface
          subroutine inistb(maxnod,nbtyma,nomail,indic,permut,limail,&
     &indicf,permuf,maxfa)
            integer :: maxfa
            integer :: maxnod
            integer :: nbtyma
            character(len=8) :: nomail(*)
            integer :: indic(*)
            integer :: permut(maxnod,*)
            integer :: limail(*)
            integer :: indicf(*)
            integer :: permuf(maxfa,*)
          end subroutine inistb
        end interface
