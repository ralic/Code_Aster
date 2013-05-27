        interface
          subroutine mdallo(nomres,basemo,masgen,riggen,amogen,nbmode,&
     &dt,nbsauv,nbchoc,noecho,intitu,nbrede,fonred,nbrevi,fonrev,jdepl,&
     &jvite,jacce,jptem,jordr,jdisc,jfcho,jdcho,jvcho,jadcho,jredc,jredd&
     &,jrevc,jrevd,method,nbsym,nomsym,typcal,sauve)
            integer :: nbrevi
            integer :: nbrede
            integer :: nbchoc
            character(len=8) :: nomres
            character(*) :: basemo
            character(*) :: masgen
            character(*) :: riggen
            character(*) :: amogen
            integer :: nbmode
            real(kind=8) :: dt
            integer :: nbsauv
            character(len=8) :: noecho(nbchoc,*)
            character(len=8) :: intitu(*)
            character(len=8) :: fonred(nbrede,*)
            character(len=8) :: fonrev(nbrevi,*)
            integer :: jdepl
            integer :: jvite
            integer :: jacce
            integer :: jptem
            integer :: jordr
            integer :: jdisc
            integer :: jfcho
            integer :: jdcho
            integer :: jvcho
            integer :: jadcho
            integer :: jredc
            integer :: jredd
            integer :: jrevc
            integer :: jrevd
            character(len=16) :: method
            integer :: nbsym
            character(len=4) :: nomsym(3)
            character(len=4) :: typcal
            character(len=4) :: sauve
          end subroutine mdallo
        end interface
