        interface
          subroutine concrk(nomres,parch,facobj,nbobjs,nom4rk,nbsaui,&
     &basemo,masgen,riggen,amogen,neqgen,dt,nbchoc,noecho,intitu,nbrede,&
     &fonred,nbrevi,fonrev,method)
            integer :: nbrevi
            integer :: nbrede
            integer :: nbchoc
            character(len=8) :: nomres
            integer :: parch
            real(kind=8) :: facobj
            integer :: nbobjs
            character(len=4) :: nom4rk
            integer :: nbsaui
            character(*) :: basemo
            character(*) :: masgen
            character(*) :: riggen
            character(*) :: amogen
            integer :: neqgen
            real(kind=8) :: dt
            character(len=8) :: noecho(nbchoc,*)
            character(len=8) :: intitu(*)
            character(len=8) :: fonred(nbrede,*)
            character(len=8) :: fonrev(nbrevi,*)
            character(len=16) :: method
          end subroutine concrk
        end interface
