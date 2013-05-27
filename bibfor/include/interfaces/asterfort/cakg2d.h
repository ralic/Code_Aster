        interface
          subroutine cakg2d(optioz,result,modele,depla,theta,mate,&
     &nchar,lchar,symech,fondf,noeud,time,iord,nbprup,noprup,lmelas,&
     &nomcas,lmoda,puls,compor)
            character(len=16) :: optioz
            character(len=8) :: result
            character(len=8) :: modele
            character(len=24) :: depla
            character(len=24) :: theta
            character(len=24) :: mate
            integer :: nchar
            character(len=8) :: lchar(*)
            character(len=8) :: symech
            character(len=8) :: fondf
            character(len=8) :: noeud
            real(kind=8) :: time
            integer :: iord
            integer :: nbprup
            character(len=16) :: noprup(*)
            logical :: lmelas
            character(len=16) :: nomcas
            logical :: lmoda
            real(kind=8) :: puls
            character(len=24) :: compor
          end subroutine cakg2d
        end interface
