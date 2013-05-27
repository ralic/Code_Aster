        interface
          subroutine mecalg(optioz,result,modele,depla,theta,mate,&
     &nchar,lchar,symech,compor,incr,time,iord,nbprup,noprup,chvite,&
     &chacce,lmelas,nomcas,kcalc)
            character(len=16) :: optioz
            character(len=8) :: result
            character(len=8) :: modele
            character(len=24) :: depla
            character(len=24) :: theta
            character(len=24) :: mate
            integer :: nchar
            character(len=8) :: lchar(*)
            character(len=8) :: symech
            character(len=24) :: compor
            logical :: incr
            real(kind=8) :: time
            integer :: iord
            integer :: nbprup
            character(len=16) :: noprup(*)
            character(len=24) :: chvite
            character(len=24) :: chacce
            logical :: lmelas
            character(len=16) :: nomcas
            character(len=8) :: kcalc
          end subroutine mecalg
        end interface
