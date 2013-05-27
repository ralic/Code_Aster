        interface
          subroutine mecagl(option,result,modele,depla,thetai,mate,&
     &compor,nchar,lchar,symech,chfond,nnoff,iord,ndeg,thlagr,glagr,&
     &thlag2,milieu,ndimte,pair,extim,time,nbprup,noprup,chvite,chacce,&
     &lmelas,nomcas,kcalc,fonoeu)
            character(len=16) :: option
            character(len=8) :: result
            character(len=8) :: modele
            character(len=24) :: depla
            character(len=8) :: thetai
            character(len=24) :: mate
            character(len=24) :: compor
            integer :: nchar
            character(len=8) :: lchar(*)
            character(len=8) :: symech
            character(len=24) :: chfond
            integer :: nnoff
            integer :: iord
            integer :: ndeg
            logical :: thlagr
            logical :: glagr
            logical :: thlag2
            logical :: milieu
            integer :: ndimte
            logical :: pair
            logical :: extim
            real(kind=8) :: time
            integer :: nbprup
            character(len=16) :: noprup(*)
            character(len=24) :: chvite
            character(len=24) :: chacce
            logical :: lmelas
            character(len=16) :: nomcas
            character(len=8) :: kcalc
            character(len=24) :: fonoeu
          end subroutine mecagl
        end interface
