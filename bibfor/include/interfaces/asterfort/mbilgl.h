        interface
          subroutine mbilgl(option,result,modele,depla1,depla2,thetai,&
     &mate,nchar,lchar,symech,chfond,nnoff,ndeg,thlagr,glagr,thlag2,&
     &milieu,ndimte,pair,extim,timeu,timev,indi,indj,nbprup,noprup,&
     &lmelas,nomcas,fonoeu)
            character(len=16) :: option
            character(len=8) :: result
            character(len=8) :: modele
            character(len=24) :: depla1
            character(len=24) :: depla2
            character(len=8) :: thetai
            character(len=24) :: mate
            integer :: nchar
            character(len=8) :: lchar(*)
            character(len=8) :: symech
            character(len=24) :: chfond
            integer :: nnoff
            integer :: ndeg
            logical :: thlagr
            logical :: glagr
            logical :: thlag2
            logical :: milieu
            integer :: ndimte
            logical :: pair
            logical :: extim
            real(kind=8) :: timeu
            real(kind=8) :: timev
            integer :: indi
            integer :: indj
            integer :: nbprup
            character(len=16) :: noprup(*)
            logical :: lmelas
            character(len=16) :: nomcas
            character(len=24) :: fonoeu
          end subroutine mbilgl
        end interface
