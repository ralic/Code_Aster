        interface
          subroutine cakg3d(option,result,modele,depla,thetai,mate,&
     &compor,nchar,lchar,symech,chfond,nnoff,basloc,courb,iord,ndeg,&
     &thlagr,glagr,thlag2,pair,ndimte,extim,time,nbprup,noprup,fiss,&
     &lmelas,nomcas,lmoda,puls,milieu,connex)
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
            character(len=24) :: basloc
            character(len=24) :: courb
            integer :: iord
            integer :: ndeg
            logical :: thlagr
            logical :: glagr
            logical :: thlag2
            logical :: pair
            integer :: ndimte
            logical :: extim
            real(kind=8) :: time
            integer :: nbprup
            character(len=16) :: noprup(*)
            character(len=8) :: fiss
            logical :: lmelas
            character(len=16) :: nomcas
            logical :: lmoda
            real(kind=8) :: puls
            logical :: milieu
            logical :: connex
          end subroutine cakg3d
        end interface
