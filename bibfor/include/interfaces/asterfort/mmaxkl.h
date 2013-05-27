        interface
          subroutine mmaxkl(latabl,modele,thetai,mate,compor,ncha,&
     &symech,chfond,nnoff,basloc,courb,ndeg,thlagr,glagr,thlag2,pair,&
     &ndimte,nbprup,noprup,fiss,lonvec,ivec,vchar,resuco,lmelas,lncas,&
     &lord,milieu,connex)
            integer :: lonvec
            character(len=8) :: latabl
            character(len=8) :: modele
            character(len=8) :: thetai
            character(len=24) :: mate
            character(len=24) :: compor
            integer :: ncha
            character(len=8) :: symech
            character(len=24) :: chfond
            integer :: nnoff
            character(len=24) :: basloc
            character(len=24) :: courb
            integer :: ndeg
            logical :: thlagr
            logical :: glagr
            logical :: thlag2
            logical :: pair
            integer :: ndimte
            integer :: nbprup
            character(len=16) :: noprup(*)
            character(len=8) :: fiss
            integer :: ivec
            character(len=19) :: vchar
            character(len=8) :: resuco
            logical :: lmelas
            logical :: lncas
            logical :: lord(lonvec)
            logical :: milieu
            logical :: connex
          end subroutine mmaxkl
        end interface
