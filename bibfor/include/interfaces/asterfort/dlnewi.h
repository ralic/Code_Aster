        interface
          subroutine dlnewi(result,force0,force1,lcrea,lamort,iinteg,&
     &neq,imat,masse,rigid,amort,dep0,vit0,acc0,fexte,famor,fliai,t0,&
     &nchar,nveca,liad,lifo,modele,mate,carele,charge,infoch,fomult,&
     &numedd,nume,solveu,criter,chondp,nondp,numrep)
            integer :: nondp
            character(len=8) :: result
            character(len=19) :: force0
            character(len=19) :: force1
            logical :: lcrea
            logical :: lamort
            integer :: iinteg
            integer :: neq
            integer :: imat(3)
            character(len=8) :: masse
            character(len=8) :: rigid
            character(len=8) :: amort
            real(kind=8) :: dep0(*)
            real(kind=8) :: vit0(*)
            real(kind=8) :: acc0(*)
            real(kind=8) :: fexte(*)
            real(kind=8) :: famor(*)
            real(kind=8) :: fliai(*)
            real(kind=8) :: t0
            integer :: nchar
            integer :: nveca
            integer :: liad(*)
            character(len=24) :: lifo(*)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: charge
            character(len=24) :: infoch
            character(len=24) :: fomult
            character(len=24) :: numedd
            integer :: nume
            character(len=19) :: solveu
            character(len=24) :: criter
            character(len=8) :: chondp(nondp)
            integer :: numrep
          end subroutine dlnewi
        end interface
