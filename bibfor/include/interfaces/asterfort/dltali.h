        interface
          subroutine dltali(neq,result,imat,masse,rigid,liad,lifo,&
     &nchar,nveca,lcrea,lprem,lamort,t0,mate,carele,charge,infoch,fomult&
     &,modele,numedd,nume,solveu,criter,dep0,vit0,acc0,fexte0,famor0,&
     &fliai0,baseno,tabwk,force0,force1)
            integer :: neq
            character(len=8) :: result
            integer :: imat(3)
            character(len=8) :: masse
            character(len=8) :: rigid
            integer :: liad(*)
            character(len=24) :: lifo(*)
            integer :: nchar
            integer :: nveca
            logical :: lcrea
            logical :: lprem
            logical :: lamort
            real(kind=8) :: t0
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: charge
            character(len=24) :: infoch
            character(len=24) :: fomult
            character(len=24) :: modele
            character(len=24) :: numedd
            integer :: nume
            character(len=19) :: solveu
            character(len=24) :: criter
            real(kind=8) :: dep0(*)
            real(kind=8) :: vit0(*)
            real(kind=8) :: acc0(*)
            real(kind=8) :: fexte0(*)
            real(kind=8) :: famor0(*)
            real(kind=8) :: fliai0(*)
            character(len=8) :: baseno
            real(kind=8) :: tabwk(*)
            character(len=19) :: force0
            character(len=19) :: force1
          end subroutine dltali
        end interface
