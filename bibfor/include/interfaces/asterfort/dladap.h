        interface
          subroutine dladap(result,tinit,lcrea,lamort,neq,imat,masse,&
     &rigid,amort,dep0,vit0,acc0,fexte,famor,fliai,nchar,nveca,liad,lifo&
     &,modele,mate,carele,charge,infoch,fomult,numedd,nume,solveu,numrep&
     &)
            character(len=8) :: result
            real(kind=8) :: tinit
            logical :: lcrea
            logical :: lamort
            integer :: neq
            integer :: imat(*)
            character(len=8) :: masse
            character(len=8) :: rigid
            character(len=8) :: amort
            real(kind=8) :: dep0(*)
            real(kind=8) :: vit0(*)
            real(kind=8) :: acc0(*)
            real(kind=8) :: fexte(*)
            real(kind=8) :: famor(*)
            real(kind=8) :: fliai(*)
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
            integer :: numrep
          end subroutine dladap
        end interface
