        interface
          subroutine dltlec(result,modele,numedd,materi,mate,carael,&
     &carele,imat,masse,rigid,amort,lamort,nchar,nveca,lischa,charge,&
     &infoch,fomult,iaadve,ialifo,nondp,iondp,solveu,iinteg,t0,nume,&
     &baseno,numrep)
            character(len=8) :: result
            character(len=24) :: modele
            character(len=24) :: numedd
            character(len=8) :: materi
            character(len=24) :: mate
            character(len=8) :: carael
            character(len=24) :: carele
            integer :: imat(3)
            character(len=8) :: masse
            character(len=8) :: rigid
            character(len=8) :: amort
            logical :: lamort
            integer :: nchar
            integer :: nveca
            character(len=19) :: lischa
            character(len=24) :: charge
            character(len=24) :: infoch
            character(len=24) :: fomult
            integer :: iaadve
            integer :: ialifo
            integer :: nondp
            integer :: iondp
            character(len=19) :: solveu
            integer :: iinteg
            real(kind=8) :: t0
            integer :: nume
            character(len=8) :: baseno
            integer :: numrep
          end subroutine dltlec
        end interface
