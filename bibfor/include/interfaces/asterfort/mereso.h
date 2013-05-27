        interface
          subroutine mereso(result,modele,mate,carele,fomult,lischa,&
     &itps,partps,numedd,vecass,assmat,solveu,matass,maprec,base,compor)
            character(len=8) :: result
            character(len=24) :: modele
            character(*) :: mate
            character(len=24) :: carele
            character(len=24) :: fomult
            character(len=19) :: lischa
            integer :: itps
            real(kind=8) :: partps(*)
            character(len=24) :: numedd
            character(len=19) :: vecass
            logical :: assmat
            character(len=19) :: solveu
            character(len=19) :: matass
            character(len=19) :: maprec
            character(len=1) :: base
            character(len=24) :: compor
          end subroutine mereso
        end interface
