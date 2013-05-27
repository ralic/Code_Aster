        interface
          subroutine meacmv(modele,mate,carele,fomult,lischa,partps,&
     &numedd,assmat,solveu,vecass,matass,maprec,cnchci,base,compor)
            character(len=24) :: modele
            character(*) :: mate
            character(len=24) :: carele
            character(len=24) :: fomult
            character(len=19) :: lischa
            real(kind=8) :: partps(*)
            character(len=24) :: numedd
            logical :: assmat
            character(len=19) :: solveu
            character(len=19) :: vecass
            character(len=19) :: matass
            character(len=19) :: maprec
            character(len=24) :: cnchci
            character(len=1) :: base
            character(len=24) :: compor
          end subroutine meacmv
        end interface
