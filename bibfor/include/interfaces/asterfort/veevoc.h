        interface
          subroutine veevoc(nomo,mate,carele,varplu,lischa,partps,&
     &vecele)
            character(len=8) :: nomo
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=19) :: varplu
            character(len=19) :: lischa
            real(kind=8) :: partps(3)
            character(len=19) :: vecele
          end subroutine veevoc
        end interface
