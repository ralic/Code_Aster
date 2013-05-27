        interface
          subroutine vefpme(modele,carele,mate,chargz,infchz,partps,&
     &templu,lvechz,ligrez)
            character(*) :: modele
            character(*) :: carele
            character(*) :: mate
            character(*) :: chargz
            character(*) :: infchz
            real(kind=8) :: partps(*)
            character(*) :: templu
            character(*) :: lvechz
            character(*) :: ligrez
          end subroutine vefpme
        end interface
