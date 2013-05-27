        interface
          subroutine ntdoth(modele,mate,carele,fomult,matcst,coecst,&
     &infcha,result,nuord)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: fomult
            logical :: matcst
            logical :: coecst
            character(len=19) :: infcha
            character(len=8) :: result
            integer :: nuord
          end subroutine ntdoth
        end interface
