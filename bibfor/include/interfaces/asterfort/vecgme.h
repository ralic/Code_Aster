        interface
          subroutine vecgme(modele,carele,mate,charge,infcha,instap,&
     &depmoz,depdez,vecelz,instam,compor,carcri,ligrez,vitez)
            character(len=24) :: modele
            character(len=24) :: carele
            character(*) :: mate
            character(len=24) :: charge
            character(len=24) :: infcha
            real(kind=8) :: instap
            character(*) :: depmoz
            character(*) :: depdez
            character(*) :: vecelz
            real(kind=8) :: instam
            character(len=24) :: compor
            character(len=24) :: carcri
            character(*) :: ligrez
            character(*) :: vitez
          end subroutine vecgme
        end interface
