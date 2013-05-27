        interface
          subroutine nmdata(result,modele,mate,carele,compor,lischa,&
     &solveu,method,parmet,parcri,parcon,carcri,sddyna,sdpost,sderro,&
     &sdener,sdcriq,sdimpr)
            character(len=8) :: result
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: compor
            character(len=19) :: lischa
            character(len=19) :: solveu
            character(len=16) :: method(*)
            real(kind=8) :: parmet(*)
            real(kind=8) :: parcri(*)
            real(kind=8) :: parcon(*)
            character(len=24) :: carcri
            character(len=19) :: sddyna
            character(len=19) :: sdpost
            character(len=24) :: sderro
            character(len=19) :: sdener
            character(len=24) :: sdcriq
            character(len=24) :: sdimpr
          end subroutine nmdata
        end interface
