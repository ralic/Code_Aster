        interface
          subroutine nmrefe(modele,compor,mate,carele,depmoi,parcon,&
     &vecelz)
            character(len=24) :: modele
            character(len=24) :: compor
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=19) :: depmoi
            real(kind=8) :: parcon(*)
            character(*) :: vecelz
          end subroutine nmrefe
        end interface
