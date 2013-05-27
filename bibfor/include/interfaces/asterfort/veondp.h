        interface
          subroutine veondp(modele,mate,sddyna,temps,vecelz)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=19) :: sddyna
            real(kind=8) :: temps
            character(*) :: vecelz
          end subroutine veondp
        end interface
