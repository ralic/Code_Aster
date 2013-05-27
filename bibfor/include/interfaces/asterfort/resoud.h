        interface
          subroutine resoud(matass,matpre,solveu,chcine,nsecm,chsecm,&
     &chsolu,base,rsolu,csolu,criter,prepos,istop,iret)
            character(*) :: matass
            character(*) :: matpre
            character(*) :: solveu
            character(*) :: chcine
            integer :: nsecm
            character(*) :: chsecm
            character(*) :: chsolu
            character(*) :: base
            real(kind=8) :: rsolu(*)
            complex(kind=8) :: csolu(*)
            character(*) :: criter
            logical :: prepos
            integer :: istop
            integer :: iret
          end subroutine resoud
        end interface
