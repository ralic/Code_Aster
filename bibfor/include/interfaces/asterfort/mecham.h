        interface
          subroutine mecham(option,modele,cara,nh,chgeoz,chcara,chharz&
     &,iret)
            character(*) :: option
            character(*) :: modele
            character(*) :: cara
            integer :: nh
            character(*) :: chgeoz
            character(*) :: chcara(*)
            character(*) :: chharz
            integer :: iret
          end subroutine mecham
        end interface
