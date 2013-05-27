        interface
          subroutine atasmo(neq,az,apddl,apptr,numedz,ataz,basez,nblia&
     &,nmul,numatz)
            integer :: neq
            character(*) :: az
            integer :: apddl(*)
            integer :: apptr(*)
            character(*) :: numedz
            character(*) :: ataz
            character(*) :: basez
            integer :: nblia
            integer :: nmul
            character(*) :: numatz
          end subroutine atasmo
        end interface
