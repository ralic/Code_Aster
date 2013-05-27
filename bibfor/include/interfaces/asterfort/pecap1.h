        interface
          subroutine pecap1(chgeoz,tempez,ngi,lisgma,ct)
            integer :: ngi
            character(*) :: chgeoz
            character(*) :: tempez
            character(*) :: lisgma(ngi)
            real(kind=8) :: ct
          end subroutine pecap1
        end interface
