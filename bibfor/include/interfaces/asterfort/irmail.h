        interface
          subroutine irmail(form,ifi,versio,noma,lmod,nomo,nive,infmai&
     &,formar)
            character(*) :: form
            integer :: ifi
            integer :: versio
            character(len=8) :: noma
            logical :: lmod
            character(len=8) :: nomo
            integer :: nive
            integer :: infmai
            character(len=16) :: formar
          end subroutine irmail
        end interface
