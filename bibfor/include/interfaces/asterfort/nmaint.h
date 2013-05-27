        interface
          subroutine nmaint(numedd,fonact,defico,veasse,vefint,cnfint,&
     &sdnume)
            character(len=24) :: numedd
            integer :: fonact(*)
            character(len=24) :: defico
            character(len=19) :: veasse(*)
            character(len=19) :: vefint
            character(len=19) :: cnfint
            character(len=19) :: sdnume
          end subroutine nmaint
        end interface
