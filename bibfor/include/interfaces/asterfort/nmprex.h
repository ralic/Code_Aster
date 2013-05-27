        interface
          subroutine nmprex(numedd,depmoi,solalg,sddisc,numins,incest,&
     &depest)
            character(len=24) :: numedd
            character(len=19) :: depmoi
            character(len=19) :: solalg(*)
            character(len=19) :: sddisc
            integer :: numins
            character(len=19) :: incest
            character(len=19) :: depest
          end subroutine nmprex
        end interface
