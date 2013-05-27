        interface
          subroutine nmprdc(method,numedd,depmoi,sddisc,numins,incest,&
     &depest)
            character(len=16) :: method(*)
            character(len=24) :: numedd
            character(len=19) :: depmoi
            character(len=19) :: sddisc
            integer :: numins
            character(len=19) :: incest
            character(len=19) :: depest
          end subroutine nmprdc
        end interface
