        interface
          subroutine nmeteo(result,sdimpr,sddisc,sdieto,force,numarc,&
     &instan,icham)
            character(len=8) :: result
            character(len=24) :: sdimpr
            character(len=19) :: sddisc
            character(len=24) :: sdieto
            logical :: force
            integer :: numarc
            real(kind=8) :: instan
            integer :: icham
          end subroutine nmeteo
        end interface
