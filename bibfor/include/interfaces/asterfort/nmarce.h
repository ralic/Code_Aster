        interface
          subroutine nmarce(sdieto,result,sdimpr,sddisc,instan,numarc,&
     &force)
            character(len=24) :: sdieto
            character(len=8) :: result
            character(len=24) :: sdimpr
            character(len=19) :: sddisc
            real(kind=8) :: instan
            integer :: numarc
            logical :: force
          end subroutine nmarce
        end interface
