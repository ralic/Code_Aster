        interface
          subroutine cfresu(noma,numins,inst,sddisc,defico,resoco,&
     &depplu,depdel,ddepla,cnsinr,cnsper)
            character(len=8) :: noma
            integer :: numins
            real(kind=8) :: inst(*)
            character(len=19) :: sddisc
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: depplu
            character(len=19) :: depdel
            character(len=19) :: ddepla
            character(len=19) :: cnsinr
            character(len=19) :: cnsper
          end subroutine cfresu
        end interface
