        interface
          subroutine mmmres(noma,inst,defico,resoco,depplu,depdel,&
     &sddisc,veasse,cnsinr,cnsper)
            character(len=8) :: noma
            real(kind=8) :: inst(*)
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: depplu
            character(len=19) :: depdel
            character(len=19) :: sddisc
            character(len=19) :: veasse(*)
            character(len=19) :: cnsinr
            character(len=19) :: cnsper
          end subroutine mmmres
        end interface
