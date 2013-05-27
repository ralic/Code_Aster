        interface
          subroutine pjspma(corres,cham1,cham2,prol0,ligre2,noca,base,&
     &iret)
            character(len=16) :: corres
            character(len=19) :: cham1
            character(len=19) :: cham2
            character(len=8) :: prol0
            character(len=19) :: ligre2
            character(len=8) :: noca
            character(len=1) :: base
            integer :: iret
          end subroutine pjspma
        end interface
