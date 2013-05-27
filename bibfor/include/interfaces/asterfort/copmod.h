        interface
          subroutine copmod(basemo,champ,neq,numer,nbmode,typc,bmodr,&
     &bmodz)
            integer :: nbmode
            integer :: neq
            character(len=8) :: basemo
            character(*) :: champ
            character(*) :: numer
            character(len=1) :: typc
            real(kind=8) :: bmodr(neq*nbmode)
            complex(kind=8) :: bmodz(neq*nbmode)
          end subroutine copmod
        end interface
