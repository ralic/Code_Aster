        interface
          subroutine rbph01(trange,nbcham,typea,itresu,nfonct,basemo,&
     &typref,typbas,tousno,multap)
            character(len=19) :: trange
            integer :: nbcham
            character(len=16) :: typea(*)
            integer :: itresu(*)
            integer :: nfonct
            character(len=8) :: basemo
            character(len=19) :: typref(*)
            character(len=16) :: typbas(*)
            logical :: tousno
            logical :: multap
          end subroutine rbph01
        end interface
