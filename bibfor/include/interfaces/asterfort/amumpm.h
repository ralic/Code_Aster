        interface
          subroutine amumpm(ldist,kxmps,kmonit,impr,ifmump,klag2,type,&
     &lmd,epsmat,ktypr,lpreco)
            logical :: ldist
            integer :: kxmps
            character(len=24) :: kmonit(12)
            character(len=14) :: impr
            integer :: ifmump
            character(len=4) :: klag2
            character(len=1) :: type
            logical :: lmd
            real(kind=8) :: epsmat
            character(len=8) :: ktypr
            logical :: lpreco
          end subroutine amumpm
        end interface
