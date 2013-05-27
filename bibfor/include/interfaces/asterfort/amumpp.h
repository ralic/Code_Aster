        interface
          subroutine amumpp(option,nbsol,kxmps,ldist,type,impr,ifmump,&
     &eli2lg,rsolu,csolu,vcine,prepos,lpreco)
            integer :: option
            integer :: nbsol
            integer :: kxmps
            logical :: ldist
            character(len=1) :: type
            character(len=14) :: impr
            integer :: ifmump
            logical :: eli2lg
            real(kind=8) :: rsolu(*)
            complex(kind=8) :: csolu(*)
            character(len=19) :: vcine
            logical :: prepos
            logical :: lpreco
          end subroutine amumpp
        end interface
