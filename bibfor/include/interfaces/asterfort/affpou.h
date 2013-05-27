        interface
          subroutine affpou(tmp,tmpf,fcx,nom,isec,ivar,car,ncar,val,&
     &tab,exp,nbo,ioc,ier)
            character(len=24) :: tmp
            character(len=24) :: tmpf
            character(len=8) :: fcx
            character(len=24) :: nom
            integer :: isec
            integer :: ivar
            character(len=8) :: car(*)
            integer :: ncar
            real(kind=8) :: val(*)
            character(len=8) :: tab(*)
            character(len=8) :: exp(*)
            integer :: nbo
            character(len=6) :: ioc
            integer :: ier
          end subroutine affpou
        end interface
