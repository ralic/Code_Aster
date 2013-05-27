        interface
          subroutine affbar(tmp,tmpf,fcx,nommai,isec,car,val,exp,nbo,&
     &kioc,ier)
            character(len=24) :: tmp
            character(len=24) :: tmpf
            character(len=8) :: fcx
            character(len=8) :: nommai
            integer :: isec
            character(len=8) :: car(*)
            real(kind=8) :: val(*)
            character(len=8) :: exp(*)
            integer :: nbo
            character(len=6) :: kioc
            integer :: ier
          end subroutine affbar
        end interface
