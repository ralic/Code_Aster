        interface
          subroutine rvechd(dim,epsi,ssch19,nbcp,nbco,nbsp,ror,rex,ma1&
     &,ma2,for,fex,n,ptadr,val)
            character(len=2) :: dim
            real(kind=8) :: epsi
            character(len=19) :: ssch19
            integer :: nbcp
            integer :: nbco
            integer :: nbsp
            real(kind=8) :: ror(*)
            real(kind=8) :: rex(*)
            integer :: ma1(*)
            integer :: ma2(*)
            integer :: for(*)
            integer :: fex(*)
            integer :: n
            integer :: ptadr
            real(kind=8) :: val(*)
          end subroutine rvechd
        end interface
