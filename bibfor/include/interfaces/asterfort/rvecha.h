        interface
          subroutine rvecha(dim,epsi,ssch19,nbcp,nbco,nbsp,ror,rex,ma1&
     &,for,fex,n,ptadr,val)
            character(len=2) :: dim
            real(kind=8) :: epsi
            character(len=19) :: ssch19
            integer :: nbcp
            integer :: nbco
            integer :: nbsp
            real(kind=8) :: ror(*)
            real(kind=8) :: rex(*)
            integer :: ma1(*)
            integer :: for(*)
            integer :: fex(*)
            integer :: n
            integer :: ptadr
            real(kind=8) :: val(*)
          end subroutine rvecha
        end interface
