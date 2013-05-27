        interface
          subroutine xmrlst(jcesd,jcesv,jcesl,noma,posma,coor,lst)
            integer :: jcesd(10)
            integer :: jcesv(10)
            integer :: jcesl(10)
            character(len=8) :: noma
            integer :: posma
            real(kind=8) :: coor(3)
            real(kind=8) :: lst
          end subroutine xmrlst
        end interface
