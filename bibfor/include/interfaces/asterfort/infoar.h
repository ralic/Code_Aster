        interface
          subroutine infoar(ndim,ar,ia,j,geom,lsn,a,b,m,lsna,lsnb,lsnm&
     &)
            integer :: ndim
            integer :: ar(12,3)
            integer :: ia
            integer :: j
            real(kind=8) :: geom(*)
            real(kind=8) :: lsn(*)
            real(kind=8) :: a(ndim)
            real(kind=8) :: b(ndim)
            real(kind=8) :: m(ndim)
            real(kind=8) :: lsna
            real(kind=8) :: lsnb
            real(kind=8) :: lsnm
          end subroutine infoar
        end interface
