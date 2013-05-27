        interface
          subroutine xdelt2(elp,nno,ndim,ksi,ptint,tabco,jtabls,ipp,ip&
     &,delta)
            integer :: ndim
            integer :: nno
            character(len=8) :: elp
            real(kind=8) :: ksi(ndim)
            real(kind=8) :: ptint(*)
            real(kind=8) :: tabco(*)
            integer :: jtabls
            integer :: ipp
            integer :: ip
            real(kind=8) :: delta(ndim)
          end subroutine xdelt2
        end interface
