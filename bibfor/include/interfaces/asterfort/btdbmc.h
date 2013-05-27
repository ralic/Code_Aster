        interface
          subroutine btdbmc(b,d,jacob,ndim,nno,nbsig,phenoz,btdb)
            integer :: nbsig
            real(kind=8) :: b(nbsig,*)
            real(kind=8) :: d(nbsig,*)
            real(kind=8) :: jacob
            integer :: ndim
            integer :: nno
            character(*) :: phenoz
            real(kind=8) :: btdb(81,81)
          end subroutine btdbmc
        end interface
