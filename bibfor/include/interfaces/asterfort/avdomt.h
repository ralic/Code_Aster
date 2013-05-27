        interface
          subroutine avdomt(nbvec,nbordr,ncycl,domel,domtot)
            integer :: nbordr
            integer :: nbvec
            integer :: ncycl(nbvec)
            real(kind=8) :: domel(nbvec*nbordr)
            real(kind=8) :: domtot(nbvec)
          end subroutine avdomt
        end interface
