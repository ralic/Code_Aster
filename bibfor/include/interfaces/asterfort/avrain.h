        interface
          subroutine avrain(nbvec,nbordr,itrv,npic,pic,opic,fatsoc,&
     &ncycl,vmin,vmax,omin,omax)
            integer :: nbordr
            integer :: nbvec
            integer :: itrv(2*(nbordr+2))
            integer :: npic(nbvec)
            real(kind=8) :: pic(nbvec*(nbordr+2))
            integer :: opic(nbvec*(nbordr+2))
            real(kind=8) :: fatsoc
            integer :: ncycl(nbvec)
            real(kind=8) :: vmin(nbvec*(nbordr+2))
            real(kind=8) :: vmax(nbvec*(nbordr+2))
            integer :: omin(nbvec*(nbordr+2))
            integer :: omax(nbvec*(nbordr+2))
          end subroutine avrain
        end interface
