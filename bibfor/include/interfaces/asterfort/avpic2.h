        interface
          subroutine avpic2(method,nbvec,nbordr,rtrv,itrv,npoin,valpoi&
     &,valord,npic,pic,ordpic)
            integer :: nbordr
            integer :: nbvec
            character(len=8) :: method
            real(kind=8) :: rtrv(nbordr+2)
            integer :: itrv(2*(nbordr+2))
            integer :: npoin(nbvec)
            real(kind=8) :: valpoi(nbvec*nbordr)
            integer :: valord(nbvec*nbordr)
            integer :: npic(nbvec)
            real(kind=8) :: pic(nbvec*(nbordr+2))
            integer :: ordpic(nbvec*(nbordr+2))
          end subroutine avpic2
        end interface
