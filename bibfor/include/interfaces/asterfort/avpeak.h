        interface
          subroutine avpeak(valaxe,nbvec,nbordr,pseuil,iflag,npoin,&
     &valpoi,valord)
            integer :: nbordr
            integer :: nbvec
            real(kind=8) :: valaxe(nbvec*nbordr)
            real(kind=8) :: pseuil
            integer :: iflag(nbvec)
            integer :: npoin(nbvec)
            real(kind=8) :: valpoi(nbvec*nbordr)
            integer :: valord(nbvec*nbordr)
          end subroutine avpeak
        end interface
