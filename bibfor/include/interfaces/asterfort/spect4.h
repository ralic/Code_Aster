        interface
          function spect4(xx,y,xlc,vitn,rhoe,defm,nbp,im,jm)
            integer :: nbp
            real(kind=8) :: xx
            real(kind=8) :: y
            real(kind=8) :: xlc
            real(kind=8) :: vitn(nbp,*)
            real(kind=8) :: rhoe(nbp,*)
            real(kind=8) :: defm(nbp,*)
            integer :: im
            integer :: jm
            real(kind=8) :: spect4
          end function spect4
        end interface
