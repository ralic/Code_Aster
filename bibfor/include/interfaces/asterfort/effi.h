        interface
          subroutine effi(nomte,sigmtd,vf,dfds,jacp,sina,cosa,r,effint&
     &)
            character(len=16) :: nomte
            real(kind=8) :: sigmtd(*)
            real(kind=8) :: vf(*)
            real(kind=8) :: dfds(*)
            real(kind=8) :: jacp
            real(kind=8) :: sina
            real(kind=8) :: cosa
            real(kind=8) :: r
            real(kind=8) :: effint(*)
          end subroutine effi
        end interface
