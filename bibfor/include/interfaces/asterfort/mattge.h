        interface
          subroutine mattge(nomte,dtild,sina,cosa,r,jacp,vf,dfds,&
     &rtangi)
            character(len=16) :: nomte
            real(kind=8) :: dtild(5,5)
            real(kind=8) :: sina
            real(kind=8) :: cosa
            real(kind=8) :: r
            real(kind=8) :: jacp
            real(kind=8) :: vf(*)
            real(kind=8) :: dfds(*)
            real(kind=8) :: rtangi(9,9)
          end subroutine mattge
        end interface
