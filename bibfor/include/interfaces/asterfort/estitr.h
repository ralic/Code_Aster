        interface
          subroutine estitr(nbm,amori,masgi,eps,ttrans,npf,npfmax,text&
     &,ier)
            integer :: nbm
            real(kind=8) :: amori(*)
            real(kind=8) :: masgi(*)
            real(kind=8) :: eps
            real(kind=8) :: ttrans
            integer :: npf
            integer :: npfmax
            real(kind=8) :: text(*)
            integer :: ier
          end subroutine estitr
        end interface
