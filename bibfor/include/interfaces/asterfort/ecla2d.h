        interface
          subroutine ecla2d(nomte,elrefa,fapg,npg,npoini,nterm1,nsomm1&
     &,csomm1,tyma,nbno2,connx,mxnbn2,mxnbpi,mxnbte,mxnbse,nbsel,corsel)
            integer :: mxnbse
            integer :: mxnbte
            integer :: mxnbpi
            integer :: mxnbn2
            character(len=16) :: nomte
            character(len=8) :: elrefa
            character(len=8) :: fapg
            integer :: npg
            integer :: npoini
            integer :: nterm1(mxnbpi)
            integer :: nsomm1(mxnbpi,mxnbte)
            real(kind=8) :: csomm1(mxnbpi,mxnbte)
            integer :: tyma(mxnbse)
            integer :: nbno2(mxnbse)
            integer :: connx(mxnbn2,mxnbse)
            integer :: nbsel
            integer :: corsel(mxnbse)
          end subroutine ecla2d
        end interface
