        interface
          subroutine dinonc(nomte,icodre,valre,klv,raide,nbpar,param,&
     &nploi,okdire)
            integer :: nbpar
            character(len=16) :: nomte
            integer :: icodre(*)
            real(kind=8) :: valre(*)
            real(kind=8) :: klv(*)
            real(kind=8) :: raide(*)
            real(kind=8) :: param(6,nbpar)
            integer :: nploi
            logical :: okdire(6)
          end subroutine dinonc
        end interface
