        interface
          subroutine aporth(sdappa,noma,defico,ndimg,posmam,coorpt,&
     &tau1m,tau2m)
            character(len=19) :: sdappa
            character(len=8) :: noma
            character(len=24) :: defico
            integer :: ndimg
            integer :: posmam
            real(kind=8) :: coorpt(3)
            real(kind=8) :: tau1m(3)
            real(kind=8) :: tau2m(3)
          end subroutine aporth
        end interface
