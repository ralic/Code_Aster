        interface
          subroutine pbflga(umoy,hmoy,rmoy,long,cf0,fsvr,icoq,imod,nbm&
     &,tcoef,s1,s2,lambda,kcalcu,condit,gamma)
            integer :: nbm
            real(kind=8) :: umoy
            real(kind=8) :: hmoy
            real(kind=8) :: rmoy
            real(kind=8) :: long
            real(kind=8) :: cf0
            real(kind=8) :: fsvr(7)
            integer :: icoq
            integer :: imod
            real(kind=8) :: tcoef(10,nbm)
            real(kind=8) :: s1
            real(kind=8) :: s2
            complex(kind=8) :: lambda(3)
            complex(kind=8) :: kcalcu(3,4)
            real(kind=8) :: condit(3)
            complex(kind=8) :: gamma(3)
          end subroutine pbflga
        end interface
