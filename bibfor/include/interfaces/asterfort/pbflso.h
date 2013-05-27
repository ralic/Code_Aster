        interface
          subroutine pbflso(umoy,rmoy,long,icoq,imod,nbm,rkip,tcoef,&
     &harm,lambda,kcalcu,passag,condit,gamma,d,ysol)
            integer :: nbm
            real(kind=8) :: umoy
            real(kind=8) :: rmoy
            real(kind=8) :: long
            integer :: icoq
            integer :: imod
            real(kind=8) :: rkip
            real(kind=8) :: tcoef(10,nbm)
            real(kind=8) :: harm(6)
            complex(kind=8) :: lambda(3)
            complex(kind=8) :: kcalcu(3,4)
            complex(kind=8) :: passag(3,3)
            real(kind=8) :: condit(3)
            complex(kind=8) :: gamma(3)
            real(kind=8) :: d(6)
            complex(kind=8) :: ysol(3,101)
          end subroutine pbflso
        end interface
