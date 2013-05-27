        interface
          subroutine pbflui(umoy,hmoy,rmoy,long,cf0,mcf0,fsvr,icoq,&
     &imod,nbm,rki,tcoef,s1,s2,ysol)
            integer :: nbm
            real(kind=8) :: umoy
            real(kind=8) :: hmoy
            real(kind=8) :: rmoy
            real(kind=8) :: long
            real(kind=8) :: cf0
            real(kind=8) :: mcf0
            real(kind=8) :: fsvr(7)
            integer :: icoq
            integer :: imod
            real(kind=8) :: rki
            real(kind=8) :: tcoef(10,nbm)
            real(kind=8) :: s1
            real(kind=8) :: s2
            complex(kind=8) :: ysol(3,101)
          end subroutine pbflui
        end interface
