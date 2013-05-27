        interface
          subroutine pbflu0(rhof,hmoy,rmoy,long,icoq,imod,nbm,rkip,&
     &tcoef,d)
            integer :: nbm
            real(kind=8) :: rhof
            real(kind=8) :: hmoy
            real(kind=8) :: rmoy
            real(kind=8) :: long
            integer :: icoq
            integer :: imod
            real(kind=8) :: rkip
            real(kind=8) :: tcoef(10,nbm)
            real(kind=8) :: d(6)
          end subroutine pbflu0
        end interface
