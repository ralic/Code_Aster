        interface
          subroutine calkce(nno,ndim,kbp,kbb,pm,dp,kce,rce)
            integer :: ndim
            integer :: nno
            real(kind=8) :: kbp(ndim,nno)
            real(kind=8) :: kbb(ndim,ndim)
            real(kind=8) :: pm(nno)
            real(kind=8) :: dp(nno)
            real(kind=8) :: kce(nno,nno)
            real(kind=8) :: rce(nno)
          end subroutine calkce
        end interface
