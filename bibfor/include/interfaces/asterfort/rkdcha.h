        interface
          subroutine rkdcha(nvi,vini,coeft,nmat,sigi,dvin)
            integer :: nmat
            integer :: nvi
            real(kind=8) :: vini(*)
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: sigi(6)
            real(kind=8) :: dvin(*)
          end subroutine rkdcha
        end interface
