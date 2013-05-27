        interface
          subroutine norton(nvi,vini,coeft,nmat,sigi,dvin,iret)
            integer :: nmat
            integer :: nvi
            real(kind=8) :: vini(nvi)
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: sigi(6)
            real(kind=8) :: dvin(nvi)
            integer :: iret
          end subroutine norton
        end interface
