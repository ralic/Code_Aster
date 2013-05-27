        interface
          subroutine rkdhay(mod,nvi,vini,coeft,nmat,sigi,dvin,iret)
            integer :: nmat
            integer :: nvi
            character(len=8) :: mod
            real(kind=8) :: vini(nvi)
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: sigi(*)
            real(kind=8) :: dvin(nvi)
            integer :: iret
          end subroutine rkdhay
        end interface
