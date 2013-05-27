        interface
          subroutine lcrkin(ndim,opt,comp,materf,nbcomm,cpmono,nmat,&
     &mod,nvi,sigd,sigf,vind,vinf,nbphas,iret)
            integer :: nmat
            integer :: ndim
            character(len=16) :: opt
            character(len=16) :: comp(*)
            real(kind=8) :: materf(nmat,2)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            character(len=8) :: mod
            integer :: nvi
            real(kind=8) :: sigd(*)
            real(kind=8) :: sigf(*)
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            integer :: nbphas
            integer :: iret
          end subroutine lcrkin
        end interface
