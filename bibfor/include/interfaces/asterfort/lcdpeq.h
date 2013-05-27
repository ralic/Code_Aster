        interface
          subroutine lcdpeq(vind,vinf,comp,nbcomm,cpmono,nmat,nvi,sig,&
     &detot,epsd,materf,pgl)
            integer :: nvi
            integer :: nmat
            real(kind=8) :: vind(nvi)
            real(kind=8) :: vinf(nvi)
            character(len=16) :: comp(*)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: sig(6)
            real(kind=8) :: detot(*)
            real(kind=8) :: epsd(*)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: pgl(3,3)
          end subroutine lcdpeq
        end interface
