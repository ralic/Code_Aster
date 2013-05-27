        interface
          subroutine calcmm(nbcomm,cpmono,nmat,pgl,nfs,nsg,toutms,comp&
     &,nvi,vind,irota)
            integer :: nsg
            integer :: nfs
            integer :: nmat
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: toutms(nfs,nsg,6)
            character(len=16) :: comp(*)
            integer :: nvi
            real(kind=8) :: vind(*)
            integer :: irota
          end subroutine calcmm
        end interface
