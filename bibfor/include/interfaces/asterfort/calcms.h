        interface
          subroutine calcms(nbphas,nbcomm,cpmono,nmat,pgl2,coeft,&
     &angmas,nfs,nsg,toutms)
            integer :: nsg
            integer :: nfs
            integer :: nmat
            integer :: nbphas
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: pgl2(3,3)
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: angmas(3)
            real(kind=8) :: toutms(nbphas,nfs,nsg,7)
          end subroutine calcms
        end interface
