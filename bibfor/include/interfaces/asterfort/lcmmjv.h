        interface
          subroutine lcmmjv(comp,nmat,cpmono,nbfsys,irota,itbint,nfs,&
     &nsg,hsr)
            integer :: nsg
            integer :: nmat
            character(len=16) :: comp(*)
            character(len=24) :: cpmono(5*nmat+1)
            integer :: nbfsys
            integer :: irota
            integer :: itbint
            integer :: nfs
            real(kind=8) :: hsr(nsg,nsg)
          end subroutine lcmmjv
        end interface
