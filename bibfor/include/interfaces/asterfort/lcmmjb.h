        interface
          subroutine lcmmjb(taur,materf,cpmono,ifa,nmat,nbcomm,dt,&
     &nuecou,nsfv,nsfa,ir,is,nbsys,nfs,nsg,hsr,vind,dy,iexp,expbp,itmax,&
     &toler,dgsdts,dksdts,dgrdbs,dkrdbs,iret)
            integer :: nsg
            integer :: nmat
            real(kind=8) :: taur
            real(kind=8) :: materf(nmat*2)
            character(len=24) :: cpmono(5*nmat+1)
            integer :: ifa
            integer :: nbcomm(nmat,3)
            real(kind=8) :: dt
            integer :: nuecou
            integer :: nsfv
            integer :: nsfa
            integer :: ir
            integer :: is
            integer :: nbsys
            integer :: nfs
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: vind(*)
            real(kind=8) :: dy(*)
            integer :: iexp
            real(kind=8) :: expbp(nsg)
            integer :: itmax
            real(kind=8) :: toler
            real(kind=8) :: dgsdts
            real(kind=8) :: dksdts
            real(kind=8) :: dgrdbs
            real(kind=8) :: dkrdbs
            integer :: iret
          end subroutine lcmmjb
        end interface
