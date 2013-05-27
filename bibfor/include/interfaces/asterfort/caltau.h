        interface
          subroutine caltau(comp,ifa,is,sigf,fkooh,nfs,nsg,toutms,taus&
     &,mus,msns)
            integer :: nsg
            integer :: nfs
            character(len=16) :: comp(*)
            integer :: ifa
            integer :: is
            real(kind=8) :: sigf(6)
            real(kind=8) :: fkooh(6,6)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: taus
            real(kind=8) :: mus(6)
            real(kind=8) :: msns(3,3)
          end subroutine caltau
        end interface
