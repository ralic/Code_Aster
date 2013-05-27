        interface
          subroutine rcfonc(quest,ktrac,jprol,jvale,nbvale,sigy,e,nu,p&
     &,rp,rprim,airerp,sieleq,dp)
            character(len=1) :: quest
            integer :: ktrac
            integer :: jprol
            integer :: jvale
            integer :: nbvale
            real(kind=8) :: sigy
            real(kind=8) :: e
            real(kind=8) :: nu
            real(kind=8) :: p
            real(kind=8) :: rp
            real(kind=8) :: rprim
            real(kind=8) :: airerp
            real(kind=8) :: sieleq
            real(kind=8) :: dp
          end subroutine rcfonc
        end interface
