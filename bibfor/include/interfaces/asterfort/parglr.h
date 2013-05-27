        interface
          subroutine parglr(nlit,elb,ea,nua,liner,omx,omy,rx,ry,hh,&
     &bn11,bn12,bn22,bn33,bm11,bm12,bm22,bc11,bc22)
            integer :: nlit
            real(kind=8) :: elb(*)
            real(kind=8) :: ea(*)
            real(kind=8) :: nua(*)
            real(kind=8) :: liner(*)
            real(kind=8) :: omx(*)
            real(kind=8) :: omy(*)
            real(kind=8) :: rx(*)
            real(kind=8) :: ry(*)
            real(kind=8) :: hh
            real(kind=8) :: bn11
            real(kind=8) :: bn12
            real(kind=8) :: bn22
            real(kind=8) :: bn33
            real(kind=8) :: bm11
            real(kind=8) :: bm12
            real(kind=8) :: bm22
            real(kind=8) :: bc11
            real(kind=8) :: bc22
          end subroutine parglr
        end interface
