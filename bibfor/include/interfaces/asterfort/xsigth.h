        interface
          subroutine xsigth(ndim,nnop,nfh,igeom,lonch,inst,nbsig,sigth&
     &)
            integer :: ndim
            integer :: nnop
            integer :: nfh
            integer :: igeom
            integer :: lonch(10)
            real(kind=8) :: inst
            integer :: nbsig
            real(kind=8) :: sigth(*)
          end subroutine xsigth
        end interface
