        interface
          subroutine recpar(neq,dti,dtmax,vmin,vvar,cmp,cdp,dtmin,nper&
     &,nrmax)
            integer :: neq
            real(kind=8) :: dti
            real(kind=8) :: dtmax
            real(kind=8) :: vmin(*)
            character(len=8) :: vvar
            real(kind=8) :: cmp
            real(kind=8) :: cdp
            real(kind=8) :: dtmin
            integer :: nper
            integer :: nrmax
          end subroutine recpar
        end interface
