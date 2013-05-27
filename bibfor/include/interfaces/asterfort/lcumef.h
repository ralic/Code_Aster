        interface
          subroutine lcumef(option,dep,depm,an,bn,cn,epsm,epsrm,epsrp,&
     &depsi,epsfm,sigi,nstrs,sigt)
            character(len=16) :: option(2)
            real(kind=8) :: dep(6,6)
            real(kind=8) :: depm(6,6)
            real(kind=8) :: an(6)
            real(kind=8) :: bn(6,6)
            real(kind=8) :: cn(6,6)
            real(kind=8) :: epsm(6)
            real(kind=8) :: epsrm
            real(kind=8) :: epsrp
            real(kind=8) :: depsi(6)
            real(kind=8) :: epsfm(6)
            real(kind=8) :: sigi(6)
            integer :: nstrs
            real(kind=8) :: sigt(6)
          end subroutine lcumef
        end interface
