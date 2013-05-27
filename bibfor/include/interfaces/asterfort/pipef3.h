        interface
          subroutine pipef3(ndim,nno,nddl,npg,lgpg,wref,vff,dfde,mate,&
     &geom,vim,ddepl,deplm,ddepl0,ddepl1,dtau,copilo,typmod)
            integer :: lgpg
            integer :: npg
            integer :: nddl
            integer :: nno
            integer :: ndim
            real(kind=8) :: wref(npg)
            real(kind=8) :: vff(nno,npg)
            real(kind=8) :: dfde(2,nno,npg)
            integer :: mate
            real(kind=8) :: geom(nddl)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: ddepl(nddl)
            real(kind=8) :: deplm(nddl)
            real(kind=8) :: ddepl0(nddl)
            real(kind=8) :: ddepl1(nddl)
            real(kind=8) :: dtau
            real(kind=8) :: copilo(5,npg)
            character(len=8) :: typmod(2)
          end subroutine pipef3
        end interface
