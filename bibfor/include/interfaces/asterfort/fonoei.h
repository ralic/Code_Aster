        interface
          subroutine fonoei(ndim,dt,fnoevo,dimdef,dimcon,yamec,yap1,&
     &yap2,yate,addeme,addep1,addep2,addete,addlh1,adcome,adcp11,adcp12,&
     &adcp21,adcp22,adcote,adcop1,adcop2,nbpha1,nbpha2,congem,r)
            integer :: dimcon
            integer :: dimdef
            integer :: ndim
            real(kind=8) :: dt
            logical :: fnoevo
            integer :: yamec
            integer :: yap1
            integer :: yap2
            integer :: yate
            integer :: addeme
            integer :: addep1
            integer :: addep2
            integer :: addete
            integer :: addlh1
            integer :: adcome
            integer :: adcp11
            integer :: adcp12
            integer :: adcp21
            integer :: adcp22
            integer :: adcote
            integer :: adcop1
            integer :: adcop2
            integer :: nbpha1
            integer :: nbpha2
            real(kind=8) :: congem(dimcon)
            real(kind=8) :: r(dimdef)
          end subroutine fonoei
        end interface
