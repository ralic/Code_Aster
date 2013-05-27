        interface
          subroutine fetreo(reorth,alphan,nbi,irg,iter,nbreor,irp,&
     &k24fir,k24ddr,k24psr,gs,igsmkp,rmin,irh,infofe,ifm,nbproc,rang,&
     &k24irp,itps,nbreoi,option,lacsm)
            logical :: reorth
            real(kind=8) :: alphan
            integer :: nbi
            integer :: irg
            integer :: iter
            integer :: nbreor
            integer :: irp
            character(len=24) :: k24fir
            character(len=24) :: k24ddr
            character(len=24) :: k24psr
            logical :: gs
            logical :: igsmkp
            real(kind=8) :: rmin
            integer :: irh
            character(len=24) :: infofe
            integer :: ifm
            integer :: nbproc
            integer :: rang
            character(len=24) :: k24irp
            integer :: itps
            integer :: nbreoi
            integer :: option
            logical :: lacsm
          end subroutine fetreo
        end interface
