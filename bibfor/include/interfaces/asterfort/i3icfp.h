        interface
          subroutine i3icfp(epsi,fglo,sgt,floc1,floc2,floc3,typf,nbpt,&
     &iret)
            real(kind=8) :: epsi
            real(kind=8) :: fglo(3,*)
            real(kind=8) :: sgt(*)
            real(kind=8) :: floc1(*)
            real(kind=8) :: floc2(*)
            real(kind=8) :: floc3(*)
            integer :: typf
            integer :: nbpt
            integer :: iret
          end subroutine i3icfp
        end interface
