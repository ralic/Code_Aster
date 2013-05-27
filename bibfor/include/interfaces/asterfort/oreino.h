        interface
          subroutine oreino(noma,lnoeud,nbno,nori,next,coor,crit,prec,&
     &iera,ier)
            character(len=8) :: noma
            integer :: lnoeud(*)
            integer :: nbno
            integer :: nori
            integer :: next
            real(kind=8) :: coor(*)
            character(*) :: crit
            real(kind=8) :: prec
            integer :: iera
            integer :: ier
          end subroutine oreino
        end interface
