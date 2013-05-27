        interface
          subroutine oriem0(kdim,type,coor,lino1,nbno1,lino2,nbno2,&
     &lino3,nbno3,ier,imai)
            character(len=2) :: kdim
            character(len=8) :: type
            real(kind=8) :: coor(*)
            integer :: lino1(*)
            integer :: nbno1
            integer :: lino2(*)
            integer :: nbno2
            integer :: lino3(*)
            integer :: nbno3
            integer :: ier
            integer :: imai
          end subroutine oriem0
        end interface
