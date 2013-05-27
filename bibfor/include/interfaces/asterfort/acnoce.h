        interface
          subroutine acnoce(noma,type,liste,nb,coor,rc,xcen,tole,v1,&
     &ispv)
            character(len=8) :: noma
            character(len=4) :: type
            character(len=24) :: liste(*)
            integer :: nb
            real(kind=8) :: coor(*)
            real(kind=8) :: rc
            real(kind=8) :: xcen(3)
            real(kind=8) :: tole
            real(kind=8) :: v1(3)
            integer :: ispv
          end subroutine acnoce
        end interface
