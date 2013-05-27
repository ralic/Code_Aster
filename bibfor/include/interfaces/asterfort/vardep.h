        interface
          subroutine vardep(nbnl,dep,dep0,tconf2,tconf1,ivar,dt0,toln,&
     &tolc,tolv)
            integer :: nbnl
            real(kind=8) :: dep(3,*)
            real(kind=8) :: dep0(3,*)
            real(kind=8) :: tconf2(4,*)
            real(kind=8) :: tconf1(4,*)
            integer :: ivar
            real(kind=8) :: dt0
            real(kind=8) :: toln
            real(kind=8) :: tolc
            real(kind=8) :: tolv
          end subroutine vardep
        end interface
