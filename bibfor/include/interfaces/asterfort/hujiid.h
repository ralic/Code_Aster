        interface
          subroutine hujiid(mod,mater,indi,deps,i1e,yd,vind,dy,loop,&
     &dsig,bnews,mtrac,iret)
            character(len=8) :: mod
            real(kind=8) :: mater(22,2)
            integer :: indi(7)
            real(kind=8) :: deps(6)
            real(kind=8) :: i1e
            real(kind=8) :: yd(18)
            real(kind=8) :: vind(*)
            real(kind=8) :: dy(18)
            logical :: loop
            real(kind=8) :: dsig(6)
            logical :: bnews(3)
            logical :: mtrac
            integer :: iret
          end subroutine hujiid
        end interface
