        interface
          subroutine projmd(testc,np1,nb1,nb2,mat,vg,vd,matpr,mtmp1,&
     &mtmp2)
            integer :: np1
            integer :: testc
            integer :: nb1
            integer :: nb2
            real(kind=8) :: mat(np1,*)
            real(kind=8) :: vg(np1,*)
            real(kind=8) :: vd(np1,*)
            real(kind=8) :: matpr(*)
            real(kind=8) :: mtmp1(np1,*)
            real(kind=8) :: mtmp2(np1,*)
          end subroutine projmd
        end interface
