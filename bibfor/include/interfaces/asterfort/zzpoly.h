        interface
          subroutine zzpoly(nno,ino,xino,yino,sig,b)
            integer :: nno
            integer :: ino
            real(kind=8) :: xino
            real(kind=8) :: yino
            real(kind=8) :: sig(1)
            real(kind=8) :: b(9,4)
          end subroutine zzpoly
        end interface
