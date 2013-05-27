        interface
          subroutine zzcalb(igr,iel,npg,nno,wi,desc,sig,x,y,xmin,xmax,&
     &ymin,ymax,f)
            integer :: igr
            integer :: iel
            integer :: npg
            integer :: nno
            real(kind=8) :: wi(1)
            integer :: desc(1)
            real(kind=8) :: sig(1)
            real(kind=8) :: x(1)
            real(kind=8) :: y(1)
            real(kind=8) :: xmin
            real(kind=8) :: xmax
            real(kind=8) :: ymin
            real(kind=8) :: ymax
            real(kind=8) :: f(9,4)
          end subroutine zzcalb
        end interface
