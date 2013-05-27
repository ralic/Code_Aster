        interface
          function spect3(x,a,b,f,tol,coeff,xlc,vitn,defm,rhoe,nbp,im,&
     &jm)
            integer :: nbp
            real(kind=8) :: x
            real(kind=8) :: a
            real(kind=8) :: b
            real(kind=8) :: f
            external f
            real(kind=8) :: tol
            real(kind=8) :: coeff(*)
            real(kind=8) :: xlc
            real(kind=8) :: vitn(nbp,*)
            real(kind=8) :: defm(nbp,*)
            real(kind=8) :: rhoe(nbp,*)
            integer :: im
            integer :: jm
            real(kind=8) :: spect3
          end function spect3
        end interface
