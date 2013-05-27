        interface
          function spect2(a,b,xlc,vitn,rhoe,defm,f,tol,ier,r1,err,nbp,&
     &im,jm)
            integer :: nbp
            real(kind=8) :: a
            real(kind=8) :: b
            real(kind=8) :: xlc
            real(kind=8) :: vitn(nbp,*)
            real(kind=8) :: rhoe(nbp,*)
            real(kind=8) :: defm(nbp,*)
            real(kind=8) :: f
            external f
            real(kind=8) :: tol
            integer :: ier
            real(kind=8) :: r1
            real(kind=8) :: err
            integer :: im
            integer :: jm
            real(kind=8) :: spect2
          end function spect2
        end interface
