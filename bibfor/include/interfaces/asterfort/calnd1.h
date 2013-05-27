        interface
          subroutine calnd1(ic,np1,np2,np3,nbm,alpha,beta,gamma,orig,&
     &rc,theta,typch,nbseg,phii,depg,dist2)
            integer :: np3
            integer :: np2
            integer :: np1
            integer :: ic
            integer :: nbm
            real(kind=8) :: alpha(2,*)
            real(kind=8) :: beta(2,*)
            real(kind=8) :: gamma(2,*)
            real(kind=8) :: orig(6,*)
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: theta(np3,*)
            integer :: typch(*)
            integer :: nbseg(*)
            real(kind=8) :: phii(np2,np1,*)
            real(kind=8) :: depg(*)
            real(kind=8) :: dist2
          end subroutine calnd1
        end interface
