        interface
          subroutine calnd2(ic,np1,np2,np3,nbm,typch,nbseg,alpha,beta,&
     &gamma,orig,rc,theta,phii,depg,vitgc,ddist2)
            integer :: np3
            integer :: np2
            integer :: np1
            integer :: ic
            integer :: nbm
            integer :: typch(*)
            integer :: nbseg(*)
            real(kind=8) :: alpha(2,*)
            real(kind=8) :: beta(2,*)
            real(kind=8) :: gamma(2,*)
            real(kind=8) :: orig(6,*)
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: theta(np3,*)
            real(kind=8) :: phii(np2,np1,*)
            real(kind=8) :: depg(*)
            real(kind=8) :: vitgc(*)
            real(kind=8) :: ddist2
          end subroutine calnd2
        end interface
