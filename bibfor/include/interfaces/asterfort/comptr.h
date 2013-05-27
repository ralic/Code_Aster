        interface
          subroutine comptr(np1,np2,np3,nbm,nbnl,ichtr,depg,vitg,phii,&
     &typch,nbseg,alpha,beta,gamma,orig,rc,theta,old)
            integer :: np3
            integer :: np2
            integer :: np1
            integer :: nbm
            integer :: nbnl
            integer :: ichtr
            real(kind=8) :: depg(*)
            real(kind=8) :: vitg(*)
            real(kind=8) :: phii(np2,np1,*)
            integer :: typch(*)
            integer :: nbseg(*)
            real(kind=8) :: alpha(2,*)
            real(kind=8) :: beta(2,*)
            real(kind=8) :: gamma(2,*)
            real(kind=8) :: orig(6,*)
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: theta(np3,*)
            real(kind=8) :: old(9,*)
          end subroutine comptr
        end interface
