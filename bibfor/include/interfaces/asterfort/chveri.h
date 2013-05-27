        interface
          subroutine chveri(np1,np2,np3,nbm,nbmcd,nbnl,typch,nbseg,&
     &phii,noecho,alpha,beta,gamma,orig,rc,theta,depg)
            integer :: np3
            integer :: np2
            integer :: np1
            integer :: nbm
            integer :: nbmcd
            integer :: nbnl
            integer :: typch(*)
            integer :: nbseg(*)
            real(kind=8) :: phii(np2,np1,3)
            character(len=8) :: noecho(np2,*)
            real(kind=8) :: alpha(2,*)
            real(kind=8) :: beta(2,*)
            real(kind=8) :: gamma(2,*)
            real(kind=8) :: orig(6,*)
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: theta(np3,*)
            real(kind=8) :: depg(*)
          end subroutine chveri
        end interface
