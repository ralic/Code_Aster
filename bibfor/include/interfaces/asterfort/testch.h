        interface
          subroutine testch(np1,np2,np3,nbmcd,nbnl,toln,tolc,tolv,&
     &typch,nbseg,phii,alpha,beta,gamma,orig,rc,theta,tconf1,depg,nbch,&
     &nbchex,iconf,ftest,iconfb,tconf2)
            integer :: np3
            integer :: np2
            integer :: np1
            integer :: nbmcd
            integer :: nbnl
            real(kind=8) :: toln
            real(kind=8) :: tolc
            real(kind=8) :: tolv
            integer :: typch(*)
            integer :: nbseg(*)
            real(kind=8) :: phii(np2,np1,3)
            real(kind=8) :: alpha(2,*)
            real(kind=8) :: beta(2,*)
            real(kind=8) :: gamma(2,*)
            real(kind=8) :: orig(6,*)
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: theta(np3,*)
            real(kind=8) :: tconf1(4,*)
            real(kind=8) :: depg(*)
            integer :: nbch
            integer :: nbchex
            integer :: iconf
            real(kind=8) :: ftest
            integer :: iconfb(*)
            real(kind=8) :: tconf2(4,*)
          end subroutine testch
        end interface
