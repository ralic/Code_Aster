        interface
          subroutine mdchof(np1,np2,np3,nbm,impr,tc,nbnl,typch,nbseg,&
     &phii,nomch,choc,alpha,beta,gamma,orig,rc,theta,vitg,depg,vitg0,&
     &depg0,old,oldia,fmres,fmod,ftmp,testc,itforn,toln)
            integer :: np3
            integer :: np2
            integer :: np1
            integer :: nbm
            integer :: impr
            real(kind=8) :: tc
            integer :: nbnl
            integer :: typch(*)
            integer :: nbseg(*)
            real(kind=8) :: phii(np2,np1,*)
            character(len=8) :: nomch(*)
            real(kind=8) :: choc(6,*)
            real(kind=8) :: alpha(2,*)
            real(kind=8) :: beta(2,*)
            real(kind=8) :: gamma(2,*)
            real(kind=8) :: orig(6,*)
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: theta(np3,*)
            real(kind=8) :: vitg(*)
            real(kind=8) :: depg(*)
            real(kind=8) :: vitg0(*)
            real(kind=8) :: depg0(*)
            real(kind=8) :: old(9,*)
            integer :: oldia(*)
            real(kind=8) :: fmres(*)
            real(kind=8) :: fmod(*)
            real(kind=8) :: ftmp(*)
            integer :: testc
            integer :: itforn(*)
            real(kind=8) :: toln
          end subroutine mdchof
        end interface
