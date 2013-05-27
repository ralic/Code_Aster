        interface
          subroutine mofick(fa,fav,cont,tange,maxfa,nface,nfacev,&
     &nfacem,fluxk,flux1k,flux2k,fluxl,flux1l,flux2l,moyfl,moyfl1,moyfl2&
     &)
            integer :: nfacem
            integer :: nfacev
            integer :: nface
            integer :: maxfa
            integer :: fa
            integer :: fav
            logical :: cont
            logical :: tange
            real(kind=8) :: fluxk(nface)
            real(kind=8) :: flux1k(1:maxfa+1,nface)
            real(kind=8) :: flux2k(1:maxfa+1,nface)
            real(kind=8) :: fluxl(nfacev)
            real(kind=8) :: flux1l(1:maxfa+1,nfacev)
            real(kind=8) :: flux2l(1:maxfa+1,nfacev)
            real(kind=8) :: moyfl(nfacem)
            real(kind=8) :: moyfl1(1:maxfa,1:maxfa+1,0:1)
            real(kind=8) :: moyfl2(1:maxfa,1:maxfa+1,0:1)
          end subroutine mofick
        end interface
