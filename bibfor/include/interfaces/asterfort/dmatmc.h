        interface
          subroutine dmatmc(fami,modeli,mater,instan,poum,igau,isgau,&
     &repere,xyzgau,nbsig,d)
            integer :: nbsig
            character(*) :: fami
            character(len=2) :: modeli
            integer :: mater
            real(kind=8) :: instan
            character(*) :: poum
            integer :: igau
            integer :: isgau
            real(kind=8) :: repere(7)
            real(kind=8) :: xyzgau(1)
            real(kind=8) :: d(nbsig,1)
          end subroutine dmatmc
        end interface
