        interface
          subroutine epstmc(fami,ndim,instan,poum,igau,isgau,xyzgau,&
     &repere,mater,option,epsth)
            character(*) :: fami
            integer :: ndim
            real(kind=8) :: instan
            character(*) :: poum
            integer :: igau
            integer :: isgau
            real(kind=8) :: xyzgau(3)
            real(kind=8) :: repere(7)
            integer :: mater
            character(len=16) :: option
            real(kind=8) :: epsth(6)
          end subroutine epstmc
        end interface
