        interface
          subroutine dmat3d(fami,mater,instan,poum,igau,isgau,repere,&
     &xyzgau,d)
            character(*) :: fami
            integer :: mater
            real(kind=8) :: instan
            character(*) :: poum
            integer :: igau
            integer :: isgau
            real(kind=8) :: repere(7)
            real(kind=8) :: xyzgau(3)
            real(kind=8) :: d(6,6)
          end subroutine dmat3d
        end interface
