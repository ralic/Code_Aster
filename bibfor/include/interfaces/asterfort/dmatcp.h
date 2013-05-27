        interface
          subroutine dmatcp(fami,mater,instan,poum,igau,isgau,repere,d&
     &)
            character(*) :: fami
            integer :: mater
            real(kind=8) :: instan
            character(*) :: poum
            integer :: igau
            integer :: isgau
            real(kind=8) :: repere(7)
            real(kind=8) :: d(4,4)
          end subroutine dmatcp
        end interface
