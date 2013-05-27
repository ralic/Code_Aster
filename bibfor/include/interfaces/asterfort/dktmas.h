        interface
          subroutine dktmas(xyzl,option,pgl,mas,ener,multic)
            real(kind=8) :: xyzl(3,*)
            character(len=16) :: option
            real(kind=8) :: pgl(*)
            real(kind=8) :: mas(*)
            real(kind=8) :: ener(*)
            integer :: multic
          end subroutine dktmas
        end interface
