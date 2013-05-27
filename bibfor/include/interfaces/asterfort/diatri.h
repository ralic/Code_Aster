        interface
          subroutine diatri(n,d,e,vector,evec,ldevec)
            integer :: ldevec
            integer :: n
            real(kind=8) :: d(*)
            real(kind=8) :: e(*)
            logical :: vector
            real(kind=8) :: evec(ldevec,*)
          end subroutine diatri
        end interface
