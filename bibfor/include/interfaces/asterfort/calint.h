        interface
          subroutine calint(i,j,vect1,nbpts,vect2,long,tt)
            integer :: long
            integer :: nbpts
            integer :: i
            integer :: j
            complex(kind=8) :: vect1(long)
            real(kind=8) :: vect2(nbpts)
            real(kind=8) :: tt
          end subroutine calint
        end interface
