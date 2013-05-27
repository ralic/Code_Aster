        interface
          subroutine matbmr(nb1,vectt,dudxri,intsr,jdn1ri,b1mri,b2mri)
            integer :: nb1
            real(kind=8) :: vectt(3,3)
            real(kind=8) :: dudxri(9)
            integer :: intsr
            real(kind=8) :: jdn1ri(9,51)
            real(kind=8) :: b1mri(3,51,4)
            real(kind=8) :: b2mri(3,51,4)
          end subroutine matbmr
        end interface
