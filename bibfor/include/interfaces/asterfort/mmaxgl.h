        interface
          subroutine mmaxgl(nborn,born,gbil,noeu,abcur,lonvec,nnoff,&
     &result)
            integer :: nborn
            real(kind=8) :: born(*)
            real(kind=8) :: gbil(*)
            character(len=8) :: noeu(*)
            real(kind=8) :: abcur(*)
            integer :: lonvec
            integer :: nnoff
            character(len=8) :: result
          end subroutine mmaxgl
        end interface
