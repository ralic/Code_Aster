        interface
          subroutine memaxg(nborn,born,gbil,lonvec,result)
            integer :: nborn
            real(kind=8) :: born(*)
            real(kind=8) :: gbil(*)
            integer :: lonvec
            character(len=8) :: result
          end subroutine memaxg
        end interface
