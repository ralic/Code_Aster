        interface
          subroutine mefsma(matm,mata,matr,nugene,masgen,amogen,riggen&
     &)
            real(kind=8) :: matm(*)
            real(kind=8) :: mata(*)
            real(kind=8) :: matr(*)
            character(len=14) :: nugene
            character(len=19) :: masgen
            character(len=19) :: amogen
            character(len=19) :: riggen
          end subroutine mefsma
        end interface
