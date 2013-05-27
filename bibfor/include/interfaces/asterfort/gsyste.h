        interface
          subroutine gsyste(matr,nchthe,nnoff,gthi,gi)
            integer :: nnoff
            integer :: nchthe
            character(len=24) :: matr
            real(kind=8) :: gthi(nnoff)
            real(kind=8) :: gi(nchthe)
          end subroutine gsyste
        end interface
