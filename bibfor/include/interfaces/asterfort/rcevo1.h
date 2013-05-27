        interface
          subroutine rcevo1(nommat,fatizh,sm,para,symax)
            character(len=8) :: nommat
            logical :: fatizh
            real(kind=8) :: sm
            real(kind=8) :: para(*)
            real(kind=8) :: symax
          end subroutine rcevo1
        end interface
