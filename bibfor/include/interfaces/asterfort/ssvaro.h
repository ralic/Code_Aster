        interface
          subroutine ssvaro(l,sens,matrix,typnoe,nomacr,iadm1,iadm2)
            real(kind=8) :: l(6,6)
            character(*) :: sens
            logical :: matrix
            character(len=4) :: typnoe
            character(len=8) :: nomacr
            integer :: iadm1
            integer :: iadm2
          end subroutine ssvaro
        end interface
