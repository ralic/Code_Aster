        interface
          subroutine trprec(mcf,iocc,epsi,crit,prec,crit2)
            character(*) :: mcf
            integer :: iocc
            real(kind=8) :: epsi
            character(len=8) :: crit
            real(kind=8) :: prec
            character(len=8) :: crit2
          end subroutine trprec
        end interface
