        interface
          subroutine xcalfe(he,lsng,lstg,baslog,fe,dgdgl,iret)
            real(kind=8) :: he
            real(kind=8) :: lsng
            real(kind=8) :: lstg
            real(kind=8) :: baslog(9)
            real(kind=8) :: fe(4)
            real(kind=8) :: dgdgl(4,3)
            integer :: iret
          end subroutine xcalfe
        end interface
