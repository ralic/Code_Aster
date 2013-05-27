        interface
          subroutine xcalf2(he,lsng,lstg,baslog,fe,dgdgl,iret)
            real(kind=8) :: he
            real(kind=8) :: lsng
            real(kind=8) :: lstg
            real(kind=8) :: baslog(6)
            real(kind=8) :: fe(4)
            real(kind=8) :: dgdgl(4,2)
            integer :: iret
          end subroutine xcalf2
        end interface
