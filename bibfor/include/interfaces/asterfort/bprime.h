        interface
          function bprime(nbmat,mater,parame,invar1,s,epssig)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: parame(5)
            real(kind=8) :: invar1
            real(kind=8) :: s(6)
            real(kind=8) :: epssig
            real(kind=8) :: bprime
          end function bprime
        end interface
