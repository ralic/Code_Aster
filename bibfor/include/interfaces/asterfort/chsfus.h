        interface
          subroutine chsfus(nbchs,lichs,lcumul,lcoefr,lcoefc,lcoc,base&
     &,chs3)
            integer :: nbchs
            character(*) :: lichs(nbchs)
            logical :: lcumul(nbchs)
            real(kind=8) :: lcoefr(nbchs)
            complex(kind=8) :: lcoefc(nbchs)
            logical :: lcoc
            character(*) :: base
            character(*) :: chs3
          end subroutine chsfus
        end interface
