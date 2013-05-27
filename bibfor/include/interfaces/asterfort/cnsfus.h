        interface
          subroutine cnsfus(nbchs,lichs,lcumul,lcoefr,lcoefc,lcoc,base&
     &,cns3z)
            integer :: nbchs
            character(*) :: lichs(nbchs)
            logical :: lcumul(nbchs)
            real(kind=8) :: lcoefr(nbchs)
            complex(kind=8) :: lcoefc(nbchs)
            logical :: lcoc
            character(*) :: base
            character(*) :: cns3z
          end subroutine cnsfus
        end interface
