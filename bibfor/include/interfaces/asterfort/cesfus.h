        interface
          subroutine cesfus(nbchs,lichs,lcumul,lcoefr,lcoefc,lcoc,base&
     &,ces3z)
            integer :: nbchs
            character(*) :: lichs(nbchs)
            logical :: lcumul(nbchs)
            real(kind=8) :: lcoefr(nbchs)
            complex(kind=8) :: lcoefc(nbchs)
            logical :: lcoc
            character(*) :: base
            character(*) :: ces3z
          end subroutine cesfus
        end interface
