        interface
          subroutine hfmss(ind,vectt,hsfm,hss)
            integer :: ind
            real(kind=8) :: vectt(3,3)
            real(kind=8) :: hsfm(3,9)
            real(kind=8) :: hss(2,9)
          end subroutine hfmss
        end interface
