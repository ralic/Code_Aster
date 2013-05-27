        interface
          subroutine intftr(np4,nfour,nbm,za4,za5,aa,bb,zitr)
            integer :: np4
            integer :: nfour
            integer :: nbm
            complex(kind=8) :: za4(np4,*)
            complex(kind=8) :: za5(np4,*)
            real(kind=8) :: aa(np4,*)
            real(kind=8) :: bb(np4,*)
            complex(kind=8) :: zitr(*)
          end subroutine intftr
        end interface
