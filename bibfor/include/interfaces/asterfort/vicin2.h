        interface
          subroutine vicin2(ndim,g,npg,lgpg,vim,rpt,nbcin,numcin)
            integer :: lgpg
            integer :: npg
            integer :: ndim
            integer :: g
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: rpt(3,3)
            integer :: nbcin
            integer :: numcin(2)
          end subroutine vicin2
        end interface
