        interface
          subroutine smcomo(coef,fmod,tempe,nbhist,ftrc,trc)
            integer :: nbhist
            real(kind=8) :: coef(*)
            real(kind=8) :: fmod(*)
            real(kind=8) :: tempe
            real(kind=8) :: ftrc((3*nbhist),3)
            real(kind=8) :: trc((3*nbhist),5)
          end subroutine smcomo
        end interface
