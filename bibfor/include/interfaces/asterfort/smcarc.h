        interface
          subroutine smcarc(nbhist,ftrc,trc,coef,fmod,ctes,ckm,nbtrc,&
     &tempe,tpoint,dt,zin,zout)
            integer :: nbtrc
            integer :: nbhist
            real(kind=8) :: ftrc((3*nbhist),3)
            real(kind=8) :: trc((3*nbhist),5)
            real(kind=8) :: coef(*)
            real(kind=8) :: fmod(*)
            real(kind=8) :: ctes(11)
            real(kind=8) :: ckm(6*nbtrc)
            real(kind=8) :: tempe
            real(kind=8) :: tpoint
            real(kind=8) :: dt
            real(kind=8) :: zin(7)
            real(kind=8) :: zout(7)
          end subroutine smcarc
        end interface
