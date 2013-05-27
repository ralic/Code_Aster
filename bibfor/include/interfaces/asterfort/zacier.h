        interface
          subroutine zacier(matos,nbhist,ftrc,trc,coef,fmod,ckm,nbtrc,&
     &tpg0,tpg1,tpg2,dt10,dt21,tamp,metapg)
            integer :: nbtrc
            integer :: nbhist
            integer :: matos
            real(kind=8) :: ftrc((3*nbhist),3)
            real(kind=8) :: trc((3*nbhist),5)
            real(kind=8) :: coef(*)
            real(kind=8) :: fmod(*)
            real(kind=8) :: ckm(6*nbtrc)
            real(kind=8) :: tpg0
            real(kind=8) :: tpg1
            real(kind=8) :: tpg2
            real(kind=8) :: dt10
            real(kind=8) :: dt21
            real(kind=8) :: tamp(7)
            real(kind=8) :: metapg(7)
          end subroutine zacier
        end interface
