        interface
          subroutine mdptem(nbmode,masgen,pulsat,nbchoc,dplmod,parcho,&
     &noecho,dt,dts,dtu,dtmax,dtmin,tinit,tfin,nbpas,info,ier,lisins)
            integer :: nbchoc
            integer :: nbmode
            real(kind=8) :: masgen(*)
            real(kind=8) :: pulsat(*)
            real(kind=8) :: dplmod(nbchoc,nbmode,*)
            real(kind=8) :: parcho(nbchoc,*)
            character(len=8) :: noecho(nbchoc,*)
            real(kind=8) :: dt
            real(kind=8) :: dts
            real(kind=8) :: dtu
            real(kind=8) :: dtmax
            real(kind=8) :: dtmin
            real(kind=8) :: tinit
            real(kind=8) :: tfin
            integer :: nbpas
            integer :: info
            integer :: ier
            character(len=24) :: lisins
          end subroutine mdptem
        end interface
