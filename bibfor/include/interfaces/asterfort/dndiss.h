        interface
          subroutine dndiss(ipara,nmnbn,nmplas,nmdpla,nmddpl,nmprox,&
     &deps,newnbn,newpla,newdpl,newddp,newzfg,despit,ddisit,dc1,dc2,dtg,&
     &normm,normn)
            integer :: ipara(4)
            real(kind=8) :: nmnbn(*)
            real(kind=8) :: nmplas(2,*)
            real(kind=8) :: nmdpla(2,*)
            real(kind=8) :: nmddpl(2,*)
            integer :: nmprox(*)
            real(kind=8) :: deps(*)
            real(kind=8) :: newnbn(*)
            real(kind=8) :: newpla(2,*)
            real(kind=8) :: newdpl(2,*)
            real(kind=8) :: newddp(2,*)
            real(kind=8) :: newzfg(2)
            real(kind=8) :: despit(*)
            real(kind=8) :: ddisit
            real(kind=8) :: dc1(6,6)
            real(kind=8) :: dc2(6,6)
            real(kind=8) :: dtg(6,6)
            real(kind=8) :: normm
            real(kind=8) :: normn
          end subroutine dndiss
        end interface
