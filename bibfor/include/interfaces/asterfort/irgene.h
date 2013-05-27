        interface
          subroutine irgene(iocc,resu,form,ifi,nbnosy,nosy,nbcmpg,cmpg&
     &,nbpara,para,nbordr,ordr,nbdisc,disc,nume,lhist)
            integer :: iocc
            character(*) :: resu
            character(*) :: form
            integer :: ifi
            integer :: nbnosy
            character(*) :: nosy(*)
            integer :: nbcmpg
            integer :: cmpg(*)
            integer :: nbpara
            character(*) :: para(*)
            integer :: nbordr
            integer :: ordr(*)
            integer :: nbdisc
            real(kind=8) :: disc(*)
            integer :: nume(*)
            logical :: lhist
          end subroutine irgene
        end interface
