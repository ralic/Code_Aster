        interface
          subroutine xnorme(indipt,iptbor,vectn,nbfacb,nunoa,nunob,&
     &nunoc,jcoor,coorg)
            integer :: indipt
            integer :: iptbor(2)
            real(kind=8) :: vectn(12)
            integer :: nbfacb
            integer :: nunoa
            integer :: nunob
            integer :: nunoc
            integer :: jcoor
            real(kind=8) :: coorg(3)
          end subroutine xnorme
        end interface
