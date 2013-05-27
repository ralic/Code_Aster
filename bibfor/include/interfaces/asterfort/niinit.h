        interface
          subroutine niinit(nomte,typmod,ndim,nno1,nno2,nno3,nno4,vu,&
     &vg,vp,vpi)
            character(len=16) :: nomte
            character(len=8) :: typmod(*)
            integer :: ndim
            integer :: nno1
            integer :: nno2
            integer :: nno3
            integer :: nno4
            integer :: vu(3,27)
            integer :: vg(27)
            integer :: vp(27)
            integer :: vpi(3,27)
          end subroutine niinit
        end interface
