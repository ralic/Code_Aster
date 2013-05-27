        interface
          subroutine uthk(nomte,geom,hk,ndim,noe,nsomm,tymvol,ifa,niv,&
     &ifm)
            character(len=16) :: nomte
            real(kind=8) :: geom(*)
            real(kind=8) :: hk
            integer :: ndim
            integer :: noe(9,6,4)
            integer :: nsomm
            integer :: tymvol
            integer :: ifa
            integer :: niv
            integer :: ifm
          end subroutine uthk
        end interface
