        interface
          subroutine irmgms(ifc,ndim,nno,noma,nbgrm,nonoe,lgmsh,versio&
     &)
            integer :: ifc
            integer :: ndim
            integer :: nno
            character(len=8) :: noma
            integer :: nbgrm
            character(len=8) :: nonoe(*)
            logical :: lgmsh
            integer :: versio
          end subroutine irmgms
        end interface
