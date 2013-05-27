        interface
          subroutine nmholi(ndim,axi,nno,npg,ipoids,ivf,idfde,imate,&
     &inst,geom,depl,chlim)
            integer :: nno
            integer :: ndim
            logical :: axi
            integer :: npg
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            integer :: imate
            real(kind=8) :: inst
            real(kind=8) :: geom(ndim,nno)
            real(kind=8) :: depl(ndim,nno)
            real(kind=8) :: chlim(3)
          end subroutine nmholi
        end interface
