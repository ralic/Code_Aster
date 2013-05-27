        interface
          subroutine mmlagm(nbdm,ndim,nnl,jdepde,ffl,dlagrc,dlagrf)
            integer :: nbdm
            integer :: ndim
            integer :: nnl
            integer :: jdepde
            real(kind=8) :: ffl(9)
            real(kind=8) :: dlagrc
            real(kind=8) :: dlagrf(2)
          end subroutine mmlagm
        end interface
