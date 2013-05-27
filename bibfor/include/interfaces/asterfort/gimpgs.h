        interface
          subroutine gimpgs(result,nnoff,absc,gs,numero,gi,ndeg,ndimte&
     &,gthi,extim,time,iordr,unit)
            character(len=8) :: result
            integer :: nnoff
            real(kind=8) :: absc(*)
            real(kind=8) :: gs(1)
            integer :: numero
            real(kind=8) :: gi(1)
            integer :: ndeg
            integer :: ndimte
            real(kind=8) :: gthi(1)
            logical :: extim
            real(kind=8) :: time
            integer :: iordr
            integer :: unit
          end subroutine gimpgs
        end interface
