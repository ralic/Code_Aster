        interface
          subroutine mmform(ndim,nommae,nommam,nne,nnm,xpc,ypc,xpr,ypr&
     &,ffe,dffe,ddffe,ffm,dffm,ddffm,ffl,dffl,ddffl)
            integer :: ndim
            character(len=8) :: nommae
            character(len=8) :: nommam
            integer :: nne
            integer :: nnm
            real(kind=8) :: xpc
            real(kind=8) :: ypc
            real(kind=8) :: xpr
            real(kind=8) :: ypr
            real(kind=8) :: ffe(9)
            real(kind=8) :: dffe(2,9)
            real(kind=8) :: ddffe(3,9)
            real(kind=8) :: ffm(9)
            real(kind=8) :: dffm(2,9)
            real(kind=8) :: ddffm(3,9)
            real(kind=8) :: ffl(9)
            real(kind=8) :: dffl(2,9)
            real(kind=8) :: ddffl(3,9)
          end subroutine mmform
        end interface
