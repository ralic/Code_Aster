        interface
          subroutine xtform(ndim,typmae,typmam,typmac,nne,nnm,nnc,&
     &coore,coorm,coorc,ffe,ffm,dffc)
            integer :: ndim
            character(len=8) :: typmae
            character(len=8) :: typmam
            character(len=8) :: typmac
            integer :: nne
            integer :: nnm
            integer :: nnc
            real(kind=8) :: coore(3)
            real(kind=8) :: coorm(3)
            real(kind=8) :: coorc(2)
            real(kind=8) :: ffe(20)
            real(kind=8) :: ffm(20)
            real(kind=8) :: dffc(3,9)
          end subroutine xtform
        end interface
