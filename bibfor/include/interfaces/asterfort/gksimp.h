        interface
          subroutine gksimp(result,nnoff,absc,iadrgk,numero,iadgks,&
     &ndeg,ndimte,iadgki,extim,time,iordr,unit)
            character(len=8) :: result
            integer :: nnoff
            real(kind=8) :: absc(*)
            integer :: iadrgk
            integer :: numero
            integer :: iadgks
            integer :: ndeg
            integer :: ndimte
            integer :: iadgki
            logical :: extim
            real(kind=8) :: time
            integer :: iordr
            integer :: unit
          end subroutine gksimp
        end interface
