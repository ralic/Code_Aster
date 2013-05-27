        interface
          subroutine dgendo(em,ef,h,syt,syc,num,nuf,pendt,pelast,pendf&
     &,pelasf,iendo,icisai,icompr,gt,gf,gc,ipente,np,dxp)
            real(kind=8) :: em
            real(kind=8) :: ef
            real(kind=8) :: h
            real(kind=8) :: syt
            real(kind=8) :: syc
            real(kind=8) :: num
            real(kind=8) :: nuf
            real(kind=8) :: pendt
            real(kind=8) :: pelast
            real(kind=8) :: pendf
            real(kind=8) :: pelasf
            integer :: iendo
            integer :: icisai
            integer :: icompr
            real(kind=8) :: gt
            real(kind=8) :: gf
            real(kind=8) :: gc
            integer :: ipente
            real(kind=8) :: np
            real(kind=8) :: dxp
          end subroutine dgendo
        end interface
