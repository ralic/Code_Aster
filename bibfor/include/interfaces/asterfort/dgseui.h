        interface
          subroutine dgseui(em,num,ef,nuf,eb,nub,sytb,h,icisai,syt,syc&
     &,dxd,syf,drd,pelast,pelasf,icompr)
            real(kind=8) :: em
            real(kind=8) :: num
            real(kind=8) :: ef
            real(kind=8) :: nuf
            real(kind=8) :: eb
            real(kind=8) :: nub
            real(kind=8) :: sytb
            real(kind=8) :: h
            integer :: icisai
            real(kind=8) :: syt
            real(kind=8) :: syc
            real(kind=8) :: dxd
            real(kind=8) :: syf
            real(kind=8) :: drd
            real(kind=8) :: pelast
            real(kind=8) :: pelasf
            integer :: icompr
          end subroutine dgseui
        end interface
