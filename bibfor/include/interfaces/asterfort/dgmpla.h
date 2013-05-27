        interface
          subroutine dgmpla(eb,nub,ea,sya,num,nuf,h,a,b1,b,nnap,rx,ry,&
     &mp,drp,w)
            real(kind=8) :: eb
            real(kind=8) :: nub
            real(kind=8) :: ea(*)
            real(kind=8) :: sya(*)
            real(kind=8) :: num
            real(kind=8) :: nuf
            real(kind=8) :: h
            real(kind=8) :: a
            real(kind=8) :: b1
            real(kind=8) :: b
            integer :: nnap
            real(kind=8) :: rx(*)
            real(kind=8) :: ry(*)
            real(kind=8) :: mp
            real(kind=8) :: drp
            real(kind=8) :: w
          end subroutine dgmpla
        end interface
