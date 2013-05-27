        interface
          subroutine zevolu(cine,z,zm,dinst,tp,k,n,tdeq,tfeq,coeffc,m,&
     &ar,br,g,dg)
            integer :: cine
            real(kind=8) :: z
            real(kind=8) :: zm
            real(kind=8) :: dinst
            real(kind=8) :: tp
            real(kind=8) :: k
            real(kind=8) :: n
            real(kind=8) :: tdeq
            real(kind=8) :: tfeq
            real(kind=8) :: coeffc
            real(kind=8) :: m
            real(kind=8) :: ar
            real(kind=8) :: br
            real(kind=8) :: g
            real(kind=8) :: dg
          end subroutine zevolu
        end interface
