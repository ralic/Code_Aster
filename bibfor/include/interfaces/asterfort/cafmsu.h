        interface
          subroutine cafmsu(ifa,cont,tange,maxfa,nface,fkss,dfks1,&
     &dfks2,mobfas,dmob1s,dmob2s,fmw,fm1w,fm2w)
            integer :: nface
            integer :: maxfa
            integer :: ifa
            logical :: cont
            logical :: tange
            real(kind=8) :: fkss
            real(kind=8) :: dfks1(1+maxfa,nface)
            real(kind=8) :: dfks2(1+maxfa,nface)
            real(kind=8) :: mobfas
            real(kind=8) :: dmob1s
            real(kind=8) :: dmob2s
            real(kind=8) :: fmw(nface)
            real(kind=8) :: fm1w(1+maxfa,nface)
            real(kind=8) :: fm2w(1+maxfa,nface)
          end subroutine cafmsu
        end interface
