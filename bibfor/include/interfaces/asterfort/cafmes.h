        interface
          subroutine cafmes(ifa,cont,tange,maxfa,nface,fkss,dfks1,&
     &dfks2,mobfas,dmob1,dmob2,dmob1f,dmob2f,fmw,fm1w,fm2w)
            integer :: maxfa
            integer :: ifa
            logical :: cont
            logical :: tange
            integer :: nface
            real(kind=8) :: fkss
            real(kind=8) :: dfks1(1+maxfa,maxfa)
            real(kind=8) :: dfks2(1+maxfa,maxfa)
            real(kind=8) :: mobfas
            real(kind=8) :: dmob1(1:maxfa)
            real(kind=8) :: dmob2(1:maxfa)
            real(kind=8) :: dmob1f(1:maxfa)
            real(kind=8) :: dmob2f(1:maxfa)
            real(kind=8) :: fmw(1:maxfa)
            real(kind=8) :: fm1w(1+maxfa,maxfa)
            real(kind=8) :: fm2w(1+maxfa,maxfa)
          end subroutine cafmes
        end interface
