        interface
          subroutine ptfop1(itype,coef1,coef2,xl,rad,angs2,global,qq,&
     &fe)
            integer :: itype
            real(kind=8) :: coef1
            real(kind=8) :: coef2
            real(kind=8) :: xl
            real(kind=8) :: rad
            real(kind=8) :: angs2
            logical :: global
            real(kind=8) :: qq(12)
            real(kind=8) :: fe(12)
          end subroutine ptfop1
        end interface
