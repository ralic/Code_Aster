        interface
          subroutine ptfocp(itype,option,nomte,xl,rad,angs2,nno,nc,pgl&
     &,pgl1,pgl2,fer,fei)
            integer :: itype
            character(*) :: option
            character(*) :: nomte
            real(kind=8) :: xl
            real(kind=8) :: rad
            real(kind=8) :: angs2
            integer :: nno
            integer :: nc
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: pgl1(3,3)
            real(kind=8) :: pgl2(3,3)
            real(kind=8) :: fer(12)
            real(kind=8) :: fei(12)
          end subroutine ptfocp
        end interface
