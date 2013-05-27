        interface
          subroutine ptforp(itype,option,nomte,a,a2,xl,rad,angs2,ist,&
     &nno,nc,pgl,pgl1,pgl2,fer,fei)
            integer :: itype
            character(*) :: option
            character(*) :: nomte
            real(kind=8) :: a
            real(kind=8) :: a2
            real(kind=8) :: xl
            real(kind=8) :: rad
            real(kind=8) :: angs2
            integer :: ist
            integer :: nno
            integer :: nc
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: pgl1(3,3)
            real(kind=8) :: pgl2(3,3)
            real(kind=8) :: fer(*)
            real(kind=8) :: fei(*)
          end subroutine ptforp
        end interface
