        interface
          subroutine matbsu(nb1,xr,npgsr,intsn,b1mnc,b2mnc,b1mni,b2mni&
     &,b1mri,b2mri,b1src,b2src,b1su,b2su)
            integer :: nb1
            real(kind=8) :: xr(*)
            integer :: npgsr
            integer :: intsn
            real(kind=8) :: b1mnc(3,51)
            real(kind=8) :: b2mnc(3,51)
            real(kind=8) :: b1mni(3,51)
            real(kind=8) :: b2mni(3,51)
            real(kind=8) :: b1mri(3,51,4)
            real(kind=8) :: b2mri(3,51,4)
            real(kind=8) :: b1src(2,51,4)
            real(kind=8) :: b2src(2,51,4)
            real(kind=8) :: b1su(5,51)
            real(kind=8) :: b2su(5,51)
          end subroutine matbsu
        end interface
