        interface
          subroutine iredm1(masse,noma,basemo,nbmode,nbmods,iamor,mass&
     &,rigi,amored,freq,smass,srigi,samor,cmass,crigi,camor)
            character(len=8) :: masse
            character(len=8) :: noma
            character(len=8) :: basemo
            integer :: nbmode
            integer :: nbmods
            integer :: iamor
            real(kind=8) :: mass(*)
            real(kind=8) :: rigi(*)
            real(kind=8) :: amored(*)
            real(kind=8) :: freq(*)
            real(kind=8) :: smass(*)
            real(kind=8) :: srigi(*)
            real(kind=8) :: samor(*)
            real(kind=8) :: cmass(*)
            real(kind=8) :: crigi(*)
            real(kind=8) :: camor(*)
          end subroutine iredm1
        end interface
