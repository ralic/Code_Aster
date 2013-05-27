        interface
          subroutine matdtd(nomte,testl1,testl2,dsidep,cisail,x3,cour,&
     &r,cosa,kappa,dtildi)
            character(len=16) :: nomte
            logical :: testl1
            logical :: testl2
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: cisail
            real(kind=8) :: x3
            real(kind=8) :: cour
            real(kind=8) :: r
            real(kind=8) :: cosa
            real(kind=8) :: kappa
            real(kind=8) :: dtildi(5,5)
          end subroutine matdtd
        end interface
