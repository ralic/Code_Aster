        interface
          subroutine mdacce(typbas,neqgen,pulsa2,masgen,descm,riggen,&
     &descr,fexgen,lamor,amogen,desca,work1,depgen,vitgen,accgen)
            character(len=16) :: typbas
            integer :: neqgen
            real(kind=8) :: pulsa2(*)
            real(kind=8) :: masgen(*)
            integer :: descm
            real(kind=8) :: riggen(*)
            integer :: descr
            real(kind=8) :: fexgen(*)
            logical :: lamor
            real(kind=8) :: amogen(*)
            integer :: desca
            real(kind=8) :: work1(*)
            real(kind=8) :: depgen(*)
            real(kind=8) :: vitgen(*)
            real(kind=8) :: accgen(*)
          end subroutine mdacce
        end interface
