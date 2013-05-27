        interface
          subroutine enerca(valinc,dep0,vit0,depl1,vite1,masse,amort,&
     &rigid,fexte,famor,fliai,fnoda,fcine,lamort,ldyna,lexpl,sdener,&
     &schema)
            character(len=19) :: valinc(*)
            real(kind=8) :: dep0(*)
            real(kind=8) :: vit0(*)
            real(kind=8) :: depl1(*)
            real(kind=8) :: vite1(*)
            character(len=19) :: masse
            character(len=19) :: amort
            character(len=19) :: rigid
            real(kind=8) :: fexte(*)
            real(kind=8) :: famor(*)
            real(kind=8) :: fliai(*)
            real(kind=8) :: fnoda(*)
            real(kind=8) :: fcine(*)
            logical :: lamort
            logical :: ldyna
            logical :: lexpl
            character(len=19) :: sdener
            character(len=8) :: schema
          end subroutine enerca
        end interface
