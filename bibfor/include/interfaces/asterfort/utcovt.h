        interface
          subroutine utcovt(type,tbvalr,tbvali,tberr,rela,tchval,&
     &tcherr)
            character(len=1) :: type
            real(kind=8) :: tbvalr(2)
            integer :: tbvali(2)
            real(kind=8) :: tberr(2)
            character(len=4) :: rela
            character(len=24) :: tchval(2)
            character(len=16) :: tcherr(2)
          end subroutine utcovt
        end interface
