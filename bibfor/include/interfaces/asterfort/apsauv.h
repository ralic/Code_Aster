        interface
          subroutine apsauv(phasez,sdappa,izone,ip,vali,valr)
            character(*) :: phasez
            character(len=19) :: sdappa
            integer :: izone
            integer :: ip
            integer :: vali(*)
            real(kind=8) :: valr(*)
          end subroutine apsauv
        end interface
