        interface
          subroutine nmreso(fonact,cndonn,cnpilo,cncine,solveu,maprec,&
     &matass,depso1,depso2,rescvg)
            integer :: fonact(*)
            character(len=19) :: cndonn
            character(len=19) :: cnpilo
            character(len=19) :: cncine
            character(len=19) :: solveu
            character(len=19) :: maprec
            character(len=19) :: matass
            character(len=19) :: depso1
            character(len=19) :: depso2
            integer :: rescvg
          end subroutine nmreso
        end interface
