        interface
          subroutine rescmp(cndiri,cnvcfo,cnfext,cnfint,cnfnod,maxres,&
     &noddlm,numno)
            character(len=19) :: cndiri
            character(len=19) :: cnvcfo
            character(len=19) :: cnfext
            character(len=19) :: cnfint
            character(len=19) :: cnfnod
            real(kind=8) :: maxres
            character(len=8) :: noddlm
            integer :: numno
          end subroutine rescmp
        end interface
