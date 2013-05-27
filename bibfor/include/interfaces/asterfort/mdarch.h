        interface
          subroutine mdarch(isto1,ipas,disc,dt,nbmode,typcal,nbsym,&
     &nomsym,depger,vitger,accger,depstr,vitstr,accstr,depgec,vitgec,&
     &accgec,depstc,vitstc,accstc,passto,iorsto,discst)
            integer :: isto1
            integer :: ipas
            real(kind=8) :: disc
            real(kind=8) :: dt
            integer :: nbmode
            character(len=4) :: typcal
            integer :: nbsym
            character(len=4) :: nomsym(3)
            real(kind=8) :: depger(*)
            real(kind=8) :: vitger(*)
            real(kind=8) :: accger(*)
            real(kind=8) :: depstr(*)
            real(kind=8) :: vitstr(*)
            real(kind=8) :: accstr(*)
            complex(kind=8) :: depgec(*)
            complex(kind=8) :: vitgec(*)
            complex(kind=8) :: accgec(*)
            complex(kind=8) :: depstc(*)
            complex(kind=8) :: vitstc(*)
            complex(kind=8) :: accstc(*)
            real(kind=8) :: passto(*)
            integer :: iorsto(*)
            real(kind=8) :: discst(*)
          end subroutine mdarch
        end interface
