        interface
          subroutine cuprep(mailla,neq,deficu,resocu,deptot,inst)
            character(len=8) :: mailla
            integer :: neq
            character(len=24) :: deficu
            character(len=24) :: resocu
            character(len=19) :: deptot
            real(kind=8) :: inst
          end subroutine cuprep
        end interface
