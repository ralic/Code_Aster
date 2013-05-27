        interface
          subroutine algocu(deficu,resocu,solveu,lmat,ldscon,cncine,&
     &resu,ctccvg)
            character(len=24) :: deficu
            character(len=24) :: resocu
            character(len=19) :: solveu
            integer :: lmat
            integer :: ldscon
            character(len=19) :: cncine
            character(len=19) :: resu
            integer :: ctccvg
          end subroutine algocu
        end interface
