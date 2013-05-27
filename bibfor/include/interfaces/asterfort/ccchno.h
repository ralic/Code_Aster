        interface
          subroutine ccchno(option,numord,resuin,resuou,lichou,mesmai,&
     &nomail,modele,carael,basopt,ligrel,ligmod,codret)
            character(len=16) :: option
            integer :: numord
            character(len=8) :: resuin
            character(len=8) :: resuou
            character(len=24) :: lichou(2)
            character(len=24) :: mesmai
            character(len=8) :: nomail
            character(len=8) :: modele
            character(len=8) :: carael
            character(len=1) :: basopt
            character(len=24) :: ligrel
            logical :: ligmod
            integer :: codret
          end subroutine ccchno
        end interface
