        interface
          subroutine qires1(modele,ligrel,chtime,sigmap,sigmad,lcharp,&
     &lchard,ncharp,nchard,chs,mate,chvois,tabido,chelem)
            character(len=8) :: modele
            character(*) :: ligrel
            character(len=24) :: chtime
            character(len=24) :: sigmap
            character(len=24) :: sigmad
            character(len=8) :: lcharp(1)
            character(len=8) :: lchard(1)
            integer :: ncharp
            integer :: nchard
            character(len=24) :: chs
            character(*) :: mate
            character(len=24) :: chvois
            integer :: tabido(5)
            character(len=24) :: chelem
          end subroutine qires1
        end interface
