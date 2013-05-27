        interface
          subroutine resloc(modele,ligrel,yaxfem,yathm,tbgrca,perman,&
     &chtime,mate,sigmam,sigmap,chsigx,chdepm,chdepp,cherrm,lchar,nchar,&
     &tabido,chvois,cvoisx,chelem)
            character(len=8) :: modele
            character(*) :: ligrel
            logical :: yaxfem
            logical :: yathm
            real(kind=8) :: tbgrca(3)
            logical :: perman
            character(len=24) :: chtime
            character(*) :: mate
            character(len=24) :: sigmam
            character(len=24) :: sigmap
            character(len=24) :: chsigx
            character(len=24) :: chdepm
            character(len=24) :: chdepp
            character(len=24) :: cherrm
            character(len=8) :: lchar(1)
            integer :: nchar
            integer :: tabido(5)
            character(len=24) :: chvois
            character(len=24) :: cvoisx
            character(len=24) :: chelem
          end subroutine resloc
        end interface
