        interface
          subroutine xls3d(callst,grille,jltsv,jltsl,jlnsv,jlnsl,nbno,&
     &jcoor,jcoorg,nbmaf,jdlima,nbsef,jdlise,jconx1,jconx2,noma)
            logical :: callst
            logical :: grille
            integer :: jltsv
            integer :: jltsl
            integer :: jlnsv
            integer :: jlnsl
            integer :: nbno
            integer :: jcoor
            integer :: jcoorg
            integer :: nbmaf
            integer :: jdlima
            integer :: nbsef
            integer :: jdlise
            integer :: jconx1
            integer :: jconx2
            character(len=8) :: noma
          end subroutine xls3d
        end interface
