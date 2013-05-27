        interface
          subroutine focrrs(nomfon,resu,base,nomcha,maille,noeud,cmp,&
     &npoint,nusp,ivari,ier)
            character(len=19) :: nomfon
            character(len=19) :: resu
            character(len=1) :: base
            character(len=16) :: nomcha
            character(len=8) :: maille
            character(len=8) :: noeud
            character(len=8) :: cmp
            integer :: npoint
            integer :: nusp
            integer :: ivari
            integer :: ier
          end subroutine focrrs
        end interface
