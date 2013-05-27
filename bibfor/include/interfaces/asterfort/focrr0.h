        interface
          subroutine focrr0(nomfon,interp,base,resu,nomcha,maille,&
     &noeud,cmp,npoint,nusp,ivari,nbordr,lordr)
            character(len=19) :: nomfon
            character(len=8) :: interp
            character(len=1) :: base
            character(len=19) :: resu
            character(len=16) :: nomcha
            character(len=8) :: maille
            character(len=8) :: noeud
            character(len=8) :: cmp
            integer :: npoint
            integer :: nusp
            integer :: ivari
            integer :: nbordr
            integer :: lordr(*)
          end subroutine focrr0
        end interface
