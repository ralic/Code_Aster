        interface
          subroutine xinils(noma,maiaux,grille,ndim,meth,nfonf,nfong,&
     &geofis,a,b,r,noeud,cote,vect1,vect2,cnslt,cnsln)
            character(len=8) :: noma
            character(len=8) :: maiaux
            logical :: grille
            integer :: ndim
            character(len=8) :: meth
            character(len=8) :: nfonf
            character(len=8) :: nfong
            character(len=16) :: geofis
            real(kind=8) :: a
            real(kind=8) :: b
            real(kind=8) :: r
            real(kind=8) :: noeud(3)
            character(len=8) :: cote
            real(kind=8) :: vect1(3)
            real(kind=8) :: vect2(3)
            character(len=19) :: cnslt
            character(len=19) :: cnsln
          end subroutine xinils
        end interface
