        interface
          subroutine xcatls(ndim,geofis,callst,jltsv,jltsl,jlnsv,jlnsl&
     &,noma,vect1,vect2,noeud,a,b,r,cote)
            integer :: ndim
            character(len=16) :: geofis
            logical :: callst
            integer :: jltsv
            integer :: jltsl
            integer :: jlnsv
            integer :: jlnsl
            character(len=8) :: noma
            real(kind=8) :: vect1(3)
            real(kind=8) :: vect2(3)
            real(kind=8) :: noeud(3)
            real(kind=8) :: a
            real(kind=8) :: b
            real(kind=8) :: r
            character(len=8) :: cote
          end subroutine xcatls
        end interface
