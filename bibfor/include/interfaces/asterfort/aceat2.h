        interface
          subroutine aceat2(nbtuy,eltuy,notuy,nbpart,noex1,noex2,nbmap&
     &,elpar,nopar,nno)
            integer :: nno
            integer :: nbpart
            integer :: nbtuy
            integer :: eltuy(nbtuy)
            integer :: notuy(nno*nbtuy)
            integer :: noex1(nbpart)
            integer :: noex2(nbpart)
            integer :: nbmap(nbpart)
            integer :: elpar(nbpart,nbtuy)
            integer :: nopar(nbpart,nno,nbtuy)
          end subroutine aceat2
        end interface
