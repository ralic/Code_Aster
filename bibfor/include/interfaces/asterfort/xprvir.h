        interface
          subroutine xprvir(fiss,covir,bavir,vitvir,angvir,numvir,&
     &numfon,nvit,nbeta,nbptff,radimp,radtor,damax,noma,locdom)
            character(len=8) :: fiss
            character(len=19) :: covir
            character(len=19) :: bavir
            character(len=19) :: vitvir
            character(len=19) :: angvir
            character(len=19) :: numvir
            integer :: numfon
            character(len=24) :: nvit
            character(len=24) :: nbeta
            integer :: nbptff
            real(kind=8) :: radimp
            real(kind=8) :: radtor
            real(kind=8) :: damax
            character(len=8) :: noma
            logical :: locdom
          end subroutine xprvir
        end interface
