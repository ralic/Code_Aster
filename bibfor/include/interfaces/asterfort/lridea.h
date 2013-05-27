        interface
          subroutine lridea(resu,typres,linoch,nbnoch,nomcmd,listrz,&
     &listiz,precis,crit,epsi,acces,mfich,noma,ligrez,nbvari)
            character(len=8) :: resu
            character(*) :: typres
            character(*) :: linoch(*)
            integer :: nbnoch
            character(*) :: nomcmd
            character(*) :: listrz
            character(*) :: listiz
            integer :: precis
            character(*) :: crit
            real(kind=8) :: epsi
            character(*) :: acces
            integer :: mfich
            character(len=8) :: noma
            character(*) :: ligrez
            integer :: nbvari
          end subroutine lridea
        end interface
