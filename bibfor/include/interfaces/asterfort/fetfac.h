        interface
          subroutine fetfac(lmat,matas,idd,nprec,nbsd,matass,sdfeti,&
     &nbsdf,base,infofe)
            integer :: lmat
            character(len=19) :: matas
            integer :: idd
            integer :: nprec
            integer :: nbsd
            character(len=19) :: matass
            character(len=24) :: sdfeti
            integer :: nbsdf
            character(len=1) :: base
            character(len=24) :: infofe
          end subroutine fetfac
        end interface
