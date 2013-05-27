        interface
          subroutine sansno(char,motfac,noma,sans,psans,nbmocl,tymocl,&
     &limocl)
            integer :: nbmocl
            character(len=8) :: char
            character(len=16) :: motfac
            character(len=8) :: noma
            character(len=24) :: sans
            character(len=24) :: psans
            character(len=16) :: tymocl(nbmocl)
            character(len=16) :: limocl(nbmocl)
          end subroutine sansno
        end interface
