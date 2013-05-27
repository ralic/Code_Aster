        interface
          subroutine mfchrl(fid,cha,val,intlac,numco,profil,pflmod,&
     &typent,typgeo,numdt,numo,cret)
            integer :: fid
            character(*) :: cha
            real(kind=8) :: val(*)
            integer :: intlac
            integer :: numco
            character(*) :: profil
            integer :: pflmod
            integer :: typent
            integer :: typgeo
            integer :: numdt
            integer :: numo
            integer :: cret
          end subroutine mfchrl
        end interface
