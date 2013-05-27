        interface
          subroutine mfchre(fid,cha,val,intlac,n,locname,numco,profil,&
     &pflmod,typent,typgeo,numdt,dt,numo,cret)
            integer :: fid
            character(*) :: cha
            real(kind=8) :: val(*)
            integer :: intlac
            integer :: n
            character(*) :: locname
            integer :: numco
            character(*) :: profil
            integer :: pflmod
            integer :: typent
            integer :: typgeo
            integer :: numdt
            real(kind=8) :: dt
            integer :: numo
            integer :: cret
          end subroutine mfchre
        end interface
