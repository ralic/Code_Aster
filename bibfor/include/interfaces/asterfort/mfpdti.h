        interface
          subroutine mfpdti(fid,cha,ind,numdt,numo,dt,cret)
            integer :: fid
            character(*) :: cha
            integer :: ind
            integer :: numdt
            integer :: numo
            real(kind=8) :: dt
            integer :: cret
          end subroutine mfpdti
        end interface
