        interface
          subroutine asveri(knomsy,nbopt,meca,psmo,stat,tronc,monoap,&
     &nbsup,nsupp,nomsup,ndir,nordr,nbmode)
            integer :: nbsup
            character(*) :: knomsy(*)
            integer :: nbopt
            character(*) :: meca
            character(*) :: psmo
            character(*) :: stat
            logical :: tronc
            logical :: monoap
            integer :: nsupp(*)
            character(*) :: nomsup(nbsup,*)
            integer :: ndir(*)
            integer :: nordr(*)
            integer :: nbmode
          end subroutine asveri
        end interface
