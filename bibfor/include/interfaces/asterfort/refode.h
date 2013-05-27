        interface
          subroutine refode(nbcmb,angle,nomch,nuharm,tyharm,coef,basz,&
     &chpres)
            integer :: nbcmb
            real(kind=8) :: angle
            character(*) :: nomch(*)
            integer :: nuharm(*)
            character(*) :: tyharm(*)
            real(kind=8) :: coef(*)
            character(*) :: basz
            character(*) :: chpres
          end subroutine refode
        end interface
