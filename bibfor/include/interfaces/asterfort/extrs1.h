        interface
          subroutine extrs1(resu0,nbrang,nuordr,nbpara,nompar,nbarch,&
     &nuarch,nbexcl,chexcl,nbnosy)
            character(*) :: resu0
            integer :: nbrang
            integer :: nuordr(*)
            integer :: nbpara
            character(len=16) :: nompar(*)
            integer :: nbarch
            integer :: nuarch(*)
            integer :: nbexcl
            character(len=16) :: chexcl(*)
            integer :: nbnosy
          end subroutine extrs1
        end interface
