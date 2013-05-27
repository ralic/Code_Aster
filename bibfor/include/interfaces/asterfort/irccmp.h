        interface
          subroutine irccmp(typ,gd,ncmpmx,nomcgd,nbcmp,nomcmp,nbcmpt,&
     &jcmp)
            character(*) :: typ
            character(*) :: gd
            integer :: ncmpmx
            character(*) :: nomcgd(*)
            integer :: nbcmp
            character(*) :: nomcmp(*)
            integer :: nbcmpt
            integer :: jcmp
          end subroutine irccmp
        end interface
