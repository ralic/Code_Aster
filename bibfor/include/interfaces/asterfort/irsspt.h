        interface
          subroutine irsspt(cesz,unite,nbmat,nummai,nbcmp,nomcmp,lsup,&
     &linf,lmax,lmin,borinf,borsup)
            character(*) :: cesz
            integer :: unite
            integer :: nbmat
            integer :: nummai(*)
            integer :: nbcmp
            character(*) :: nomcmp(*)
            logical :: lsup
            logical :: linf
            logical :: lmax
            logical :: lmin
            real(kind=8) :: borinf
            real(kind=8) :: borsup
          end subroutine irsspt
        end interface
