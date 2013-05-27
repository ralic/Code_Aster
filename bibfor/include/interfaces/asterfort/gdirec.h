        interface
          subroutine gdirec(noma,fond,chaine,nomobj,nomnoe,coorn,&
     &nbnoeu,dire3,milieu)
            character(len=8) :: noma
            character(len=8) :: fond
            character(len=8) :: chaine
            character(len=24) :: nomobj
            character(len=8) :: nomnoe(*)
            character(len=24) :: coorn
            integer :: nbnoeu
            character(len=24) :: dire3
            logical :: milieu
          end subroutine gdirec
        end interface
