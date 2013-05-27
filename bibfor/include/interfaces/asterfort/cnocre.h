        interface
          subroutine cnocre(maz,nomgdz,nbnoz,linoe,ncmpz,licmp,cnocmp,&
     &basez,prof,cnoz)
            integer :: ncmpz
            integer :: nbnoz
            character(*) :: maz
            character(*) :: nomgdz
            integer :: linoe(nbnoz)
            character(*) :: licmp(ncmpz)
            integer :: cnocmp(nbnoz*ncmpz)
            character(*) :: basez
            character(*) :: prof
            character(*) :: cnoz
          end subroutine cnocre
        end interface
