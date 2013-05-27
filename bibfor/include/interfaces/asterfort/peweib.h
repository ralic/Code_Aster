        interface
          subroutine peweib(resu,modele,mate,cara,chmat,nchar,lchar,nh&
     &,nbocc,iresu,nomcmd)
            character(*) :: resu
            character(*) :: modele
            character(*) :: mate
            character(*) :: cara
            character(len=8) :: chmat
            integer :: nchar
            character(*) :: lchar(*)
            integer :: nh
            integer :: nbocc
            integer :: iresu
            character(*) :: nomcmd
          end subroutine peweib
        end interface
