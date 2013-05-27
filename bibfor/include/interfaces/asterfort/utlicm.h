        interface
          subroutine utlicm(nbcmpv,nomcmp,nomgd,ncmprf,nomcmr,ncmpve,&
     &numcmp,ntncmp,ntucmp)
            integer :: nbcmpv
            character(*) :: nomcmp(*)
            character(len=8) :: nomgd
            integer :: ncmprf
            character(*) :: nomcmr(*)
            integer :: ncmpve
            character(*) :: numcmp
            character(*) :: ntncmp
            character(*) :: ntucmp
          end subroutine utlicm
        end interface
