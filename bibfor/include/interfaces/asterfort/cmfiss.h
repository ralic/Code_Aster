        interface
          subroutine cmfiss(main,gno1,gno2,prefix,mainit,nomgma,noma,&
     &connec,tyma,ngma,gpma)
            character(len=8) :: main
            character(len=24) :: gno1
            character(len=24) :: gno2
            character(len=8) :: prefix
            integer :: mainit
            character(len=24) :: nomgma
            character(len=24) :: noma
            character(len=24) :: connec
            character(len=24) :: tyma
            character(len=24) :: ngma
            character(len=24) :: gpma
          end subroutine cmfiss
        end interface
