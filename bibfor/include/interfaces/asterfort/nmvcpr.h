        interface
          subroutine nmvcpr(modelz,numedd,mate,carele,comref,compor,&
     &valinc,cnvcpr)
            character(*) :: modelz
            character(len=24) :: numedd
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: comref
            character(len=24) :: compor
            character(len=19) :: valinc(*)
            character(len=24) :: cnvcpr
          end subroutine nmvcpr
        end interface
