        interface
          subroutine nmcpl1(compor,typmod,option,vimp,deps,optio2,cp,&
     &nvv)
            character(len=16) :: compor(*)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            real(kind=8) :: vimp(*)
            real(kind=8) :: deps(*)
            character(len=16) :: optio2
            integer :: cp
            integer :: nvv
          end subroutine nmcpl1
        end interface
