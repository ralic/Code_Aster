        interface
          subroutine nmcerr(sddisc,iter1,iter2,elasdt,rgmaxi,rgrela,&
     &inikry,lctcd,defico)
            character(len=19) :: sddisc
            integer :: iter1
            integer :: iter2
            real(kind=8) :: elasdt
            real(kind=8) :: rgmaxi
            real(kind=8) :: rgrela
            real(kind=8) :: inikry
            logical :: lctcd
            character(len=24) :: defico
          end subroutine nmcerr
        end interface
