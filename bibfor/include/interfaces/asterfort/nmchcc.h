        interface
          subroutine nmchcc(fonact,nbmatr,ltypma,loptme,loptma,lassme,&
     &lcalme)
            integer :: fonact(*)
            integer :: nbmatr
            character(len=6) :: ltypma(20)
            character(len=16) :: loptme(20)
            character(len=16) :: loptma(20)
            logical :: lassme(20)
            logical :: lcalme(20)
          end subroutine nmchcc
        end interface
