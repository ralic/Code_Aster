        interface
          subroutine nmcmat(oper,typmaz,optcaz,optasz,lcalc,lasse,&
     &nbmatr,ltypma,loptme,loptma,lcalme,lassme)
            character(len=4) :: oper
            character(*) :: typmaz
            character(*) :: optcaz
            character(*) :: optasz
            logical :: lcalc
            logical :: lasse
            integer :: nbmatr
            character(len=6) :: ltypma(20)
            character(len=16) :: loptme(20)
            character(len=16) :: loptma(20)
            logical :: lcalme(20)
            logical :: lassme(20)
          end subroutine nmcmat
        end interface
