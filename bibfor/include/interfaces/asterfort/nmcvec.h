        interface
          subroutine nmcvec(oper,typvez,optioz,lcalc,lasse,nbvect,&
     &ltypve,loptve,lcalve,lassve)
            character(len=4) :: oper
            character(*) :: typvez
            character(*) :: optioz
            logical :: lcalc
            logical :: lasse
            integer :: nbvect
            character(len=6) :: ltypve(20)
            character(len=16) :: loptve(20)
            logical :: lcalve(20)
            logical :: lassve(20)
          end subroutine nmcvec
        end interface
