        interface
          subroutine pmactn(sddisc,parcri,iterat,numins,itemax,sderro,&
     &liccvg,actite,action)
            character(len=19) :: sddisc
            real(kind=8) :: parcri(*)
            integer :: iterat
            integer :: numins
            logical :: itemax
            character(len=24) :: sderro
            integer :: liccvg(5)
            integer :: actite
            integer :: action
          end subroutine pmactn
        end interface
