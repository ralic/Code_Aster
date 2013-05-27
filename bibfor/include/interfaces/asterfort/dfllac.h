        interface
          subroutine dfllac(mcfact,iechec,dtmin,even,action,submet,&
     &subaut,pasmin,nbrpas,niveau,pcplus,cmmaxi,prcoll,ducoll)
            character(len=16) :: mcfact
            integer :: iechec
            real(kind=8) :: dtmin
            character(len=16) :: even
            character(len=16) :: action
            character(len=16) :: submet
            character(len=16) :: subaut
            real(kind=8) :: pasmin
            integer :: nbrpas
            integer :: niveau
            real(kind=8) :: pcplus
            real(kind=8) :: cmmaxi
            real(kind=8) :: prcoll
            real(kind=8) :: ducoll
          end subroutine dfllac
        end interface
