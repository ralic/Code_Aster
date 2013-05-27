        interface
          subroutine vefnme(modele,sigma,caraz,depmoi,depdel,vecelz,&
     &matcod,compor,nh,fnoevo,partps,carcri,chvarc,ligrez,option,strx)
            character(*) :: modele
            character(*) :: sigma
            character(*) :: caraz
            character(*) :: depmoi
            character(*) :: depdel
            character(*) :: vecelz
            character(*) :: matcod
            character(*) :: compor
            integer :: nh
            logical :: fnoevo
            real(kind=8) :: partps(*)
            character(*) :: carcri
            character(*) :: chvarc
            character(*) :: ligrez
            character(len=16) :: option
            character(*) :: strx
          end subroutine vefnme
        end interface
