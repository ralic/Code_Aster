        interface
          subroutine nxlect(result,modele,mate,carele,matcst,coecst,&
     &fomult,lischa,charge,infoch,parmei,parmer,solveu,parcri,parcrr,&
     &compor,evolsc)
            character(len=24) :: result
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            logical :: matcst
            logical :: coecst
            character(len=24) :: fomult
            character(len=19) :: lischa
            character(len=24) :: charge
            character(len=24) :: infoch
            integer :: parmei(2)
            real(kind=8) :: parmer(2)
            character(len=19) :: solveu
            integer :: parcri(3)
            real(kind=8) :: parcrr(2)
            character(len=24) :: compor
            character(len=8) :: evolsc
          end subroutine nxlect
        end interface
