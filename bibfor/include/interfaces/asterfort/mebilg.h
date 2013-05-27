        interface
          subroutine mebilg(optioz,result,modele,depla1,depla2,theta,&
     &mate,nchar,lchar,symech,extim,timeu,timev,indi,indj,nbprup,noprup)
            character(len=16) :: optioz
            character(len=8) :: result
            character(len=8) :: modele
            character(len=24) :: depla1
            character(len=24) :: depla2
            character(len=24) :: theta
            character(len=24) :: mate
            integer :: nchar
            character(len=8) :: lchar(*)
            character(len=8) :: symech
            logical :: extim
            real(kind=8) :: timeu
            real(kind=8) :: timev
            integer :: indi
            integer :: indj
            integer :: nbprup
            character(len=16) :: noprup(*)
          end subroutine mebilg
        end interface
