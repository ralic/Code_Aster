        interface
          subroutine calir3(mo,nbma1,lima1,nbno2,lino2,geom2,corre1,&
     &corre2,jlisv1,iocc)
            integer :: nbno2
            integer :: nbma1
            character(len=8) :: mo
            integer :: lima1(nbma1)
            integer :: lino2(nbno2)
            character(len=24) :: geom2
            character(len=16) :: corre1
            character(len=16) :: corre2
            integer :: jlisv1
            integer :: iocc
          end subroutine calir3
        end interface
