        interface
          subroutine vdesga(nomte,kwgt,inte,intsn,nb1,nb2,xi,depl,&
     &btild,indith,alpha,tempga,epsiln,sigma,vectt)
            character(*) :: nomte
            integer :: kwgt
            integer :: inte
            integer :: intsn
            integer :: nb1
            integer :: nb2
            real(kind=8) :: xi(3,*)
            real(kind=8) :: depl(*)
            real(kind=8) :: btild(5,42)
            integer :: indith
            real(kind=8) :: alpha
            real(kind=8) :: tempga(*)
            real(kind=8) :: epsiln(6,*)
            real(kind=8) :: sigma(6,*)
            real(kind=8) :: vectt(3,3)
          end subroutine vdesga
        end interface
