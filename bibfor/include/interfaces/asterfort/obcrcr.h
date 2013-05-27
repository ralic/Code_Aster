        interface
          subroutine obcrcr(nomstr,nbparb,nbpari,nbparr,nbpark,nbparo,&
     &parab,parai,parar,parak,parao,typeo)
            integer :: nbparo
            integer :: nbpark
            integer :: nbparr
            integer :: nbpari
            integer :: nbparb
            character(len=24) :: nomstr
            character(len=24) :: parab(nbparb)
            character(len=24) :: parai(nbpari)
            character(len=24) :: parar(nbparr)
            character(len=24) :: parak(nbpark)
            character(len=24) :: parao(nbparo)
            character(len=24) :: typeo(nbparo)
          end subroutine obcrcr
        end interface
