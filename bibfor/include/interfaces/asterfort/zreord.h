        interface
          subroutine zreord(zmat,nbddg,nbmod,nbmob,nbddr,axok,liax,&
     &nbliax,zvec)
            integer :: nbliax
            integer :: nbmod
            integer :: nbddg
            complex(kind=8) :: zmat(nbddg,nbmod)
            integer :: nbmob
            integer :: nbddr
            logical :: axok
            integer :: liax(nbliax)
            complex(kind=8) :: zvec(nbddg)
          end subroutine zreord
        end interface
